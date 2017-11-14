module munstead.pe.resources;

import munstead.ast.base;
import munstead.core.byteorder;
import munstead.core.exception;
import munstead.core.interval;
import munstead.core.mmap;
import munstead.core.util;
import munstead.core.wordtypes;
import munstead.pe.files;
import munstead.pe.sections;
import std.array: array;
import std.conv: to;
import std.datetime: SysTime;
import std.range: retro;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// PE Resource Section
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class PeResourceSection(size_t nBits): PeSection!nBits {
  mixin AstNodeFeatures;
  struct AstChildren {
    PeResourceDirectoryTable!nBits root; // root of resource tree
  }

  void parse(ParseLocation parentLoc) {
    auto fhdr = ancestor!(PeFileHeader!nBits);
    assert(fhdr !is null);
    assert(fhdr.memBytes !is null);

    auto loc = sectionLocation(fhdr.formatName ~ " resource section", 0, parentLoc);
    root = PeResourceDirectoryTable!nBits.instance();
    root.parse(loc, 0);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// PE Resource directory table
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class PeResourceDirectoryTable(size_t nBits): Ast {
  mixin AstNodeFeatures;
  struct AstChildren {
    AstList!(PeResourceDirectoryEntry!nBits) entries;
  }

  struct Disk {
    Word!32 reserved_0;		// must be zero. Named "characteristics" in the PE documentation
    Word!32 timeStamp;		// time that resource was created by the resource compiler
    Word!16 majorVersion;	// major version set by user
    Word!16 minorVersion;	// minor version set by user
    Word!16 nNameEntries;	// number of entries that use strings to identify type
    Word!16 nIdEntries;		// number of entries following the name entries
  }

  static size_t maxEntryErrors = 25; // max number of errors while parsing entries before giving up on table
  Disk disk;
  SysTime timeStamp;		// more useful API for disk.timeStamp
  Interval!(Word!nBits) preferredExtent; // where this table appears in memory (header only, no entries)

  this() {
    entries = new AstList!(PeResourceDirectoryEntry!nBits);
    timeStamp = SysTime.fromUnixTime(0); // init to prevent segfault in Phobos 2.076.1 and possibly others
  }

  void parse(ParseLocation parentLoc, Word!nBits offsetInSection) {
    auto section = ancestor!(PeResourceSection!nBits);
    assert(section !is null);
    assert(section.bytes !is null);
    auto fhdr = section.ancestor!(PeFileHeader!nBits);
    assert(fhdr !is null);

    auto tableLoc = section.sectionLocation(fhdr.formatName ~ " resource directory table", offsetInSection, parentLoc);

    // Address of the table header "disk" in virtual memory (excludes entries, which have their own extents)
    preferredExtent = Interval!(Word!nBits).baseSize(0, disk.sizeof)
      .shiftRightClip(section.preferredExtent.least)
      .shiftRightClip(offsetInSection)
      .intersect(section.preferredExtent);

    if (!section.bytes.readObjectAt(offsetInSection, disk))
      appendError(tableLoc, "short read");
    disk = disk.fromLittleEndian;
    timeStamp = SysTime.fromUnixTime(disk.timeStamp);

    // Parse name entries followed by id entries
    size_t entrySize = PeResourceDirectoryEntry!nBits.Disk.sizeof;
    size_t startingErrorCount = totalErrorCount;
    foreach (index; 0 .. (disk.nNameEntries + disk.nIdEntries)) {
      auto entryLoc = indexParseLocation(fhdr.formatName ~ " resource directory entry", index, tableLoc);
      entries.pushBack(PeResourceDirectoryEntry!nBits.instance());
      entries.back.isNameEntry = index < disk.nNameEntries;
      entries.back.parse(entryLoc, cast(Word!nBits)(offsetInSection + disk.sizeof + index * entrySize));

      // Entries must be sorted
      if (entries.length >= 2) {
	auto prevEntry = entries[entries.length-2];
	if (entries.back.isNameEntry && prevEntry.name >= entries.back.name) {
	  appendError(entryLoc, "entry name (" ~ entries.back.name.cEscape ~ ")" ~
		      " is not lexicographically greater than previous entry name (" ~ prevEntry.name.cEscape ~ ")");
	} else if (prevEntry.index >= disk.nNameEntries && prevEntry.id >= entries.back.id) {
	  appendError(entryLoc, "entry ID (" ~ entries.back.id.to!string ~ ")" ~
		      " is not greater than previous entry ID (" ~ prevEntry.id.to!string ~ ")");
	}
      }

      // Bail if too many errors
      if (totalErrorCount > startingErrorCount && totalErrorCount - startingErrorCount > maxEntryErrors) {
	appendError(tableLoc, "number of entry errors (" ~ (totalErrorCount-startingErrorCount).to!string ~ ")" ~
		    " exceeds allowed limit for this table (" ~ maxEntryErrors.to!string ~ ")");
	break;
      }
    }
  }
}

class PeResourceDirectoryEntry(size_t nBits): Ast {
  mixin AstNodeFeatures;
  struct AstChildren {
    PeResourceDirectoryTable!nBits subdir; // at most one of subdir or leaf is non-null
    PeResourceDataEntry!nBits leaf;
  }

  struct Disk {
    Word!32 nameOffsetOrId;	// entry ID or offset within resource section for name of this entry
    Word!32 childOffset;	// offset in resource section for another directory table or leaf data
  }

  bool isNameEntry = false;     // should be set before calling parse()
  uint id;			// id if isNameEntry is false
  string name;			// entry name if isNameEntry is true
  size_t index;			// index of this entry in the parent directory
  Interval!(Word!nBits) preferredExtent; // address of this entry in virtual memory
  Disk disk;

  void parse(ParseLocation parentLoc, Word!nBits offsetInSection) {
    auto dir = ancestor!(PeResourceDirectoryTable!nBits);
    assert(dir !is null);
    auto section = dir.ancestor!(PeResourceSection!nBits);
    assert(section !is null);
    assert(section.bytes !is null);
    auto fhdr = section.ancestor!(PeFileHeader!nBits);
    assert(fhdr !is null);

    assert(dir.entries.length > 0);
    assert(dir.entries.back is this);
    index = dir.entries.length - 1;

    auto loc = section.sectionLocation(fhdr.formatName ~ " resource directory entry", offsetInSection, parentLoc);

    preferredExtent = Interval!(Word!nBits).baseSize(0, disk.sizeof)
      .shiftRightClip(section.preferredExtent.least)
      .shiftRightClip(offsetInSection)
      .intersect(section.preferredExtent);

    if (!section.bytes.readObjectAt(offsetInSection, disk))
      appendError(loc, "short read");
    disk = disk.fromLittleEndian;

    // Parse the entry name or ID
    if (isNameEntry) {
      // 2-byte length followed by Unicode string data, 2-byte aligned
      auto nameLengthOffsetInSection = disk.nameOffsetOrId;
      auto nameLengthLoc = section.sectionLocation(fhdr.formatName ~ " resource entry name", nameLengthOffsetInSection, loc);
      if (nameLengthOffsetInSection % 2 != 0)
	appendError(nameLengthLoc, "not 2-byte aligned");
      
      Word!16 nameLength;
      if (!section.bytes.readObjectAt(nameLengthOffsetInSection, nameLength))
	appendError(nameLengthLoc, "short read for resource entry name length field");
      nameLength = nameLength.fromLittleEndian;

      auto nameOffsetInSection = nameLengthOffsetInSection + 2;
      auto nameLoc = section.sectionLocation(fhdr.formatName ~ " resource entry name", nameOffsetInSection, loc);
      auto nameBuf = section.bytes.segmentsAt(nameOffsetInSection).byBuffer(nameLength).frontOrDflt;
      if (nameBuf.interval.length < nameLength)
	appendError(nameLoc, "short read (" ~ nameBuf.interval.length.plural("bytes") ~ ")" ~
		    " for resource entry name (expected " ~ nameLength.plural("bytes") ~ ")");
      name = (cast(char[]) nameBuf.buffer).idup;
		    
    } else {
      id = disk.nameOffsetOrId;
    }

    // Parse the child, either a resource data entry (leaf node) or another directory table (interior node)
    if ((disk.childOffset & 0x80000000uL) == 0) {
      leaf = PeResourceDataEntry!nBits.instance();
      leaf.parse(loc, disk.childOffset);
    } else {
      subdir = PeResourceDirectoryTable!nBits.instance();
      subdir.parse(loc, disk.childOffset & 0x7fffffff);
    }
  }

  string entryName() {
    return isNameEntry ? name : id.to!string;
  }
    
  string absoluteName() {
    string retval;
    foreach (entry; this.parents.nodeType!(PeResourceDirectoryEntry!nBits).array.retro)
      retval ~= "/" ~ entry.entryName;
    return retval ~ "/" ~ entryName;
  }
}

class PeResourceDataEntry(size_t nBits): Ast {
  mixin AstNodeFeatures;
  struct AstChildren {}

  struct Disk {
    Word!32 dataRva;		// "address of a unit of resource data in the resource data area"
    Word!32 size;		// size in bytes of the pointed-to resource data
    Word!32 codePage;		// typicaly the unicode code page
    Word!32 reserved_12;	// must be zero
  }

  Disk disk;
  Interval!(Word!nBits) preferredExtent; // location of this resource data entry in virtual memory (not the data itself)
  Interval!(Word!nBits) dataPreferredExtent; // location of the data in virtual memory

  MemoryMap!(Word!nBits) data;	// the data for this resource

  void parse(ParseLocation parentLoc, Word!nBits offsetInSection) {
    auto section = ancestor!(PeResourceSection!nBits);
    assert(section !is null);
    assert(section.bytes !is null);
    auto fhdr = section.ancestor!(PeFileHeader!nBits);
    assert(fhdr !is null);
    assert(fhdr.memBytes !is null);

    auto loc = section.sectionLocation(fhdr.formatName ~ " resource data entry", offsetInSection, parentLoc);

    preferredExtent = Interval!(Word!nBits).baseSize(0, disk.sizeof)
      .shiftRightClip(section.preferredExtent.least)
      .shiftRightClip(offsetInSection)
      .intersect(section.preferredExtent);

    if (!section.bytes.readObjectAt(offsetInSection, disk))
      appendError(loc, "short read");
    disk = disk.fromLittleEndian;

    dataPreferredExtent = Interval!(Word!nBits).baseSizeTrunc(fhdr.va(disk.dataRva), disk.size);
    data = new MemoryMap!(Word!nBits);
    foreach (node; fhdr.memBytes.segmentsWithin(dataPreferredExtent.intersect(section.preferredExtent)))
      data.insert(node.interval.shiftLeft(dataPreferredExtent.least), node.segment);

    if (data.hull.length != disk.size)
      appendError(loc, "short read resource data at va " ~ dataPreferredExtent.hexStr ~
		  " within resource section at va " ~ section.preferredExtent.hexStr);
  }
}
  
