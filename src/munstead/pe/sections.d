module munstead.pe.sections;

import munstead.ast.base;
import munstead.ast.sections;
import munstead.core.bitflags;
import munstead.core.byteorder;
import munstead.core.exception;
import munstead.core.interval;
import munstead.core.mmap;
import munstead.core.util;
import munstead.core.wordtypes;
import munstead.pe.files;
import std.algorithm: filter, fold, map, min;
import std.array: array;
import std.conv: to;
import std.range: enumerate, take;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// A section of a file or memory
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class PeSection(size_t nBits): AsmSection!nBits {
  mixin AstNodeFeatures;
  struct AstChildren {}

  // Create content for a section by taking it from a header directory entry (RVA/size pair)
  void createContent(PeDataDirectoryEntry!nBits ddent) {
    auto fhdr = ancestor!(PeFileHeader!nBits);
    assert(fhdr !is null);
    assert(fhdr.memBytes !is null);

    if (ddent.disk.nBytes > 0)
      preferredExtent = ddent.vaExtent;
    createSectionBytes!(MemoryMap!(Word!nBits))(fhdr.memBytes, preferredExtent, AddressSpace.MEMORY);

    // Create a comment to describe which data directory entry this came from
    foreach (ch; (cast(PeDataDirectory!nBits.DataDirType) ddent.index).to!string) {
      import core.stdc.ctype;
      if ('_' == ch) {
        comment ~= " ";
      } else {
        comment ~= tolower(ch);
      }
    }
    comment ~= " optional hdr data directory entry " ~ ddent.index.to!string;
  }

  // Create content for a section by using info from the PE section table
  void createContent(PeSectionTableEntry!nBits shent) {
    assert(shent !is null);
    auto fhdr = ancestor!(PeFileHeader!nBits);
    assert(fhdr !is null);
    assert(fhdr.fileBytes !is null);

    fileExtent = shent.sectionFileExtent;
    createSectionBytes!(MemoryMap!(Word!nBits))(fhdr.fileBytes, fileExtent, AddressSpace.FILE);
    preferredExtent = shent.sectionPreferredExtent;
    name = shent.name;
  }

  // Printable name of section
  override string printableName() {
    string[] parts;

    if (auto fhdr = this.ancestor!(PeFileHeader!nBits)) {
      if (fhdr.sectionTable !is null)
        parts ~= fhdr.sectionTable.indexesOf(this).take(1).map!(i => "section " ~ to!string(i)).array;
    }

    if (name != "")
      parts ~= name.cEscape;

    if (comment != "")
      parts ~= comment;

    string ret;
    for (size_t i=0; i<parts.length; ++i)
      ret ~= (i > 0 ? ", " : "") ~ parts[i];
    if (ret == "")
      ret = "no name, no table";
    return ret;
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// A table describing sections in a file
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class PeSectionTable(size_t nBits): Ast {
  mixin AstNodeFeatures;
  struct AstChildren {
    AstList!(PeSectionTableEntry!nBits) entries;
  }

  static size_t maxEntryErrors = 25; // number of errors when parsing entries before giving up on this table
  Interval!(Word!nBits) fileExtent; // location of section table within file
  
  this() {
    entries = new AstList!(PeSectionTableEntry!nBits);
  }

  void parse(ParseLocation parentLoc) {
    auto fhdr = ancestor!(PeFileHeader!nBits);
    assert(fhdr !is null);

    // Section table immediately follows whatever headers were parsed.
    Word!nBits firstEntryOffset;
    if (fhdr.dataDirectory !is null && !fhdr.dataDirectory.fileExtent.empty) {
      firstEntryOffset = cast(Word!nBits)(fhdr.dataDirectory.fileExtent.greatest + 1);
    } else if (fhdr.optionalHeader !is null && !fhdr.optionalHeader.fileExtent.empty) {
      firstEntryOffset = cast(Word!nBits)(fhdr.optionalHeader.fileExtent.greatest + 1);
    } else if (fhdr.coffHeader !is null && !fhdr.coffHeader.fileExtent.empty) {
      firstEntryOffset = cast(Word!nBits)(fhdr.coffHeader.fileExtent.greatest + 1);
    }

    auto tableLoc = fhdr.fileLocation(fhdr.formatName ~ " section table", firstEntryOffset, parentLoc);
    size_t nEntries = fhdr.coffHeader.disk.numberOfSections;
    fileExtent = Interval!(Word!nBits).baseSizeTrunc(firstEntryOffset, nEntries * PeSectionTableEntry!nBits.Disk.sizeof);

    // Section table cannot be larger than entire file
    size_t availBytes = fhdr.fileBytes.segmentsWithin(fileExtent).contiguous.map!"a.interval.length".fold!"a+b"(0UL);
    size_t maxEntries = availBytes / PeSectionTableEntry!nBits.Disk.sizeof;
    if (nEntries > maxEntries) {
      appendError(tableLoc, "header-specified table size (" ~ nEntries.plural("entries") ~ ")" ~
                  " is larger than available size (" ~ maxEntries.plural("entries") ~ ")");
    }

    // Parse entries, but limit total error count
    size_t startingErrorCount = totalErrorCount;
    foreach (idx; 0 .. min(nEntries, maxEntries+1)) {
      auto entryLoc = indexParseLocation(fhdr.formatName ~ " section table entry", idx, tableLoc);
      auto entry = PeSectionTableEntry!nBits.instance();
      entries.pushBack(entry); // always link into the AST before parsing
      entry.parse(entryLoc);
      if (totalErrorCount > startingErrorCount && totalErrorCount - startingErrorCount > maxEntryErrors) {
        appendError(tableLoc, "number of entry parsing errors (" ~ (totalErrorCount-startingErrorCount).to!string ~ ")" ~
                    " exceeds allowed limit for this table (" ~ maxEntryErrors.to!string ~ ")");
        break;
      }
    }
  }

  // Returns the indexes of all the section table entries that point to the specified section. The return value is an
  // input range.
  auto indexesOf(PeSection!nBits section) {
    return entries[]
      .enumerate
      .filter!(a => a.value.section is section)
      .map!"a.index";
  }

  // Find a particular section index, which must exist or an error is noted.
  size_t indexOf(PeSection!nBits section, ParseLocation loc) {
    auto found = indexesOf(section).take(1);
    if (!found.empty)
      return found.front;
    if (loc !is null)
      appendError(loc, "section not found in section table");
    assert(0, "section not found");
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// One entry in a section table
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class PeSectionTableEntry(size_t nBits): Ast {
  mixin AstNodeFeatures;
  struct AstChildren {}

  // Microsoft prefixes all these with "IMAGE_SCN_"
  enum Characteristics: Word!32 {
    // reserved              0x00000001
    // reserved              0x00000002
    // reserved              0x00000004
    TYPE_NO_PAD            = 0x00000008, // obsolete
    // reserved              0x00000010
    CNT_CODE               = 0x00000020, // contains executable code
    CNT_INITIALIZED_DATA   = 0x00000040, // contains initialized data
    CNT_UNINITIALIZED_DATA = 0x00000080, // contains uninitialized data
    LNK_OTHER              = 0x00000100, // reserved
    LNK_INFO               = 0x00000200, // contains comments or other information
    // reserved              0x00000400
    LNK_REMOVE             = 0x00000800, // will not become part of the image
    LNK_COMDAT             = 0x00001000, // contains COMDAT data
    // undocumented          0x00002000
    // undocumented          0x00004000
    GPREL                  = 0x00008000, // contains data referenced through the global pointer (GP)
    MEM_PURGEABLE          = 0x00020000, // reserved (seems to be a typo: should it be 0x00010000?
    MEM_16BIT              = 0x00020000, // reserved
    MEM_LOCKED             = 0x00040000, // reserved
    MEM_PRELOAD            = 0x00080000, // reserved
      
    // This group is not actually bit flags, but rather a field encoded in 8 bits
    ALIGN_1BYTES           = 0x00100000, // align data on a 1-byte boundary
    ALIGN_2BYTES           = 0x00200000, // align data on a 2-byte boundary
    ALIGN_4BYTES           = 0x00300000, // align data on a 4-byte boundary
    ALIGN_8BYTES           = 0x00400000, // align data on an 8-byte boundary
    ALIGN_16BYTES          = 0x00500000, // align data on a 16-byte boundary
    ALIGN_32BYTES          = 0x00600000, // align data on a 32-byte boundary
    ALIGN_64BYTES          = 0x00700000, // align data on a 64-byte boundary
    ALIGN_128BYTES         = 0x00800000, // align data on a 128-byte boundary
    ALIGN_256BYTES         = 0x00900000, // align data on a 256-byte boundary
    ALIGN_512BYTES         = 0x00a00000, // align data on a 512-byte boundary
    ALIGN_1024BYTES        = 0x00b00000, // align data on a 1024-byte boundary
    ALIGN_2048BYTES        = 0x00c00000, // align data on a 2048-byte boundary
    ALIGN_4096BYTES        = 0x00d00000, // align data on a 4096-byte boundary
    ALIGN_8192BYTES        = 0x00e00000, // align data on an 8192-byte boundary
    // possibly omitted      0x00f00000  // undocumented

    LNK_NRELOC_OVFL        = 0x01000000, // contains extended relocations
    MEM_DISCARDABLE        = 0x02000000, // can be discarded as needed
    MEM_NOT_CACHED         = 0x04000000, // cannot be cached
    MEM_NOT_PAGED          = 0x08000000, // not pageable
    MEM_SHARED             = 0x10000000, // can be shared in memory
    MEM_EXECUTE            = 0x20000000, // can be executed as code
    MEM_READ               = 0x40000000, // can be read
    MEM_WRITE              = 0x80000000  // can be written to
  }

  struct Disk {
    align(1):
    ubyte[8] name;
    Word!32 virtualSize;        // size of section in memory with zero padding
    Word!32 rva;                // where first byte of section is mapped
    Word!32 sizeOfRawData;      // bytes
    Word!32 pointerToRawData;   // file offset
    Word!32 pointerToRelocations; // file offset
    Word!32 pointerToLineNumbers; // file offset
    Word!16 numberOfRelocations;
    Word!16 numberOfLineNumbers;
    BitFlags!Characteristics characteristics;
  }

  Interval!(Word!nBits) entryFileExtent; // location of section table entry within file
  Disk disk;
  string name;
  size_t index;                 // position of this entry in the section table
  PeSection!nBits section;      // associated section; AST child of the file header

  void parse(ParseLocation parentLoc) {
    auto fhdr = ancestor!(PeFileHeader!nBits);
    assert(fhdr !is null);
    auto table = ancestor!(PeSectionTable!nBits);
    assert(table !is null);

    index = table.entries.length - 1;
    assert(table.entries[index] is this);
    auto startOffset = cast(Word!nBits)(table.fileExtent.least + index * Disk.sizeof);
    entryFileExtent = Interval!(Word!nBits).baseSizeTrunc(startOffset, Disk.sizeof);

    auto loc = fhdr.fileLocation(fhdr.formatName ~ " section table entry", startOffset, parentLoc);

    if (!fhdr.fileBytes.readObjectAt(startOffset, disk))
      appendError(loc, "short read");
    
    disk = disk.fromLittleEndian;
    foreach (ch; disk.name) {
      if (ch == 0)
        break;
      name ~= ch;
    }

    // Adjust the location indicator now that we have a name.
    loc = fhdr.fileLocation(fhdr.formatName ~ " section table entry for for " ~ name.cEscape, startOffset, parentLoc);

    if (disk.sizeOfRawData % fhdr.optionalHeader.disk.fileAlignment != 0)
      appendError(loc, "size of raw data (" ~ disk.sizeOfRawData.hexStr ~ ")" ~
                  " is not a multiple of the file alignment" ~
                  " (" ~ fhdr.optionalHeader.disk.fileAlignment.hexStr ~ ")");

    bool hasRelocOverflowBit = disk.characteristics.isSet(Characteristics.LNK_NRELOC_OVFL);
    bool hasRelocOverflowPattern = disk.numberOfRelocations == 0xffff;
    if (hasRelocOverflowBit && !hasRelocOverflowPattern) {
      appendError(loc, "has 0xffff relocations but the overflow bit (IMAGE_SCN_LINK_NRELOC_OVFL) is clear");
    } else if (!hasRelocOverflowBit && hasRelocOverflowPattern) {
      appendError(loc, "relocations overflow bit (IMAGE_SCN_LINK_NRELOC_OVFL) is set but number of relocations field " ~
                  " has non-0xffff value (" ~ disk.numberOfRelocations.to!string ~ ")");
    }

    if (!fhdr.fileBytes.hull.contains(sectionFileExtent())) {
      appendError(loc, "file offsets (" ~ sectionFileExtent.hexStr ~ ")" ~
                  " extend outside file domain (" ~ fhdr.fileBytes.hull.hexStr ~ ")");
    }

    if (disk.characteristics.nonFlagsSet != 0)
      appendError(loc, "characterstics (" ~ disk.characteristics.to!string ~ ") has unrecognized bits");
  }

  Interval!(Word!nBits) sectionFileExtent() @property {
    if (disk.sizeOfRawData > 0) {
      return Interval!(Word!nBits).baseSizeTrunc(disk.pointerToRawData, disk.sizeOfRawData);
    } else {
      return Interval!(Word!nBits)();
    }
  }

  Interval!(Word!nBits) sectionPreferredExtent() @property {
    if (disk.virtualSize > 0) {
      auto fhdr = this.ancestor!(PeFileHeader!nBits);
      assert(fhdr !is null);
      return Interval!(Word!nBits).baseSizeTrunc(disk.rva + fhdr.optionalHeader.disk.imageBase, disk.virtualSize);
    } else {
      return Interval!(Word!nBits)();
    }
  }
}
                                         
