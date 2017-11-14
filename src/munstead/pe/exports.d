module munstead.pe.exports;

import munstead.ast.base;
import munstead.core.byteorder;
import munstead.core.exception;
import munstead.core.interval;
import munstead.core.mmap;
import munstead.core.util;
import munstead.core.wordtypes;
import munstead.pe.files;
import munstead.pe.sections;
import std.algorithm: fold, map, min;
import std.conv: to;
import std.datetime.systime: SysTime;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Export section
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class PeExportSection(size_t nBits): PeSection!nBits {
  mixin AstNodeFeatures;
  struct AstChildren {
    PeExportDirectory!nBits directory;
  }

  void parse(ParseLocation parentLoc) {
    auto fhdr = ancestor!(PeFileHeader!nBits);
    assert(fhdr !is null);
    auto loc = sectionLocation(fhdr.formatName ~ " export section", 0, parentLoc);
    directory = PeExportDirectory!nBits.instance();
    directory.parse(loc);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Export directory
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class PeExportDirectory(size_t nBits): Ast {
  mixin AstNodeFeatures;
  struct AstChildren {
    PeExportAddressTable!nBits addressTable;
    PeExportNameTable!nBits nameTable;
    PeExportOrdinalTable!nBits ordinalTable;
  }

  struct Disk {
    align(1):
    Word!32 reserved_0;         // named "export flags" in the documentation, but also stead "must be zero"
    Word!32 timeStamp;          // time that the export data was created
    Word!16 majorVersion;       // user-settable version number
    Word!16 minorVersion;
    Word!32 nameRva;            // relative virtual address of ASCII string containing name of this DLL
    Word!32 ordinalBase;        // first ordinal for this library, usually 1
    Word!32 nAddressTableEntries; // number of entries in the export address table
    Word!32 nNamePointers;      // number of entries in the name pointer table and the ordinal table
    Word!32 addressTableRva;    // relative virtaul address of the export address table
    Word!32 namePointersRva;    // relative virtual address for the list of name addresses
    Word!32 ordinalTableRva;    // relative virtual address for the ordinal table
  }

  Disk disk;
  Interval!(Word!nBits) preferredExtent; // Virtual address for this export directory
  SysTime timeStamp;
  string dllName;

  this() {
    // Initialize the time stamp to avoid segfaults from the Phobos library (occurs with version 2.076.1 and possibly others).
    timeStamp = SysTime.fromUnixTime(0);
  }

  void parse(ParseLocation parentLoc) {
    auto section = ancestor!(PeExportSection!nBits);
    assert(section !is null);

    auto fhdr = section.ancestor!(PeFileHeader!nBits);
    assert(fhdr !is null);

    preferredExtent = Interval!(Word!nBits).baseSizeTrunc(section.preferredExtent.least, Disk.sizeof);
    auto loc = fhdr.vaLocation(fhdr.formatName ~ " export directory", preferredExtent.least, parentLoc);

    if (!fhdr.memBytes.readObjectAt(preferredExtent.least, disk))
      appendError(loc, "short read");
    disk = disk.fromLittleEndian;
    timeStamp = SysTime.fromUnixTime(disk.timeStamp);

    if (disk.reserved_0 != 0)
      appendError(loc, "export directory reserved field \"export flags\" (" ~ disk.reserved_0.hexStr ~ ") is not zero");

    dllName = fhdr.memBytes.stringAt(fhdr.va(disk.nameRva), this, loc);

    addressTable = PeExportAddressTable!nBits.instance();
    addressTable.parse(loc);

    if (disk.nNamePointers > 0) {
      nameTable = PeExportNameTable!nBits.instance();
      nameTable.parse(loc);

      ordinalTable = PeExportOrdinalTable!nBits.instance();
      ordinalTable.parse(loc);

      if (nameTable.entries.length != ordinalTable.entries.length)
        appendError(loc, "name table and ordinal table are unequal lengths");
    }
  }
}
                                       
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Export address table
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class PeExportAddressTable(size_t nBits): Ast {
  mixin AstNodeFeatures;
  struct AstChildren {
    AstList!(PeExportAddressEntry!nBits) entries; // indexed by ordinal
  }

  static size_t maxEntryErrors = 25;    // max number of errors caused by entries before giving up
  Interval!(Word!nBits) preferredExtent; // location of this table in virtual memory

  this() {
    entries = new AstList!(PeExportAddressEntry!nBits);
  }

  void parse(ParseLocation parentLoc) {
    auto edir = ancestor!(PeExportDirectory!nBits);
    assert(edir !is null);
    auto fhdr = edir.ancestor!(PeFileHeader!nBits);
    assert(fhdr !is null);
    auto tableVa = fhdr.va(edir.disk.addressTableRva);
    auto tableNBytes = edir.disk.nAddressTableEntries * PeExportAddressEntry!nBits.Disk.sizeof;
    preferredExtent = Interval!(Word!nBits).baseSizeTrunc(tableVa, tableNBytes);
    auto tableLoc = fhdr.vaLocation(fhdr.formatName ~ " export address table", preferredExtent.least, parentLoc);

    // The size of the address table (tableNBytes) must not exceed the amount of memory contiguously mapped at that location
    size_t availBytes = fhdr.memBytes.segmentsWithin(preferredExtent).contiguous.map!"a.interval.length".fold!"a+b"(0UL);
    size_t maxEntries = availBytes / PeExportAddressEntry!nBits.Disk.sizeof;
    if (tableNBytes > availBytes) {
      appendError(tableLoc, "directory-specified table size (" ~ edir.disk.nAddressTableEntries.plural("entries") ~ ", " ~
                  tableNBytes.plural("bytes") ~ ") is larger than available size (" ~ maxEntries.plural("entries") ~ ", " ~
                  availBytes.plural("bytes") ~ ")");
    }

    // The number of entries in the address table cannot be larger than 2^16 since it's indexed by ordinal (minus
    // ordinalBase) and the ordinal table stores 16-bit values.
    if (edir.disk.nAddressTableEntries > 2^^16) {
      appendError(tableLoc, "directory-specified table size (" ~ edir.disk.nAddressTableEntries.plural("entries") ~ ")" ~
                  " is larger than maximum possible number of distinct ordinals (65536, 2^16)");
      maxEntries = min(maxEntries, 2^^16);
    }

    // Parse each entry
    size_t startingErrorCount = totalErrorCount;
    foreach (i; 0 .. min(edir.disk.nAddressTableEntries, maxEntries+1)) {
      auto entryLoc = indexParseLocation(fhdr.formatName ~ " export address table entry", i, tableLoc);
      entries.pushBack(PeExportAddressEntry!nBits.instance());
      entries.back.parse(entryLoc);
      if (totalErrorCount > startingErrorCount && totalErrorCount - startingErrorCount > maxEntryErrors) {
        appendError(tableLoc, "number of entry parsing errors (" ~ (totalErrorCount-startingErrorCount).to!string ~ ")" ~
                    " exceeds allowed limit for this table (" ~ maxEntryErrors.to!string ~ ")");
        break;
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Export address entry
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class PeExportAddressEntry(size_t nBits): Ast {
  mixin AstNodeFeatures;
  struct AstChildren {}

  struct Disk {
    Word!32 value; // either an export RVA or a forwarder RVA
  }

  Disk disk;
  size_t index; // index in the parent node
  string forwarder; // format is "<DLLNAME>.<FUNCNAME>" or "<DLLNAME>.#<ORDINAL>"
  Interval!(Word!nBits) preferredExtent; // location of this entry in virtual memory
  
  void parse(ParseLocation parentLoc) {
    auto table = ancestor!(PeExportAddressTable!nBits);
    assert(table !is null);
    assert(table.entries.length > 0);
    assert(table.entries.back is this);
    auto fhdr = table.ancestor!(PeFileHeader!nBits);
    assert(fhdr !is null);

    index = table.entries.length - 1;
    auto va = cast(Word!nBits)(table.preferredExtent.least + index * disk.sizeof);
    preferredExtent = Interval!(Word!nBits).baseSizeTrunc(va, disk.sizeof);
    auto loc = fhdr.vaLocation(fhdr.formatName ~ " export address table entry", va, parentLoc);

    if (!fhdr.memBytes.readObjectAt(va, disk))
      appendError(loc, "short read");
    disk = disk.fromLittleEndian;

    if (isForwarder) {
      auto forwardVa = fhdr.va(disk.value);
      auto forwardLoc = fhdr.vaLocation(fhdr.formatName ~ " export forwarder string", forwardVa, loc);
      auto maxBytes = fhdr.dataDirectory.entries[PeDataDirectory!nBits.DataDirType.EXPORT_TABLE]
        .vaExtent.greaterThanEqual(forwardVa).length;
      forwarder = fhdr.memBytes.stringAt(disk.value, this, forwardLoc, maxBytes);

      // Check the forwarder format.
      import std.regex;
      if (matchFirst(forwarder, `^\w+\.(\w+|#\d+)$`).empty)
        appendError(forwardLoc, "malformed export forwarder string " ~ forwarder.cEscape);
    }
  }

  // If disk.value is an rva within the bounds of the export section defined in the PE optional file header data
  // directory, then disk.value is a forwarder RVA that names a symbol in another DLL.
  bool isForwarder() {
    auto fhdr = ancestor!(PeFileHeader!nBits);
    assert(fhdr !is null);

    Interval!(Word!nBits) exportPreferredExtent;
    size_t ddIdx = PeDataDirectory!nBits.DataDirType.EXPORT_TABLE;
    return (fhdr.dataDirectory !is null &&
            ddIdx < fhdr.dataDirectory.entries.length &&
            fhdr.dataDirectory.entries[ddIdx].rvaExtent.contains(disk.value));
  }
  
  bool isExport() {
    return !isForwarder();
  }

  Word!nBits forwarderRva() {
    return isForwarder ? disk.value : 0;
  }

  Word!nBits exportRva() {
    return isExport ? disk.value : 0;
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Export name table
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class PeExportNameTable(size_t nBits): Ast {
  mixin AstNodeFeatures;
  struct AstChildren {
    AstList!(PeExportNameEntry!nBits) entries;
  }

  static size_t maxEntryErrors = 25;     // max number of errors before giving up on the table
  Interval!(Word!nBits) preferredExtent; // location of this table of name pointers in virtual memory

  this() {
    entries = new AstList!(PeExportNameEntry!nBits);
  }

  void parse(ParseLocation parentLoc) {
    auto edir = ancestor!(PeExportDirectory!nBits);
    assert(edir !is null);
    auto fhdr = ancestor!(PeFileHeader!nBits);
    assert(fhdr !is null);
    auto tableVa = fhdr.va(edir.disk.namePointersRva);
    auto tableNBytes = edir.disk.nNamePointers * PeExportNameEntry!nBits.Disk.sizeof;
    preferredExtent = Interval!(Word!nBits).baseSizeTrunc(tableVa, tableNBytes);
    auto tableLoc = fhdr.vaLocation(fhdr.formatName ~ " export name table", preferredExtent.least, parentLoc);

    // The size of the table (tableNBytes) must not exceed the amount of memory contiguously mapped at that location
    size_t availBytes = fhdr.memBytes.segmentsWithin(preferredExtent).contiguous.map!"a.interval.length".fold!"a+b"(0UL);
    size_t maxEntries = availBytes / PeExportNameEntry!nBits.Disk.sizeof;
    if (tableNBytes > availBytes) {
      appendError(tableLoc, "directory-specified table size (" ~ edir.disk.nNamePointers.plural("entries") ~ ", " ~
                  tableNBytes.plural("bytes") ~ ") is larger than available size (" ~ maxEntries.plural("entries") ~ ", " ~
                  availBytes.plural("bytes") ~ ")");
    }

    // Parse each entry
    size_t startingErrorCount = totalErrorCount;
    foreach (i; 0 .. min(edir.disk.nNamePointers, maxEntries+1)) {
      auto entryLoc = indexParseLocation(fhdr.formatName ~ " export name table entry", i, tableLoc);
      entries.pushBack(PeExportNameEntry!nBits.instance());
      entries.back.parse(entryLoc);

      if (i > 0 && !(entries[i-1].name < entries[i].name))    // The names in the table must be sorted.
        entries[i].appendError(entryLoc, "incorrect sort order w.r.t. previous entry");

      if (totalErrorCount > startingErrorCount && totalErrorCount - startingErrorCount > maxEntryErrors) {
        appendError(tableLoc, "number of entry parsing errors (" ~ (totalErrorCount-startingErrorCount).to!string ~ ")" ~
                    " exceeds allowed limit for this table (" ~ maxEntryErrors.to!string ~ ")");
        break;
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Export name table entry
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class PeExportNameEntry(size_t nBits): Ast {
  mixin AstNodeFeatures;
  struct AstChildren {}

  struct Disk {
    Word!32 nameRva;
  }

  Disk disk;
  size_t index; // index in the parent node
  string name;
  Interval!(Word!nBits) preferredExtent; // location of this entry in virtual memory

  void parse(ParseLocation parentLoc) {
    auto table = ancestor!(PeExportNameTable!nBits);
    assert(table !is null);
    assert(table.entries.length > 0);
    assert(table.entries.back is this);
    auto fhdr = table.ancestor!(PeFileHeader!nBits);
    assert(fhdr !is null);

    index = table.entries.length - 1;
    auto diskVa = cast(Word!nBits)(table.preferredExtent.least + index * disk.sizeof);
    preferredExtent = Interval!(Word!nBits).baseSizeTrunc(diskVa, disk.sizeof);
    auto loc = fhdr.vaLocation(fhdr.formatName ~ " export name table entry", diskVa, parentLoc);

    if (!fhdr.memBytes.readObjectAt(diskVa, disk))
      appendError(loc, "short read");
    disk = disk.fromLittleEndian;

    auto nameVa = fhdr.va(disk.nameRva);
    auto nameLoc = fhdr.vaLocation(fhdr.formatName ~ " export name string", nameVa, loc);
    name = fhdr.memBytes.stringAt(nameVa, this, nameLoc);
  }
}
                                       
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Export ordinal table
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class PeExportOrdinalTable(size_t nBits): Ast {
  mixin AstNodeFeatures;
  struct AstChildren {
    AstList!(PeExportOrdinalEntry!nBits) entries;
  }

  static size_t maxEntryErrors = 25; // number of errors when parsing entries before giving up on this table
  Interval!(Word!nBits) preferredExtent; // location of this table of name pointers in virtual memory

  this() {
    entries = new AstList!(PeExportOrdinalEntry!nBits);
  }

  void parse(ParseLocation parentLoc) {
    auto edir = ancestor!(PeExportDirectory!nBits);
    assert(edir !is null);
    auto fhdr = ancestor!(PeFileHeader!nBits);
    assert(fhdr !is null);
    auto tableVa = fhdr.va(edir.disk.ordinalTableRva);
    auto tableNBytes = edir.disk.nNamePointers * PeExportOrdinalEntry!nBits.Disk.sizeof; // parallel with name array
    preferredExtent = Interval!(Word!nBits).baseSizeTrunc(tableVa, tableNBytes);
    auto tableLoc = fhdr.vaLocation(fhdr.formatName ~ " export ordinal table", preferredExtent.least, parentLoc);

    // The size of the table (tableNBytes) must not exceed the amount of memory contiguously mapped at that location
    size_t availBytes = fhdr.memBytes.segmentsWithin(preferredExtent).contiguous.map!"a.interval.length".fold!"a+b"(0UL);
    size_t maxEntries = availBytes / PeExportOrdinalEntry!nBits.Disk.sizeof;
    if (tableNBytes > availBytes) {
      appendError(tableLoc, "directory-specified table size (" ~ edir.disk.nNamePointers.plural("entries") ~ ", " ~
                  tableNBytes.plural("bytes") ~ ") is larger than available size (" ~ maxEntries.plural("entries") ~ ", " ~
                  availBytes.plural("bytes") ~ ")");
    }

    // Parse each entry
    size_t startingErrorCount = totalErrorCount;
    foreach (i; 0 .. min(edir.disk.nNamePointers, maxEntries+1)) {
      auto entryLoc = indexParseLocation(fhdr.formatName ~ " export ordinal table entry", i, tableLoc);
      entries.pushBack(PeExportOrdinalEntry!nBits.instance());
      entries.back.parse(entryLoc);
      if (totalErrorCount > startingErrorCount && totalErrorCount - startingErrorCount > maxEntryErrors) {
        appendError(tableLoc, "number of entry parsing errors (" ~ (totalErrorCount-startingErrorCount).to!string ~ ")" ~
                    " exceeds allowed limit for this table (" ~ maxEntryErrors.to!string ~ ")");
        break;
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Export ordinal table entry
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class PeExportOrdinalEntry(size_t nBits): Ast {
  mixin AstNodeFeatures;
  struct AstChildren {}

  struct Disk {
    Word!16 ordinal; // subtract export dir's ordinalBase to get index into export address table
  }

  Disk disk;
  size_t index; // index in the parent node
  Interval!(Word!nBits) preferredExtent; // location of this entry in virtual memory
  
  void parse(ParseLocation parentLoc) {
    auto table = ancestor!(PeExportOrdinalTable!nBits);
    assert(table !is null);
    assert(table.entries.length > 0);
    assert(table.entries.back is this);
    auto fhdr = table.ancestor!(PeFileHeader!nBits);
    assert(fhdr !is null);

    index = table.entries.length - 1;
    auto va = cast(Word!nBits)(table.preferredExtent.least + index * disk.sizeof);
    preferredExtent = Interval!(Word!nBits).baseSizeTrunc(va, disk.sizeof);
    auto loc = fhdr.vaLocation(fhdr.formatName ~ " export ordinal table entry", va, parentLoc);

    if (!fhdr.memBytes.readObjectAt(va, disk))
      appendError(loc, "short read");
    disk = disk.fromLittleEndian;

    // Error checks
    auto edir = ancestor!(PeExportDirectory!nBits);
    assert(edir !is null);
    if (disk.ordinal < edir.disk.ordinalBase) {
      appendError(loc, "ordinal (" ~ disk.ordinal.to!string ~ ") is smaller than directory's ordinal base (" ~
                  edir.disk.ordinalBase.to!string ~ ")");
    } else if (disk.ordinal - edir.disk.ordinalBase >= edir.disk.nAddressTableEntries) {
      appendError(loc, "ordinal (" ~ disk.ordinal.to!string ~ ") is beyond end of export address table (" ~
                  edir.disk.nAddressTableEntries.plural("entries") ~ " starting with #" ~
                  edir.disk.ordinalBase.to!string ~ ")");
    }
  }
}
