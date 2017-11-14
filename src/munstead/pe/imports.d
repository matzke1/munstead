module munstead.pe.imports;

import munstead.ast.base;
import munstead.core.byteorder;
import munstead.core.exception;
import munstead.core.interval;
import munstead.core.util;
import munstead.core.wordtypes;
import munstead.pe.files;
import munstead.pe.sections;
import std.algorithm: all, min;
import std.conv: to;
import std.datetime.systime: SysTime;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Import section
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Typical layout of an import section:
//    1. The directory table, ending with a null directory entry
//    2. Import lookup tables, per DLL, each terminated with a null entry
//    3. Hint-Name table
class PeImportSection(size_t nBits): PeSection!nBits {
  mixin AstNodeFeatures;
  struct AstChildren {
    PeImportDirectoryTable!nBits directory;
  }

  void parse(ParseLocation parentLoc) {
    auto fhdr = ancestor!(PeFileHeader!nBits);
    assert(fhdr !is null);
    assert(fhdr.memBytes !is null);
    
    auto loc = sectionLocation(fhdr.formatName ~ " import section", 0, parentLoc);
    directory = PeImportDirectoryTable!nBits.instance();
    directory.parse(loc);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Import directory table
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class PeImportDirectoryTable(size_t nBits): Ast {
  mixin AstNodeFeatures;
  struct AstChildren {
    AstList!(PeImportDirectoryEntry!nBits) entries;
  }

  Interval!(Word!nBits) preferredExtent; // location of this table in memory

  this() {
    entries = new AstList!(PeImportDirectoryEntry!nBits);
  }

  void parse(ParseLocation parentLoc) {
    auto fhdr = ancestor!(PeFileHeader!nBits);
    assert(fhdr !is null);
    assert(fhdr.memBytes !is null);

    auto section = ancestor!(PeImportSection!nBits);
    assert(section !is null);

    // The Import Directory Table starts at the same address as the import section containing it
    preferredExtent = section.preferredExtent;
    auto tableLoc = fhdr.vaLocation(fhdr.formatName ~ " import directory", preferredExtent.least, parentLoc);
    enum bytesPerEntry = PeImportDirectoryEntry!nBits.Disk.sizeof;

    while (true) {
      auto entryLoc = indexParseLocation(fhdr.formatName ~ " import directory entry", entries.length, tableLoc);

      auto entry = PeImportDirectoryEntry!nBits.instance();
      entries.pushBack(entry);
      entry.parse(entryLoc);
      if (entry.isZero) {
        entries.popBack();
        break;
      }
    }
  }    
}


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Import directory table entry
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class PeImportDirectoryEntry(size_t nBits): Ast {
  mixin AstNodeFeatures;
  struct AstChildren {
    PeImportLookupTable!nBits importLookupTable;
    PeImportAddressTable!nBits importAddressTable;
  }

  struct Disk {
    align(1):
    Word!32 importLookupTableRva;    // points to the ILT containing name or ordinal for each import
    Word!32 timeStamp;               // zero until image is bound, then the timestamp of the DLL
    Word!32 forwarderChainIdx;       // index of first forwarder reference
    Word!32 nameRva;                 // ptr to ASCII string
    Word!32 importAddressTableRva;   // points to the IAT initially same content as the ILT until image is bound
  }

  Disk disk;
  size_t index;                      // index of this entry in the import directory table
  bool isZero;                       // is this a null entry that contains only zeros?
  string dllName;
  Interval!(Word!nBits) preferredExtent; // virtual address of this object
  SysTime timeStamp;

  this() {
    // Initialize the time stamp to avoid segfaults from the Phobos library (occurs with version 2.076.1 and possibly others).
    timeStamp = SysTime.fromUnixTime(0);
  }

  void parse(ParseLocation parentLoc) {
    auto fhdr = ancestor!(PeFileHeader!nBits);
    assert(fhdr !is null);
    auto table = ancestor!(PeImportDirectoryTable!nBits);
    assert(table !is null);

    index = table.entries.length - 1;
    assert(table.entries[index] is this);
    auto startVa = cast(Word!nBits)(table.preferredExtent.least + index * Disk.sizeof);
    preferredExtent = Interval!(Word!nBits).baseSizeTrunc(startVa, Disk.sizeof);

    auto loc = fhdr.vaLocation(fhdr.formatName ~ " import directory entry", startVa, parentLoc);

    if (!fhdr.memBytes.readObjectAt(startVa, disk))
      appendError(loc, "short read");
    
    disk = disk.fromLittleEndian;
    isZero = (cast(ubyte[Disk.sizeof]) disk)[].all!"a==0";

    // Is this the last entry of the table? If so, it should be all zeros
    bool assumeLast;
    if (startVa + disk.sizeof - 1 == table.preferredExtent.greatest) {
      if (!isZero) {
          appendError(loc, "last import directory entry should be all zeros but isn't");
          assumeLast = true;
      }
    } else {
      if (isZero)
        appendError(loc, "non-last import directory is unexpectedly all zeros, terminating table early");
    }

    if (!isZero) {
      dllName = fhdr.memBytes.stringAt(fhdr.va(disk.nameRva), this, loc);
      timeStamp = SysTime.fromUnixTime(disk.timeStamp);

      importLookupTable = PeImportLookupTable!nBits.instance();
      importLookupTable.parse(loc);

      importAddressTable = PeImportAddressTable!nBits.instance();
      importAddressTable.parse(loc);

      if (importAddressTable.entries.length != importLookupTable.entries.length) {
        appendError(loc, "import lookup table size (" ~ importLookupTable.entries.length.plural("entries") ~ ")" ~
                    " does not match import address table size (" ~ importAddressTable.entries.length.plural("entries") ~ ")");
      }

      if (assumeLast)
        isZero = true;
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Import lookup table
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class PeImportLookupTable(size_t nBits): Ast {
  mixin AstNodeFeatures;
  struct AstChildren {
    AstList!(PeImportLookupEntry!nBits) entries;
  }

  static size_t maxEntryErrors = 25;     // max number of errors while parsing entries before giving up on table
  Interval!(Word!nBits) preferredExtent; // where this table lives in virtual memory

  this() {
    entries = new AstList!(PeImportLookupEntry!nBits);
  }

  void parse(ParseLocation parentLoc) {
    auto fhdr = ancestor!(PeFileHeader!nBits);
    assert(fhdr !is null);
    auto dirent = ancestor!(PeImportDirectoryEntry!nBits);
    assert(dirent !is null);
    auto startVa = fhdr.va(dirent.disk.importLookupTableRva);
    size_t entrySize = PeImportLookupEntry!nBits.Disk.sizeof;

    auto tableLoc = fhdr.vaLocation(fhdr.formatName ~ " import lookup table", startVa, parentLoc);

    size_t startingErrorCount = totalErrorCount;
    while (true) {
      preferredExtent = Interval!(Word!nBits).baseSizeTrunc(startVa, (entries.length+1)*entrySize);
      auto entryLoc = indexParseLocation(fhdr.formatName ~ " import lookup table entry", entries.length, tableLoc);
      entries.pushBack(PeImportLookupEntry!nBits.instance());
      entries.back.parse(entryLoc);
      if (entries.back.isZero && entries.back.errors.length == 0) {
        entries.popBack();
        break;
      }
      if (totalErrorCount > startingErrorCount && totalErrorCount - startingErrorCount > maxEntryErrors) {
        appendError(tableLoc, "number of entry parsing errors (" ~ (totalErrorCount-startingErrorCount).to!string ~ ")" ~
                    " exceeds allowed limit for this table (" ~ maxEntryErrors.to!string ~ ")");
        break;
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Import lookup table entry
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class PeImportLookupEntry(size_t nBits): Ast {
  mixin AstNodeFeatures;
  struct AstChildren {}

  struct Disk {
    Word!nBits rawValue;
  }

  Disk disk;
  size_t index;                          // index in the PeImportLookupTable
  Interval!(Word!nBits) preferredExtent; // virtual addresses for this entry (i.e, of "disk")

  Interval!(Word!nBits) hintNamePreferredExtent; // virtual addresses for the hint and name rva
  Word!16 hint;
  string name;

  void parse(ParseLocation parentLoc) {
    auto table = ancestor!(PeImportLookupTable!nBits);
    assert(table !is null);
    auto idir = table.ancestor!(PeImportDirectoryEntry!nBits);
    assert(idir !is null);
    auto fhdr = idir.ancestor!(PeFileHeader!nBits);
    assert(fhdr !is null);

    index = table.entries.length - 1;
    assert(table.entries[index] is this);
    auto startVa = cast(Word!nBits)(table.preferredExtent.least + index * Disk.sizeof);

    auto loc = fhdr.vaLocation(fhdr.formatName ~ " import lookup table entry", startVa, parentLoc);

    preferredExtent = Interval!(Word!nBits).baseSizeTrunc(startVa, disk.sizeof);
    if (!fhdr.memBytes.readObjectAt(startVa, disk.rawValue))
      appendError(loc, "short read");
    disk = disk.fromLittleEndian;

    if (!isZero) {
      if (isImportByName) {
        // hint
        Word!nBits va = fhdr.va(hintNameRva);
        hintNamePreferredExtent = Interval!(Word!nBits).baseSizeTrunc(va, hint.sizeof); // just the hint for now
        if (!fhdr.memBytes.readObjectAt(va, hint)) {
          auto hintLoc = fhdr.vaLocation(fhdr.formatName ~ " import lookup table hint", va, loc);
          appendError(hintLoc, "short read");
        }
        hint = hint.fromLittleEndian;

        // name
        va += 2;
        auto nameLoc = fhdr.vaLocation(fhdr.formatName ~ " import lookup table name", va, loc);
        name = fhdr.memBytes.stringAt(va, this, nameLoc);
        hintNamePreferredExtent = Interval!(Word!nBits).baseSizeTrunc(hintNamePreferredExtent.least,
                                                                      hint.sizeof + name.length + 1/*NUL*/);
      }
    }
  }
                                       
  bool isImportByOrdinal() const @property {
    static if (nBits == 32) {
      return (disk.rawValue & 0x80000000) != 0;
    } else {
      static assert(nBits == 64);
      return (disk.rawValue & 0x80000000_00000000uL) != 0;
    }
  }

  bool isImportByName() const @property {
    return !isImportByOrdinal;
  }

  size_t ordinal() const @property {
    assert(isImportByOrdinal());
    return disk.rawValue & 0xffff;
  }

  Word!nBits hintNameRva() const @property {
    assert(isImportByName());
    return disk.rawValue & 0x7fffffff;
  }

  bool isZero() const @property {
    return disk.rawValue == 0;
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Import address table
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class PeImportAddressTable(size_t nBits): Ast {
  mixin AstNodeFeatures;
  struct AstChildren {
    AstList!(PeImportAddressEntry!nBits) entries;
  }

  static size_t maxEntryErrors = 25;     // max number of errors while parsing entries before giving up on table
  Interval!(Word!nBits) preferredExtent; // where this table lives in virtual memory

  this() {
    entries = new AstList!(PeImportAddressEntry!nBits);
  }

  void parse(ParseLocation parentLoc) {
    auto fhdr = ancestor!(PeFileHeader!nBits);
    assert(fhdr !is null);
    auto dirent = ancestor!(PeImportDirectoryEntry!nBits);
    assert(dirent !is null);
    auto startVa = fhdr.va(dirent.disk.importAddressTableRva);
    size_t entrySize = PeImportAddressEntry!nBits.Disk.sizeof;

    auto tableLoc = fhdr.vaLocation(fhdr.formatName ~ " import address table", startVa, parentLoc);

    size_t startingErrorCount = totalErrorCount;
    while (true) {
      preferredExtent = Interval!(Word!nBits).baseSizeTrunc(startVa, (entries.length+1)*entrySize);
      auto entryLoc = indexParseLocation(fhdr.formatName ~ " import address table entry", entries.length, tableLoc);
      entries.pushBack(PeImportAddressEntry!nBits.instance());
      entries.back.parse(entryLoc);
      if (entries.back.disk.rawValue == 0 && entries.back.errors.length == 0) {
        entries.popBack();
        break;
      }
      if (totalErrorCount > startingErrorCount && totalErrorCount - startingErrorCount > maxEntryErrors) {
        appendError(tableLoc, "number of entry parsing errors (" ~ (totalErrorCount-startingErrorCount).to!string ~ ")" ~
                    " exceeds allowed limit for this table (" ~ maxEntryErrors.to!string ~ ")");
        break;
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Import address table entry
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class PeImportAddressEntry(size_t nBits): Ast {
  mixin AstNodeFeatures;
  struct AstChildren {}

  struct Disk {
    Word!nBits rawValue;
  }

  Disk disk;
  size_t index;                          // index in the PeImportAddressTable
  Interval!(Word!nBits) preferredExtent; // virtual addresses for this entry (i.e, of "disk")

  void parse(ParseLocation parentLoc) {
    auto table = ancestor!(PeImportAddressTable!nBits);
    assert(table !is null);
    auto idir = table.ancestor!(PeImportDirectoryEntry!nBits);
    assert(idir !is null);
    auto fhdr = idir.ancestor!(PeFileHeader!nBits);
    assert(fhdr !is null);

    index = table.entries.length - 1;
    assert(table.entries[index] is this);
    auto startVa = cast(Word!nBits)(table.preferredExtent.least + index * Disk.sizeof);

    auto loc = fhdr.vaLocation(fhdr.formatName ~ " import address table entry", startVa, parentLoc);

    preferredExtent = Interval!(Word!nBits).baseSizeTrunc(startVa, disk.sizeof);
    if (!fhdr.memBytes.readObjectAt(startVa, disk.rawValue))
      appendError(loc, "short read");
    disk = disk.fromLittleEndian;
  }
}
