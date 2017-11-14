module munstead.elf.dynamic;

import munstead.ast.base;
import munstead.core.bitflags;
import munstead.core.byteorder;
import munstead.core.exception;
import munstead.core.interval;
import munstead.core.mmap;
import munstead.core.util;
import munstead.core.wordtypes;
import munstead.elf.array;
import munstead.elf.files;
import munstead.elf.reloc;
import munstead.elf.sections;
import munstead.elf.segments;
import munstead.elf.strings;
import munstead.elf.symbols;
import munstead.elf.types;
import std.algorithm: filter, map;
import std.array: array;
import std.conv: to;
import std.range: take;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class ElfDynamicSection(size_t nBits): ElfSection!nBits {
  mixin AstNodeFeatures;
  struct AstChildren {
    AstList!(ElfDynamicEntry!nBits) entries;
  }

  ElfStringTable!nBits strtab;                         // first DT_STRTAB
  ElfSymbolTable!nBits symtab;                         // first DT_SYMTAB
  ElfArray!nBits initArray;                            // first DT_INIT_ARRAY
  ElfArray!nBits finiArray;                            // first DT_FINI_ARRAY
  ElfRelocSection!(nBits, ElfRelocHasAddend.YES) rela; // first DT_REL
  ElfRelocSection!(nBits, ElfRelocHasAddend.NO) rel;   // first DT_RELA

  this() {
    entries = new AstList!(ElfDynamicEntry!nBits);
  }

  // Parsing phase I. Parses and builds the entries of the table, but none of the entries themselves get parsed yet
  // because many of them refer to virtual memory addresses rather than file offsets.
  void parse(ParseLocation parentLoc) {
    auto fhdr = ancestor!(ElfFileHeader!nBits);
    assert(fhdr !is null);
    auto tableLoc = sectionLocation(fhdr.formatName ~ " dynamic section", 0, parentLoc);

    enum bytesPerEntry = ElfDynamicEntry!nBits.Disk.sizeof;
    size_t nEntries = bytes.length / bytesPerEntry;
    for (size_t index = 0; index < nEntries; ++index) {
      auto entryLoc = indexParseLocation(fhdr.formatName ~ " dynamic section entry", index, tableLoc);
      auto entry = ElfDynamicEntry!nBits.instance();
      entries.pushBack(entry);
      entry.parse(entryLoc);
      if (entry.Tag.DT_NULL == entry.disk.d_tag)
        break;
    }
  }

  // For sections that come from an Elf Section Table, set info from the section table entry's sh_link and sh_info.
  override void setLinkInfo(ElfSectionTableEntry!nBits shent, ParseLocation parentLoc) {
    assert(shent !is null);
    auto fhdr = ancestor!(ElfFileHeader!nBits);
    assert(fhdr !is null);
    assert(fhdr.sectionTable !is null);
    auto loc = sectionLocation(fhdr.formatName ~ " dynamic section", 0, parentLoc);

    // Handle the sh_link field
    assert(strtab is null); // we haven't parsed the dynamic section's contents yet
    strtab = fhdr.sectionByIndex!(ElfStringTable!nBits)(shent.disk.sh_link, this, loc);
    
    // Handle the sh_info field
    if (shent.disk.sh_info != 0)
      appendError(loc, "sh_info field is non-zero (" ~ shent.disk.sh_info.to!string ~ ")");
  }

  // Parsing phase II. This should be called after the program is loaded into memory.
  void parseMemory(ParseLocation parentLoc) {
    auto fhdr = ancestor!(ElfFileHeader!nBits);
    assert(fhdr !is null);

    auto loc = sectionLocation(fhdr.formatName ~ " dynamic section", 0, parentLoc);

    // Order is important for some of these
    parseStrtabDetails(fhdr, loc);
    parseRelDetails(fhdr, loc);
    parseRelaDetails(fhdr, loc);
    parseNeededDetails(fhdr, loc);
    parseSymtabDetails(fhdr, loc);
    parseSonameDetails(fhdr, loc);
    parseRpathDetails(fhdr, loc);
    parseRunpathDetails(fhdr, loc);
    parseFlagsDetails(fhdr, loc);
    parseFlags1Details(fhdr, loc);
    parsePosflag1Details(fhdr, loc);
    parseInitArrayDetails(fhdr, loc);
    parseFiniArrayDetails(fhdr, loc);
  }

  // Looks for an address and size tag in the dynamic section and uses that to either find an existing section
  // at that address (and verify it has the correct size) or to parse a new section using the virtual memory map.
  // Produces errors if multiple address entries exist, or returns null (without error) if no address entry exists.
  // If an address entry exists, check that exacly one size entry exists.
  T parseTable(T)(ElfFileHeader!nBits fhdr, ElfDynamicEntry!nBits.Tag addrTag, ElfDynamicEntry!nBits.Tag sizeTag,
                  ParseLocation parentLoc) {
    assert(fhdr !is null);
    auto loc = parentLoc;

    // Get the size
    auto sizes = entries[]
      .filter!(entry => entry.disk.d_tag == sizeTag)
      .map!"a.disk.d_val"
      .take(2).array;
    if (sizes.length > 1)
      appendError(loc, "has multiple " ~ sizeTag.to!string ~ " entries");
    size_t size = sizes.length > 0 ? sizes[0] : 0;

    // Get the address
    auto addresses = entries[].filter!(entry => entry.disk.d_tag == addrTag).map!"a.disk.d_val".take(2).array;
    if (addresses.length > 1)
      appendError(loc, "has multiple " ~ addrTag.to!string ~ " entries");
    if (addresses.length > 0 && sizes.length == 0)
      appendError(loc, " has " ~ addrTag.to!string ~ " but not " ~ sizeTag.to!string);

    // Find an existing section or parse one from memory
    T table;
    if (addresses.length > 0) {
      table = fhdr.sectionMappedAt!T(addresses[0]);
      bool shouldCreate = table is null;

      // Sometimes the .dynamic tables don't correspond directly with sections. For instance, on my machine /usr/bin/emacs24-x
      // the .dynamic DT_RELA, DT_RELASZ, and DT_RELAENT triple is equivalent to a single large relocation consisting of both
      // ".rela.got" and ".rela,bss" relocation sections, which are at neighboring memory locations.
      if (table !is null && table.fileExtent.length != size) {
        static if (1) {
          shouldCreate = true;
        } else {
          appendError(loc, sizeTag.to!string ~ " (" ~ size.to!string ~ ")" ~
                      " does not match the size of " ~ table.printableName ~
                      " (" ~ table.fileExtent.length.to!string ~ ")");
        }
      }

      if (shouldCreate) {
        table = T.instance();
        fhdr.sections.pushBack(table);
        auto interval = Interval!(Word!nBits).baseSizeTrunc(addresses[0], size);
        table.createContent(fhdr.memBytes, interval);
        table.parse(loc);
      }
    }
    return table;
  }
      
  // Find the string table needed by many of the other entries.
  void parseStrtabDetails(ElfFileHeader!nBits fhdr, ParseLocation parentLoc) {
    auto loc = parentLoc;
    auto oldStrtab = strtab;
    with (ElfDynamicEntry!nBits.Tag) {
      strtab = parseTable!(ElfStringTable!nBits)(fhdr, DT_STRTAB, DT_STRSZ, parentLoc);
      if (strtab is null)
        appendError(loc, "missing DT_STRTAB entry");
    }

    if (strtab !is null && oldStrtab !is null && strtab !is oldStrtab) {
      appendError(loc, "DT_STRTAB entry (" ~ strtab.printableName ~ ")" ~
                  " points to a different string table than the sht_link field of the section table entry" ~
                  " (" ~ oldStrtab.printableName ~ ")");
    }
  }

  void parseRelDetails(ElfFileHeader!nBits fhdr, ParseLocation parentLoc) {
    with (ElfDynamicEntry!nBits.Tag)
      rel = parseTable!(ElfRelocSection!(nBits, ElfRelocHasAddend.NO))(fhdr, DT_REL, DT_RELSZ, parentLoc);
  }
      
  void parseRelaDetails(ElfFileHeader!nBits fhdr, ParseLocation parentLoc) {
    with (ElfDynamicEntry!nBits.Tag)
      rela = parseTable!(ElfRelocSection!(nBits, ElfRelocHasAddend.YES))(fhdr, DT_RELA, DT_RELASZ, parentLoc);
  }
      
  void parseNeededDetails(ElfFileHeader!nBits fhdr, ParseLocation parentLoc) {
    assert(fhdr !is null);
    auto loc = parentLoc;
    if (strtab is null) {
      appendError(loc, "has DT_NEEDED but no string table");
    } else {
      foreach (entry; entries[].filter!(entry => entry.disk.d_tag == entry.Tag.DT_NEEDED))
        entry.detail.needed = strtab.stringAt(entry.disk.d_val, this, loc);
    }
  }

  void parseSymtabDetails(ElfFileHeader!nBits fhdr, ParseLocation parentLoc) {
    assert(fhdr !is null);
    auto loc = parentLoc;
    size_t nFound;
    foreach (entry; entries[].filter!(entry => entry.disk.d_tag == entry.Tag.DT_SYMTAB)) {
      if (0 == nFound++)
        symtab = fhdr.sectionMappedAt!(ElfSymbolTable!nBits)(entry.disk.d_val);
    }
    if (nFound == 0) {
      appendError(loc, "section has no DT_SYMTAB entry");
    } else if (nFound > 1) {
      appendError(loc, "section has multiple DT_SYMTAB entries");
    }
  }

  void parseSonameDetails(ElfFileHeader!nBits fhdr, ParseLocation parentLoc) {
    assert(fhdr !is null);
    auto loc = parentLoc;
    if (strtab is null) {
      appendError(loc, "has DT_SONAME but no string table");
    } else {
      foreach (entry; entries[].filter!(entry => entry.disk.d_tag == entry.Tag.DT_SONAME))
        entry.detail.soname = strtab.stringAt(entry.disk.d_val, this, parentLoc);
    }
  }

  void parseRpathDetails(ElfFileHeader!nBits fhdr, ParseLocation parentLoc) {
    assert(fhdr !is null);
    auto loc = parentLoc;
    if (strtab is null) {
      appendError(loc, "has DT_RPATH but no string table");
    } else {
      foreach (entry; entries[].filter!(entry => entry.disk.d_tag == entry.Tag.DT_RPATH))
        entry.detail.rpath = strtab.stringAt(entry.disk.d_val, this, parentLoc);
    }
  }

  void parseRunpathDetails(ElfFileHeader!nBits fhdr, ParseLocation parentLoc) {
    assert(fhdr !is null);
    auto loc = parentLoc;
    if (strtab is null) {
      appendError(loc, "has DT_RUNPATH but no string table");
    } else {
      foreach (entry; entries[].filter!(entry => entry.disk.d_tag == entry.Tag.DT_RUNPATH))
        entry.detail.runpath = strtab.stringAt(entry.disk.d_val, this, loc);
    }
  }

  void parseFlagsDetails(ElfFileHeader!nBits fhdr, ParseLocation parentLoc) {
    assert(fhdr !is null);
    foreach (entry; entries[].filter!(entry => entry.disk.d_tag == entry.Tag.DT_FLAGS))
      entry.detail.flags.value = cast(entry.Flags) entry.disk.d_val;
  }

  void parseFlags1Details(ElfFileHeader!nBits fhdr, ParseLocation parentLoc) {
    assert(fhdr !is null);
    foreach (entry; entries[].filter!(entry => entry.disk.d_tag == entry.Tag.DT_FLAGS_1))
      entry.detail.flags1.value = cast(entry.Flags1) entry.disk.d_val;
  }

  void parsePosflag1Details(ElfFileHeader!nBits fhdr, ParseLocation parentLoc) {
    assert(fhdr !is null);
    foreach (entry; entries[].filter!(entry => entry.disk.d_tag == entry.Tag.DT_POSFLAG_1))
      entry.detail.posflag1.value = cast(entry.Posflag1) entry.disk.d_val;
  }

  void parseInitArrayDetails(ElfFileHeader!nBits fhdr, ParseLocation parentLoc) {
    with (ElfDynamicEntry!nBits.Tag)
      initArray = parseTable!(ElfArray!nBits)(fhdr, DT_INIT_ARRAY, DT_INIT_ARRAYSZ, parentLoc);
  }

  void parseFiniArrayDetails(ElfFileHeader!nBits fhdr, ParseLocation parentLoc) {
    with (ElfDynamicEntry!nBits.Tag)
      finiArray = parseTable!(ElfArray!nBits)(fhdr, DT_FINI_ARRAY, DT_FINI_ARRAYSZ, parentLoc);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class ElfDynamicEntry(size_t nBits): Ast {
  mixin AstNodeFeatures;
  struct AstChildren {}

  enum Tag : Elf!nBits.Sxword {
    //                                      Union       Executable  Library     Level-2
    //                                      ----------- ----------- ----------- ---------
    DT_NULL                =0,           // ignored     mandatory   mandatory   no
    DT_NEEDED              =1,           // d_val       optional    optional    no
    DT_PLTRELSZ            =2,           // d_val       optional    optional    no
    DT_PLTGOT              =3,           // d_ptr       optional    optional    no
    DT_HASH                =4,           // d_ptr       mandatory   mandatory   no
    DT_STRTAB              =5,           // d_ptr       mandatory   mandatory   no
    DT_SYMTAB              =6,           // d_ptr       mandatory   mandatory   no
    DT_RELA                =7,           // d_ptr       mandatory   optional    no
    DT_RELASZ              =8,           // d_val       mandatory   optional    no
    DT_RELAENT             =9,           // d_val       mandatory   optional    no
    DT_STRSZ               =10,          // d_val       mandatory   mandatory   no
    DT_SYMENT              =11,          // d_val       mandatory   mandatory   no
    DT_INIT                =12,          // d_ptr       optional    optional    no
    DT_FINI                =13,          // d_ptr       optional    optional    no
    DT_SONAME              =14,          // d_val       ignored     optional    no
    DT_RPATH               =15,          // d_val       optional    ignored     yes
    DT_SYMBOLIC            =16,          // ignored     ignored     optional    yes
    DT_REL                 =17,          // d_ptr       mandatory   optional    no
    DT_RELSZ               =18,          // d_val       mandatory   optional    no
    DT_RELENT              =19,          // d_val       mandatory   optional    no
    DT_PLTREL              =20,          // d_val       optional    optional    no
    DT_DEBUG               =21,          // d_ptr       optional    ignored     no
    DT_TEXTREL             =22,          // ignored     optional    optional    yes
    DT_JMPREL              =23,          // d_ptr       optional    optional    no
    DT_BIND_NOW            =24,          // ignored     optional    optional    yes
    DT_INIT_ARRAY          =25,          // d_ptr       optional    optional    no
    DT_FINI_ARRAY          =26,          // d_ptr       optional    optional    no
    DT_INIT_ARRAYSZ        =27,          // d_val       optional    optional    no
    DT_FINI_ARRAYSZ        =28,          // d_val       optional    optional    no
    DT_RUNPATH             =29,          // d_val       optional    optional    no
    DT_FLAGS               =30,          // d_val       optional    optional    no
    DT_ENCODING            =32,          // unspecified unspecified unspecified no
    DT_PREINIT_ARRAY       =32,          // d_ptr       optional    ignored     no
    DT_PREINIT_ARRAYSZ     =33,          // d_val       optional    ignored     no
    DT_SYMTAB_SHNDX        =34,          // d_ptr       optional    optional    no

    // From https://docs.oracle.com/cd/E23824_01/html/819-0690/chapter6-42444.html
    DT_SUNW_AUXILIARY      = 0x6000000d, // d_ptr       unspecified optional
    DT_SUNW_RTLDINF        = 0x6000000e, // d_ptr       optional    optional
    DT_SUNW_FILTER         = 0x6000000e, // d_ptr       unspecified optional         (doc says 0x60000e)
    DT_SUNW_CAP            = 0x60000010, // d_ptr       optional    optional
    DT_SUNW_SYMTAB         = 0x60000011, // d_ptr       optional    optional
    DT_SUNW_SYMSZ          = 0x60000012, // d_val       optional    optional
    DT_SUNW_ENCODING       = 0x60000013, // unspecified unspecified unspecified
    DT_SUNW_SORTENT        = 0x60000013, // d_ptr       optional    optional         (doc says 0x60000013)
    DT_SUNW_SYMSORT        = 0x60000014, // d_ptr       optional    optional
    DT_SUNW_SYMSORTSZ      = 0x60000015, // d_val       optional    optional
    DT_SUNW_TLSSORT        = 0x60000016, // d_ptr       optional    optional
    DT_SUNW_TLSSORTSZ      = 0x60000017, // d_val       optional    optional
    DT_SUNW_CAPINFO        = 0x60000018, // d_ptr       optional    optional
    DT_SUNW_STRPAD         = 0x60000019, // d_val       optional    optional
    DT_SUNW_CAPCHAIN       = 0x6000001a, // d_ptr       optional    optional
    DT_SUNW_LDMATCH        = 0x6000001b, // d_val       optional    optional
    DT_SUNW_CAPCHAINENT    = 0x6000001d, // d_val       optional    optional
    DT_SUNW_CAPCHAINSZ     = 0x6000001f, // d_val       optional    optional

    // From https://docs.oracle.com/cd/E23824_01/html/819-0690/chapter6-42444.html
    DT_VALRNGLO            = 0x6ffffd00, // unspecified unspecified unspecified
    DT_CHECKSUM            = 0x6ffffdf8, // d_val       optional    optional
    DT_PLTPADSZ            = 0x6ffffdf9, // d_val       optional    optional
    DT_MOVEENT             = 0x6ffffdfa, // d_val       optional    optional
    DT_MOVESZ              = 0x6ffffdfb, // d_val       optional    optional
    DT_POSFLAG_1           = 0x6ffffdfd, // d_val       optional    optional
    DT_SYMINSZ             = 0x6ffffdfe, // d_val       optional    optional
    DT_SYMINENT            = 0x6ffffdff, // d_val       optional    optional
    DT_VALRNGHI            = 0x6ffffdff, // unspecified unspecified unspecified
    DT_ADDRRNGLO           = 0x6ffffe00, // unspecified unspecified unspecified
    DT_CONFIG              = 0x6ffffefa, // d_ptr       optional    optional
    DT_DEPAUDIT            = 0x6ffffefb, // d_ptr       optional    optional
    DT_AUDIT               = 0x6ffffefc, // d_ptr       optional    optional
    DT_PLTPAD              = 0x6ffffefd, // d_ptr       optional    optional
    DT_MOVETAB             = 0x6ffffefe, // d_ptr       optional    optional
    DT_SYMINFO             = 0x6ffffeff, // d_ptr       optional    optional
    DT_ADDRRNGHI           = 0x6ffffeff, // unspecified unspecified unspecified
    DT_RELACOUNT           = 0x6ffffff9, // d_val       optional    optional
    DT_RELCOUNT            = 0x6ffffffa, // d_val       optional    optional
    DT_FLAGS_1             = 0x6ffffffb, // d_val       optional    optional
    DT_VERDEF              = 0x6ffffffc, // d_ptr       optional    optional
    DT_VERDEFNUM           = 0x6ffffffd, // d_val       optional    optional
    DT_VERNEED             = 0x6ffffffe, // d_ptr       optional    optional
    DT_VERNEEDNUM          = 0x6fffffff, // d_val       optional    optional
    DT_SPARC_REGISTER      = 0x70000001, // d_val       optional    optional
    DT_AUXILIARY           = 0x7ffffffd, // d_val       unspecified optional
    DT_USED                = 0x7ffffffe, // d_val       optional    optional
    DT_FILTER              = 0x7fffffff, // d_val       unspecified optional

    // From running the real 'readelf' on Linux
    DT_GNU_HASH            = 0x6ffffef5,
    DT_VERSYM              = 0x6ffffff0,

    // Limits
    DT_LOOS                = 0x6000000D, // unspecified unspecified unspecified no
    DT_HIOS                = 0x6ffff000, // unspecified unspecified unspecified no
    DT_LOPROC              = 0x70000000, // unspecified unspecified unspecified no
    DT_HIPROC              = 0x7fffffff  // unspecified unspecified unspecified no
  }

  // Bit flags for the DT_FLAGS entry
  enum Flags {
    DF_ORIGIN     = 0x01,
    DF_SYMBOLIC   = 0x02,
    DF_TEXTREL    = 0x04,
    DF_BIND_NOW   = 0x08,
    DF_STATIC_TLS = 0x10
  }

  // Bit flags for the DT_FLAGS_1 entry. From https://docs.oracle.com/cd/E23824_01/html/819-0690/chapter6-42444.html
  enum Flags1 {
    DF_1_NOW        = 0x00000001, // Perform complete relocation processing.
    DF_1_GLOBAL     = 0x00000002, // Unused.
    DF_1_GROUP      = 0x00000004, // Indicate object is a member of a group.
    DF_1_NODELETE   = 0x00000008, // Object cannot be deleted from a process.
    DF_1_LOADFLTR   = 0x00000010, // Ensure immediate loading of filtees.
    DF_1_INITFIRST  = 0x00000020, // Objects' initialization occurs first.
    DF_1_NOOPEN     = 0x00000040, // Object can not be used with dlopen(3C).
    DF_1_ORIGIN     = 0x00000080, // $ORIGIN processing required.
    DF_1_DIRECT     = 0x00000100, // Direct bindings enabled.
    // unused?      = 0x00000200,
    DF_1_INTERPOSE  = 0x00000400, // Object is an interposer.
    DF_1_NODEFLIB   = 0x00000800, // Ignore the default library search path.
    DF_1_NODUMP     = 0x00001000, // Object cannot be dumped with dldump(3C).
    DF_1_CONFALT    = 0x00002000, // Object is a configuration alternative.
    DF_1_ENDFILTEE  = 0x00004000, // Filtee terminates filter's search.
    DF_1_DISPRELDNE = 0x00008000, // Displacement relocation has been carried out.
    DF_1_DISPRELPND = 0x00010000, // Displacement relocation pending.
    DF_1_NODIRECT   = 0x00020000, // Object contains non-direct bindings.
    DF_1_IGNMULDEF  = 0x00040000, // Internal use.
    DF_1_NOKSYMS    = 0x00080000, // Internal use.
    DF_1_NOHDR      = 0x00100000, // Internal use.
    DF_1_EDITED     = 0x00200000, // Object has been modified since originally built.
    DF_1_NORELOC    = 0x00400000, // Internal use.
    DF_1_SYMINTPOSE = 0x00800000, // Individual symbol interposers exist.
    DF_1_GLOBAUDIT  = 0x01000000, // Establish global auditing.
    DF_1_SINGLETON  = 0x02000000, // Singleton symbols exist.
    // unused?      = 0x04000000,
    DF_1_PIE        = 0x08000000  // Position independent executable
  }

  // Bit flags for the DT_POSFLAG_1 entry. From https://docs.oracle.com/cd/E23824_01/html/819-0690/chapter6-42444.html
  enum Posflag1 {
    DF_P1_LAZYLOAD  = 0x00000001,
    DF_P1_GROUPPERM = 0x00000002
  }

  struct Disk {
    align(1):
    Tag d_tag;
    Elf!nBits.Xword d_val; // or file memory address d_addr which is the same size
  }

  // Details are discriminated by disk.d_tag
  union Detail {
    string needed;               // DT_NEEDED
    string soname;               // DT_SONAME
    string rpath;                // DT_RPATH
    string runpath;              // DT_RUNPATH
    BitFlags!Flags flags;        // DT_FLAGS
    BitFlags!Flags1 flags1;      // DT_FLAGS_1
    BitFlags!Posflag1 posflag1;  // DT_POSFLAG_1
  }
  
  size_t index; // index of this entry in the dynamic section
  Disk disk;
  Detail detail;

  void parse(ParseLocation parentLoc) {
    auto fhdr = ancestor!(ElfFileHeader!nBits);
    assert(fhdr !is null);
    auto table = ancestor!(ElfDynamicSection!nBits);
    assert(table !is null);

    index = table.entries.length - 1;
    assert(table.entries[index] is this);
    auto offset = cast(Word!nBits)(index * Disk.sizeof);
    auto loc = table.sectionLocation(fhdr.formatName ~ " dynamic entry", offset, parentLoc);

    table.bytes.readObjectAt(offset, disk);
    disk = toNative(disk, fhdr.byteOrder);
  }

  bool isPointer() const pure {
    with (Tag) {
      switch (disk.d_tag) {
        case DT_PLTGOT:
        case DT_HASH:
        case DT_STRTAB:
        case DT_SYMTAB:
        case DT_RELA:
        case DT_INIT:
        case DT_FINI:
        case DT_REL:
        case DT_DEBUG:
        case DT_JMPREL:
        case DT_INIT_ARRAY:
        case DT_FINI_ARRAY:
        case DT_PREINIT_ARRAY:
        case DT_SYMTAB_SHNDX:
        case DT_SUNW_AUXILIARY:
        case DT_SUNW_RTLDINF:
        // case DT_SUNW_FILTER:  -- same as DT_SUNW_RTLDINF
        case DT_SUNW_CAP:
        case DT_SUNW_SYMTAB:
        case DT_SUNW_SORTENT:
        case DT_SUNW_SYMSORT:
        case DT_SUNW_TLSSORT:
        case DT_SUNW_CAPINFO:
        case DT_SUNW_CAPCHAIN:
        case DT_CONFIG:
        case DT_DEPAUDIT:
        case DT_AUDIT:
        case DT_PLTPAD:
        case DT_MOVETAB:
        case DT_SYMINFO:
        case DT_VERDEF:
        case DT_VERNEED:
        case DT_GNU_HASH:
          return true;
        default:
          if (disk.d_tag >= DT_ENCODING && (disk.d_tag < DT_LOPROC || disk.d_tag > DT_HIPROC))
            return disk.d_tag % 2 == 0;
          return false;
      }
    }
  }
}                                    
