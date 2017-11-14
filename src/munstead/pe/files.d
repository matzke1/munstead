module munstead.pe.files;

import core.bitop: popcnt;
import munstead.ast.base;
import munstead.ast.files;
import munstead.core.bitflags;
import munstead.core.byteorder;
import munstead.core.exception;
import munstead.core.interval;
import munstead.core.mmap;
import munstead.core.util;
import munstead.core.wordtypes;
import munstead.dos.files;
import munstead.pe.exports;
import munstead.pe.imports;
import munstead.pe.loader;
import munstead.pe.resources;
import munstead.pe.sections;
import std.algorithm: fold, map;
import std.array: array;
import std.conv: to;
import std.datetime.systime: SysTime;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Base class for Windows PE files
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class PeFile: AsmFile {
  mixin AstNodeFeatures;
  struct AstChildren {}
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// PE file header
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class PeFileHeader(size_t nBits): PeFile {
  mixin AstNodeFeatures;
  struct AstChildren {
    DosFileHeader!nBits dosHeader; // all PE files start with a DOS file header
    PeCoffFileHeader!nBits coffHeader;
    PeOptionalFileHeader!nBits optionalHeader; // required for images; optional for objects
    PeSectionTable!nBits sectionTable;
    PeDataDirectory!nBits dataDirectory;

    // All sections whether created because they belong to a section table, segment table, or other reason.
    AstList!(PeSection!nBits) sections;
  }

  MemoryMap!(Word!nBits) fileBytes;       // data in the file address space
  MemoryMap!(Word!nBits) memBytes;        // data mapped using its preferred addresses
  Word!32 peMagicOffset;                  // file offset where "PE\0\0" magic number would be located
  PeFileLoader!nBits fileLoader;          // maps whole file into memory at its preferred address
  PeSectionLoader!nBits sectionLoader;    // maps sections into memory
  
  this() {
    sections = new AstList!(PeSection!nBits);
  }

  // Before parsing, you must assign a map to the "fileBytes" member. This map should contain the file contents to be
  // parsed starting at offset zero.
  void parse(ParseLocation parentLoc) {
    auto loc = fileLocation("", 0, parentLoc);
    parseBasicHeaders(loc);
    parseRemainingHeaders(loc);
    parseSectionTable(loc);

    // Create the memory image. If the total size of mapped memory is different than the optional header's sizeOfImage
    // field, then the optional header should incurr an error.
    loadFile();
    loadSectionTableEntries();
    if (memBytes.hull.length != optionalHeader.disk.sizeOfImage) {
      auto ohLoc = fileLocation(formatName ~ " \"optional\" header", optionalHeader.fileExtent.least, loc);
      optionalHeader.appendError(ohLoc, "optional header's size of image (" ~ optionalHeader.disk.sizeOfImage.hexStr ~ ")" ~
				 " does not match memory map hull size (" ~ memBytes.hull.length.hexStr ~ ")");
    }

    // Build sections from memory.
    createExportSections(loc);
    createImportSections(loc);
    createResourceSections(loc);
    createGenericSections(loc);
  }

  // Alternative parsing method where the user supplies a file name.
  void parseFile(string fileName) {
    fileBytes = new MemoryMap!(Word!nBits);
    fileBytes.insertFile(fileName, 0);
    parse(null);
  }

  // True if the file looks like it's proabably a PE file of the right type (PE32 or PE32+).
  static bool testFile(string fileName) {
    auto fhdr = PeFileHeader.instance();
    fhdr.fileBytes = new MemoryMap!(Word!nBits);
    fhdr.fileBytes.insertFile(fileName, 0);
    try {
      auto loc = fhdr.fileLocation("", 0, null);
      fhdr.parseBasicHeaders(loc);
      if (fhdr.preOrder.map!(node => node.errors.length).fold!"a+b"(0uL) > 0)
        return false;

      // look for the 2-byte PE format constant that immediately follows the COFF header, which we didn't parse
      auto offset = cast(Word!nBits)(fhdr.peMagicOffset + 4 + PeCoffFileHeader!nBits.disk.sizeof);
      Word!16 peFmt;
      if (!fhdr.fileBytes.readObjectAt(offset, peFmt))
        return false;
      peFmt = peFmt.fromLittleEndian;
      return ((nBits == 32 && peFmt == PeOptionalFileHeader!nBits.PeFormat.PE32) ||
              (nBits == 64 && peFmt == PeOptionalFileHeader!nBits.PeFormat.PE32PLUS));
    } catch {
      return false;
    }
  }

  // Name of the file format
  static string formatName() {
    static if (nBits == 32)
      return "PE32";
    static if (nBits == 64)
      return "PE32+";
    assert(0);
  }

  // Parse all the file headers non-recursively.  Image files (executables) start with a DOS file header, then a
  // fourt-byte "PE\0\0" magic number, then the PE file header. Object files start directly with a PE file header (no
  // DOS file header or "PE\0\0" magic). We're only handling the image format for the time being (FIXME).
  void parseBasicHeaders(ParseLocation parentLoc) {
    dosHeader = DosFileHeader!nBits.instance();
    dosHeader.fileBytes = fileBytes;
    dosHeader.parseHeader(parentLoc);
    parsePeMagic(parentLoc);
  }

  void parseRemainingHeaders(ParseLocation parentLoc) {
    coffHeader = PeCoffFileHeader!nBits.instance();
    coffHeader.parse(parentLoc);

    optionalHeader = PeOptionalFileHeader!nBits.instance();
    optionalHeader.parse(parentLoc);

    dataDirectory = PeDataDirectory!nBits.instance();
    dataDirectory.parse(parentLoc);
  }    

  void parsePeMagic(ParseLocation parentLoc) {
    auto loc = fileLocation(formatName ~ " magic number location", 0x3c, parentLoc);

    // File offset for PE magic number
    if (!fileBytes.readObjectAt(0x3c, peMagicOffset))
      appendError(loc, "short read of magic number offset at 0x3c");
    peMagicOffset = peMagicOffset.fromLittleEndian;
    
    // PE magic number
    loc = fileLocation(formatName ~ " magic number", peMagicOffset, loc);
    ubyte[4] peMagic;
    if (!fileBytes.readObjectAt(peMagicOffset, peMagic))
      appendError(loc, "short read at " ~ peMagicOffset.hexStr);
    if (peMagic[0] != 'P' || peMagic[1] != 'E' || peMagic[2] != 0 || peMagic[3] != 0)
      appendError(loc, "invalid (" ~ peMagic.to!string ~ ")");
  }

  // Convert a relative virtual address to a virtual address
  Word!nBits va(Word!nBits rva) {
    return cast(Word!nBits)(optionalHeader.disk.imageBase + rva);
  }

  // Load the file into virtual memory at its preferred address. This is usually in preparation for parsing various
  // tables in the optional header's data directory.
  void loadFile() {
    if (fileLoader is null)
      fileLoader = new PeFileLoader!nBits;
    if (memBytes is null)
      memBytes = new MemoryMap!(Word!nBits);
    memBytes = fileLoader.load(this);
  }

  // Parse the section table and its entries. Do not create actual section yet.
  void parseSectionTable(ParseLocation parentLoc) {
    sectionTable = PeSectionTable!nBits.instance();
    sectionTable.parse(parentLoc);
  }
  
  // Create a loader (if the user hasn't already) and use it to load some sections into the memBytes map.
  void loadSectionTableEntries() {
    if (sectionLoader is null)
      sectionLoader = new PeSectionLoader!nBits;
    if (memBytes is null)
      memBytes = new MemoryMap!(Word!nBits);
    sectionLoader.loadSectionTableEntries(memBytes, this);
  }

  // Create the export section pointed to by the optional header
  void createExportSections(ParseLocation parentLoc) {
    size_t idx = PeDataDirectory!nBits.DataDirType.EXPORT_TABLE;
    if (dataDirectory !is null && dataDirectory.entries.length > idx) {
      auto ddent = dataDirectory.entries[idx];
      if (ddent.disk.nBytes > 0) {
        auto dirLoc = indexParseLocation(formatName ~ " header data directory entry ", idx, parentLoc);
        auto entryLoc = fileLocation(formatName ~ " header data directory entry", ddent.fileExtent.least, dirLoc);
        auto exports = PeExportSection!nBits.instance();
        sections.pushBack(exports); // always attach to AST before parsing
        exports.createContent(ddent);
        exports.parse(entryLoc);
      }
    }
  }

  // Create the import section pointed to by the optional header
  void createImportSections(ParseLocation parentLoc) {
    size_t idx = PeDataDirectory!nBits.DataDirType.IMPORT_TABLE;
    if (dataDirectory !is null && dataDirectory.entries.length > idx) {
      auto ddent = dataDirectory.entries[idx];
      if (ddent.disk.nBytes > 0) {
        auto dirLoc = indexParseLocation(formatName ~ " header data directory entry", idx, parentLoc);
        auto entryLoc = fileLocation(formatName ~ " header data directory entry", ddent.fileExtent.least, dirLoc);
        auto imports = PeImportSection!nBits.instance();
        sections.pushBack(imports); // always attach to AST before parsing
        imports.createContent(ddent);
        imports.parse(entryLoc);
      }
    }
  }

  // Create the resource section pointed to by the optional header
  void createResourceSections(ParseLocation parentLoc) {
    size_t idx = PeDataDirectory!nBits.DataDirType.RESOURCE_TABLE;
    if (dataDirectory !is null && dataDirectory.entries.length > idx) {
      auto ddent = dataDirectory.entries[idx];
      if (ddent.disk.nBytes > 0) {
	auto dirLoc = indexParseLocation(formatName ~ " header directory entry", idx, parentLoc);
	auto entryLoc = fileLocation(formatName ~ " header data directory entry", ddent.fileExtent.least, dirLoc);
	auto resources = PeResourceSection!nBits.instance();
	sections.pushBack(resources);
	resources.createContent(ddent);
	resources.parse(entryLoc);
      }
    }
  }
      
  // Create sections generically for section table entries that don't have sections yet
  void createGenericSections(ParseLocation parentLoc) {
    if (sectionTable !is null) {
      auto tableLoc = fileLocation(formatName ~ " section table", sectionTable.fileExtent.least, parentLoc);
      foreach (shent; sectionTable.entries[]) {
        if (shent.section is null) {
          auto entryLoc = indexParseLocation(formatName ~ " section table entry", shent.index, tableLoc);
          shent.section = PeSection!nBits.instance();
          sections.pushBack(shent.section); // always attach to AST before parsing
          shent.section.createContent(shent);
        }
      }
    }
  }

  // Memory location for error reporting
  ParseLocation vaLocation(string what, Word!nBits va, ParseLocation parent) {
    auto global = Interval!(Word!nBits).whole;
    return new MapParseLocation!(MemoryMap!(Word!nBits))(what, AddressSpace.MEMORY, memBytes, global, va, "", parent);
  }

  // File location for error reporting
  ParseLocation fileLocation(string what, Word!nBits offset, ParseLocation parent) {
    auto global = Interval!(Word!nBits).whole;
    return new MapParseLocation!(MemoryMap!(Word!nBits))(what, AddressSpace.FILE, fileBytes, global, offset, "", parent);
  }
}


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// COFF file header
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class PeCoffFileHeader(size_t nBits): Ast {
  mixin AstNodeFeatures;
  struct AstChildren {}

  // Microsoft documentation prefixes all these with "IMAGE_FILE_MACHINE_"
  enum MachineType: Word!16 {
    UNKNOWN = 0x0,
    AM33 = 0x1d3,      // Matsushita AM33
    AMD64 = 0x8664,    // x64
    ARM = 0x1c0,       // ARM little endian
    ARM64 = 0xaa64,    // ARM64 little endian
    ARMNT = 0x1c4,     // ARM Thumb-2 little endian
    EBC = 0xebc,       // EFI byte code
    I386 = 0x14c,      // Intel 386 or later processors and compatible processors
    IA64 = 0x200,      // Intel Itanium processor family
    M32R = 0x9041,     // Mitsubishi M32R little endian
    MIPS16 = 0x266,    // MIPS16
    MIPSFPU = 0x366,   // MIPS with FPU
    MIPSFPU16 = 0x466, // MIPS16 with FPU
    POWERPC = 0x1f0,   // Power PC little endian
    POWERPCFP = 0x1f1, // Power PC with floating point support
    R4000 = 0x166,     // MIPS little endian
    RISCV32 = 0x5032,  // RISC-V 32-bit address space
    RISCV64 = 0x5064,  // RISC-V 64-bit address space
    RISCV128 = 0x5128, // RISC-V 128-bit address space
    SH3 = 0x1a2,       // Hitachi SH3
    SH3DSP = 0x1a3,    // Hitachi SH3 DSP
    SH4 = 0x1a6,       // Hitachi SH4
    SH5 = 0x1a8,       // Hitachi SH5
    THUMB = 0x1c2,     // Thumb
    WCEMIPSV2 = 0x169, // MIPS little-endian WCE v2
  }

  // Microsoft documentation prefixes all these with "IMAGE_FILE_"
  enum Characteristics: Word!16 {
    RELOCS_STRIPPED = 0x0001,
    EXECUTABLE_IMAGE = 0x0002,
    LINE_NUMS_STRIPPED = 0x0004,
    LOCAL_SYMS_STRIPPED = 0x0008,
    AGGRESSIVE_WS_TRIM = 0x0010,
    LARGE_ADDRESS_AWARE = 0x0020,
    // reserved 0x0040
    BYTES_REVERSED_LO = 0x0080,
    _32BIT_MACHINE = 0x0100,
    DEBUG_STRIPPED = 0x0200,
    REMOVABLE_RUN_FROM_SWAP = 0x0400,
    NET_RUN_FROM_SWAP = 0x0800,
    SYSTEM = 0x1000,
    DLL = 0x2000,
    UP_SYSTEM_ONLY = 0x4000,
    BYTES_REVERSED_HI = 0x8000
  }

  struct Disk {
    align(1):
    MachineType machine;
    Word!16 numberOfSections;             // number of sections defined in the Section Table
    Word!32 timeDateStamp;                // time and date file was created or modified by the linker
    Word!32 pointerToSymbolTable;         // offset to COFF debugging symbol table; zero for executable images
    Word!32 numberOfSymbols;              // number of symbols in COFF symbol table; zero for executable images
    Word!16 sizeOfOptionalHeader;         // num remaining bytes in the header following the 'flags' field; zero for objects
    BitFlags!Characteristics characteristics;
  }

  Disk disk;
  Interval!(Word!nBits) fileExtent;       // file offset where COFF header would be located
  SysTime timeStamp;                      // same as disk.timeDateStamp

  this() {
    timeStamp = SysTime.fromUnixTime(0); // prevent segfaults in phobos library by explicitly initializing
  }

  void parse(ParseLocation parentLoc) {
    auto fhdr = ancestor!(PeFileHeader!nBits);
    assert(fhdr !is null);

    fileExtent = Interval!(Word!nBits).baseSizeTrunc(cast(Word!nBits)(fhdr.peMagicOffset + 4), disk.sizeof);
    auto loc = fhdr.fileLocation(fhdr.formatName ~ " COFF header", fileExtent.least, parentLoc);

    if (!fhdr.fileBytes.readObjectAt(fileExtent.least, disk))
      appendError(loc, "short read");
    disk = disk.fromLittleEndian;
    timeStamp = SysTime.fromUnixTime(disk.timeDateStamp);

    if (disk.pointerToSymbolTable == 0 && disk.numberOfSymbols != 0)
      appendError(loc, "has no symbol table but non-zero number of symbols (" ~ disk.numberOfSymbols.to!string ~ ")");
    
    if (disk.sizeOfOptionalHeader > 0 && disk.sizeOfOptionalHeader < PeOptionalFileHeader!nBits.Disk.sizeof)
      appendError(loc, "size of optional header (" ~ disk.sizeOfOptionalHeader.to!string ~ ")" ~
                  " is smaller than minimum optional header size (" ~ PeOptionalFileHeader!nBits.Disk.sizeof.to!string ~ ")");

    if (disk.characteristics.nonFlagsSet != 0)
      appendError(loc, "characteristics (" ~ disk.characteristics.to!string ~ ") contain unrecognized bits");
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// PE Optional Header
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class PeOptionalFileHeader(size_t nBits): Ast {
  mixin AstNodeFeatures;
  struct AstChildren {}

  enum PeFormat: Word!16 {
    ROM = 0x0107,                         // "ROM image"
    PE32 = 0x010b,                        // "PE32" ("normal") file format: 32-bit offsets and addresses
    PE32PLUS = 0x020b                     // "PE32+" file format: 64-bit addresses (but still 32-bit file offsets)
  }

  // Microsoft documentation prefixes all these with "IMAGE_SUBSYSTEM_"
  enum Subsystem: Word!16 {
    UNKNOWN = 0,
    NATIVE = 1,         // Device drivers and native Windows processes
    WINDOWS_GUI = 2,    // The Windows graphical user interface (GUI) subsystem
    WINDOWS_CUI = 3,    // The Windows character subsystem
    OS2_CUI = 5,        // The OS/2 character subsystem
    POSIX_CUI = 7,      // The Posix character subsystem
    NATIVE_WINDOWS = 8, // Native Win9x driver
    WINDOWS_CE_GUI =9,  // Windows CE
    EFI_APPLICATION = 10, // An Extensible Firmware Interface (EFI) application
    EFI_BOOT_SERVICE_DRIVER = 11, // An EFI driver with boot services
    EFI_RUNTIME_DRIVER = 12, // An EFI driver with run-time services
    EFI_ROM =13,        // An EFI ROM image
    XBOX = 14,          // XBOX
    WINDOWS_BOOT_APPLICATION = 16 // Windows boot application.
  }

  // Microsoft prefixes all these with "IMAGE_CHARACTERISTICS_"
  enum DllCharacteristics: Word!16 {
    // reserved 0x0001
    // reserved 0x0002
    // reserved 0x0004
    // reserved 0x0008
    HIGH_ENTROPY_VA = 0x0020, // Image can handle a high entropy 64-bit virtual address space
    DYNAMIC_BASE = 0x0040,  // DLL can be relocated at load time
    FORCE_INTEGRITY = 0x0080, // Code Integrity checks are enforced
    NX_COMPAT = 0x0100,       // Image is NX compatible
    NO_ISOLATION = 0x0200,    // Isolation aware, but do not isolate the image
    NO_SEH = 0x0400,          // Does not use structured exception (SE) handling
    NO_BIND = 0x0800,         // Do not bind the image
    APPCONTAINER = 0x1000,    // Image must execute in an AppContainer
    WDM_DRIVER = 0x2000,      // A WDM driver
    GUARD_CF = 0x4000,        // Image supports Control Flow Guard
    TERMINAL_SERVER_AWARE = 0x8000 // Terminal Server aware
  }

  struct Disk {
    align(1):
    // Optional header standard fields
    PeFormat magic;
    Word!8 majorLinkerVersion;
    Word!8 minorLinkerVersion;
    Word!32 sizeOfCode;                   // sum of sizes of all code (text) sections
    Word!32 sizeOfInitializedData;        // sum of sizes of all initialized data sections
    Word!32 sizeOfUninitializeData;       // sum of sizes of all uninitialized data (bss) sections
    Word!32 entryPointRva;                // address of entry point or zero
    Word!32 baseOfCode;                   // RVA of beginning of code section
    static if (32 == nBits)
      Word!32 baseOfData;                 // RVA of beginning of data section

    // Optional header Windows-specific fields
    Word!nBits imageBase;                 // preferred VA of first byte of image
    Word!32 sectionAlignment;             // in bytes, >= file alignment; defaults to OS page size
    Word!32 fileAlignment;                // in bytes, n must be power of 2 s.t. 512<=n<=64kB
    Word!16 majorOsVersion;
    Word!16 minorOsVersion;
    Word!16 majorImageVersion;
    Word!16 minorImageVersion;
    Word!16 majorSubsystemVersion;
    Word!16 minorSubsystemVersion;
    Word!32 win32VersionNumber;           // reserved; must be zero
    Word!32 sizeOfImage;                  // memory size in bytes of image w/all sections; multiple of sectionAlignment
    Word!32 sizeOfHeaders;                // sizeof DOS, PE header, section headers rounded up to multiple of fileAlignment
    Word!32 checkSum;                     // checked for certain critical images
    Subsystem subsystem;                  // subsystem required to run this image
    BitFlags!DllCharacteristics dllCharacteristics;
    Word!nBits sizeOfStackReserve;        // size of stack to reserve in the virtual address space
    Word!nBits sizeOfStackCommit;         // part of sizeOfStackReserve that is actually mapped
    Word!nBits sizeOfHeapReserve;         // size of heap to reserve in the virtual address space
    Word!nBits sizeOfHeapCommit;          // part of sizeOfHeapReserve that is actually mapped
    Word!32 loaderFlags;                  // reserved; must be zero
    Word!32 numberOfRvaAndSizes;          // num. data directory entries following this OptionalHeader
  }

  Disk disk;
  Interval!(Word!nBits) fileExtent; // file offsets for optional header, excluding data directory

  void parse(ParseLocation parentLoc) {
    auto fhdr = ancestor!(PeFileHeader!nBits);
    assert(fhdr !is null);
    assert(fhdr.coffHeader !is null);

    if (fhdr.coffHeader.disk.sizeOfOptionalHeader > 0) {
      fileExtent = Interval!(Word!nBits).baseSizeTrunc(cast(Word!nBits)(fhdr.coffHeader.fileExtent.greatest + 1), disk.sizeof);
      auto loc = fhdr.fileLocation(fhdr.formatName ~ " \"optional\" header", fileExtent.least, parentLoc);

      if (!fhdr.fileBytes.readObjectAt(fileExtent.least, disk))
        appendError(loc, "short read");
      disk = disk.fromLittleEndian;

      switch (disk.magic) {
        case PeFormat.ROM:
          appendError(loc, "ROM images are not supported");
          break;
        case PeFormat.PE32:
          if (nBits != 32)
            appendError(loc, "wrong optional header magic number (" ~ disk.magic.hexStr ~ ") for a PE32 file");
          break;
        case PeFormat.PE32PLUS:
          if (nBits != 64)
            appendError(loc, "wrong optional header magic number (" ~ disk.magic.hexStr ~ ") for a PE32+ file");
          break;
        default:
          appendError(loc, "invalid optional header magic (" ~ disk.magic.hexStr ~ ")");
          break;
      }

      if (disk.imageBase % (64*1024) != 0)
        appendError(loc, "image base (" ~ disk.imageBase.hexStr ~ ")" ~ " is not a multiple of 0x10000");

      if (disk.sectionAlignment < disk.fileAlignment)
        appendError(loc, "section alignment (" ~ disk.sectionAlignment.hexStr ~ ")" ~
                    " is less than the file alignment (" ~ disk.fileAlignment.hexStr ~ ")");

      if (disk.fileAlignment != 0 && popcnt(disk.fileAlignment) != 1) {
        appendError(loc, "file alignment (" ~ disk.fileAlignment.hexStr ~ ")" ~ " is not a power of two");
        disk.sectionAlignment = 4096; // take a guess
      }

      if (disk.fileAlignment != 0 && (disk.fileAlignment < 512 || disk.fileAlignment > 64*1024)) {
        appendError(loc, "file alignment (" ~ disk.fileAlignment.hexStr ~ ")" ~ " is outside range [512, 64*1024]");
        disk.fileAlignment = 4096; // take a guess
      }

      if (disk.win32VersionNumber != 0)
        appendError(loc, "win32 version (" ~ disk.win32VersionNumber.to!string ~ ")" ~ " should be zero");

      if (disk.sizeOfImage % disk.sectionAlignment != 0)
        appendError(loc, "size of image (" ~ disk.sizeOfImage.hexStr ~ ")" ~
                    " is not a multiple of section alignment (" ~ disk.sectionAlignment.hexStr ~ ")");

      if (disk.sizeOfHeaders % disk.fileAlignment != 0)
        appendError(loc, "size of headers (" ~ disk.sizeOfHeaders.hexStr ~ ")" ~
                    " is not a multiple of file alignment (" ~ disk.fileAlignment.hexStr ~ ")");

      if (disk.dllCharacteristics.nonFlagsSet != 0)
        appendError(loc, "dll characterstics (" ~ disk.dllCharacteristics.to!string ~ ") contain unrecognized bits");
      
      if (disk.loaderFlags != 0)
        appendError(loc, "loader flags (" ~ disk.loaderFlags.to!string ~ ")" ~ " is not zero");

      size_t dataDirectorySize = disk.numberOfRvaAndSizes * PeDataDirectoryEntry!nBits.Disk.sizeof;
      if (Disk.sizeof + dataDirectorySize != fhdr.coffHeader.disk.sizeOfOptionalHeader)
        appendError(loc, "number of data directory entries " ~ " (" ~ disk.numberOfRvaAndSizes.to!string ~ ")" ~
                    " is not consistent with COFF header sizeOfOptionalHeader" ~
                    " (" ~ fhdr.coffHeader.disk.sizeOfOptionalHeader.to!string ~ ")");
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// PE Optional Header Data Directory Table
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class PeDataDirectory(size_t nBits): Ast {
  mixin AstNodeFeatures;
  struct AstChildren {
    AstList!(PeDataDirectoryEntry!nBits) entries;
  }

  enum DataDirType {                      // names for the data directory entries, by index
    EXPORT_TABLE,
    IMPORT_TABLE,
    RESOURCE_TABLE,
    EXCEPTION_TABLE,
    CERTIFICATE_TABLE,
    BASE_RELOC_TABLE,
    DEBUG,
    ARCHITECTURE,
    GLOBAL_PTR,
    TLS_TABLE,
    LOAD_CONFIG_TABLE,
    BOUND_IMPORT_TABLE,
    IMPORT_ADDRESS_TABLE,
    DELAY_IMPORT_DESCRIPTOR,
    CLR_RUNTIME_HEADER,
    RESERVED_15
  }

  Interval!(Word!nBits) fileExtent; // location of this entire table within the file

  this() {
    entries = new AstList!(PeDataDirectoryEntry!nBits);
  }

  void parse(ParseLocation parentLoc) {
    auto fhdr = ancestor!(PeFileHeader!nBits);
    assert(fhdr !is null);
    assert(fhdr.optionalHeader !is null);

    auto dataDirectoryOffset = cast(Word!nBits)(fhdr.optionalHeader.fileExtent.greatest + 1);
    size_t dataDirectorySize = fhdr.optionalHeader.disk.numberOfRvaAndSizes * PeDataDirectoryEntry!nBits.Disk.sizeof;
    fileExtent = Interval!(Word!nBits).baseSizeTrunc(dataDirectoryOffset, dataDirectorySize);
    auto loc = fhdr.fileLocation(fhdr.formatName ~ " header data directory", fileExtent.least, parentLoc);

    if (fhdr.optionalHeader.disk.numberOfRvaAndSizes > 0) {
      foreach (i; 0 .. fhdr.optionalHeader.disk.numberOfRvaAndSizes) {
        auto entry = PeDataDirectoryEntry!nBits.instance();
        entries.pushBack(entry);
        auto entryLoc = indexParseLocation(fhdr.formatName ~ "header data directory entry", i, loc);
        entry.parse(entryLoc);
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// PE Optional header data directory table entry
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class PeDataDirectoryEntry(size_t nBits): Ast {
  mixin AstNodeFeatures;
  struct AstChildren {}
  
  struct Disk {
    align(1):
    Word!32 rva;
    Word!32 nBytes;
  }

  Disk disk;
  size_t index; // index of this entry in the data directory parent
  Interval!(Word!nBits) fileExtent; // location of this data directory entry in the file

  void parse(ParseLocation parentLoc) {
    auto fhdr = ancestor!(PeFileHeader!nBits);
    assert(fhdr !is null);

    auto dir = ancestor!(PeDataDirectory!nBits);
    assert(dir !is null);
    assert(dir.entries.back is this);
    index = dir.entries.length - 1;

    auto offset = cast(Word!nBits)(dir.fileExtent.least + index * Disk.sizeof);
    fileExtent = Interval!(Word!nBits).baseSizeTrunc(offset, disk.sizeof);
    auto loc = fhdr.fileLocation(fhdr.formatName ~ " header data directory entry", offset, parentLoc);

    if (!fhdr.fileBytes.readObjectAt(offset, disk))
      appendError(loc, "short read");
    disk = disk.fromLittleEndian;
  }

  // Value of directory entry as an interval using RVAs.
  Interval!(Word!nBits) rvaExtent() {
    return Interval!(Word!nBits).baseSizeTrunc(disk.rva, disk.nBytes);
  }

  // Value of directory entry as an invertval converted to VAs.
  Interval!(Word!nBits) vaExtent() {
    auto fhdr = ancestor!(PeFileHeader!nBits);
    assert(fhdr !is null);
    return Interval!(Word!nBits).baseSizeTrunc(fhdr.va(disk.rva), disk.nBytes);
  }
}
