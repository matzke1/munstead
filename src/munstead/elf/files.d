module munstead.elf.files;

import munstead.ast.base;
import munstead.ast.files;
import munstead.ast.sections;
import munstead.core.byteorder;
import munstead.core.exception;
import munstead.core.interval;
import munstead.core.mmap;
import munstead.core.util;
import munstead.core.wordtypes;
import munstead.elf.dynamic;
import munstead.elf.interp;
import munstead.elf.notes;
import munstead.elf.loader;
import munstead.elf.reloc;
import munstead.elf.sections;
import munstead.elf.segments;
import munstead.elf.strings;
import munstead.elf.symbols;
import munstead.elf.types;
import std.algorithm: equal, filter, fold, map;
import std.conv: to;
import std.typecons: Tuple, tuple;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Base class for ELF files
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class ElfFile: AsmFile {
  mixin AstNodeFeatures;
  struct AstChildren {}
}
    

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// ELF file header
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class ElfFileHeader(size_t nBits): ElfFile {
  mixin AstNodeFeatures;

  struct AstChildren {
    ElfSegmentTable!nBits segmentTable; // i.e., "program header table"
    ElfSectionTable!nBits sectionTable;

    // All sections whether created because they belong to a section table, segment table, or other reason.
    AstList!(ElfSection!nBits) sections;
  }

  static if (nBits == 32) {
    enum formatName = "ELF32";
  } else {
    static assert(nBits == 64);
    enum formatName = "ELF64";
  }

  enum FileClass: ubyte { ELFCLASSNONE = 0, ELFCLASS32 = 1, ELFCLASS64 = 2 };

  enum DataEncoding: ubyte { ELFDATANONE = 0, ELFDATA2LSB = 1, ELFDATA2MSB = 2 };

  enum OsAbi: ubyte {
    ELFOSABI_NONE=0, ELFOSABI_HPUX=1, ELFOSABI_NETBSD=2, ELFOSABI_GNU=3, ELFOSABI_LINUX=3, ELFOSABI_SOLARIS=6,
    ELFOSABI_AIX=7, ELFOSABI_IRIX=8, ELFOSABI_FREEBSD=9, ELFOSABI_TRU64=10, ELFOSABI_MODESTO=11, ELFOSABI_OPENBSD=12,
    ELFOSABI_OPENVMS=13, ELFOSABI_NSK=14, ELFOSABI_AROS=15, ELFOSABI_FENIXOS=16, ELFOSABI_CLOUDABI=17,
    ELFOSABI_OPENVOS=18
  }

  enum ObjectType: Elf!nBits.Half { ET_NONE = 0, ET_REL  = 1, ET_EXEC = 2, ET_DYN = 3, ET_CORE = 4 };

  enum Machine: Elf!nBits.Half {
    EM_NONE         = 0,    // No machine
    EM_M32          = 1,    // AT&T WE 32100
    EM_SPARC        = 2,    // SPARC
    EM_386          = 3,    // Intel 80386
    EM_68K          = 4,    // Motorola 68000
    EM_88K          = 5,    // Motorola 88000
    EM_IAMCU        = 6,    // Intel MCU
    EM_860          = 7,    // Intel 80860
    EM_MIPS         = 8,    // MIPS I Architecture
    EM_S370         = 9,    // IBM System/370 Processor
    EM_MIPS_RS3_LE  = 10,   // MIPS RS3000 Little-endian
    EM_PARISC       = 15,   // Hewlett-Packard PA-RISC
    EM_VPP500       = 17,   // Fujitsu VPP500
    EM_SPARC32PLUS  = 18,   // Enhanced instruction set SPARC
    EM_960          = 19,   // Intel 80960
    EM_PPC          = 20,   // PowerPC
    EM_PPC64        = 21,   // 64-bit PowerPC
    EM_S390         = 22,   // IBM System/390 Processor
    EM_SPU          = 23,   // IBM SPU/SPC
    EM_V800         = 36,   // NEC V800
    EM_FR20         = 37,   // Fujitsu FR20
    EM_RH32         = 38,   // TRW RH-32
    EM_RCE          = 39,   // Motorola RCE
    EM_ARM          = 40,   // ARM 32-bit architecture (AARCH32)
    EM_ALPHA        = 41,   // Digital Alpha
    EM_SH           = 42,   // Hitachi SH
    EM_SPARCV9      = 43,   // SPARC Version 9
    EM_TRICORE      = 44,   // Siemens TriCore embedded processor
    EM_ARC          = 45,   // Argonaut RISC Core, Argonaut Technologies Inc.
    EM_H8_300       = 46,   // Hitachi H8/300
    EM_H8_300H      = 47,   // Hitachi H8/300H
    EM_H8S          = 48,   // Hitachi H8S
    EM_H8_500       = 49,   // Hitachi H8/500
    EM_IA_64        = 50,   // Intel IA-64 processor architecture
    EM_MIPS_X       = 51,   // Stanford MIPS-X
    EM_COLDFIRE     = 52,   // Motorola ColdFire
    EM_68HC12       = 53,   // Motorola M68HC12
    EM_MMA          = 54,   // Fujitsu MMA Multimedia Accelerator
    EM_PCP          = 55,   // Siemens PCP
    EM_NCPU         = 56,   // Sony nCPU embedded RISC processor
    EM_NDR1         = 57,   // Denso NDR1 microprocessor
    EM_STARCORE     = 58,   // Motorola Star*Core processor
    EM_ME16         = 59,   // Toyota ME16 processor
    EM_ST100        = 60,   // STMicroelectronics ST100 processor
    EM_TINYJ        = 61,   // Advanced Logic Corp. TinyJ embedded processor family
    EM_X86_64       = 62,   // AMD x86-64 architecture
    EM_PDSP         = 63,   // Sony DSP Processor
    EM_PDP10        = 64,   // Digital Equipment Corp. PDP-10
    EM_PDP11        = 65,   // Digital Equipment Corp. PDP-11
    EM_FX66         = 66,   // Siemens FX66 microcontroller
    EM_ST9PLUS      = 67,   // STMicroelectronics ST9+ 8/16 bit microcontroller
    EM_ST7          = 68,   // STMicroelectronics ST7 8-bit microcontroller
    EM_68HC16       = 69,   // Motorola MC68HC16 Microcontroller
    EM_68HC11       = 70,   // Motorola MC68HC11 Microcontroller
    EM_68HC08       = 71,   // Motorola MC68HC08 Microcontroller
    EM_68HC05       = 72,   // Motorola MC68HC05 Microcontroller
    EM_SVX          = 73,   // Silicon Graphics SVx
    EM_ST19         = 74,   // STMicroelectronics ST19 8-bit microcontroller
    EM_VAX          = 75,   // Digital VAX
    EM_CRIS         = 76,   // Axis Communications 32-bit embedded processor
    EM_JAVELIN      = 77,   // Infineon Technologies 32-bit embedded processor
    EM_FIREPATH     = 78,   // Element 14 64-bit DSP Processor
    EM_ZSP          = 79,   // LSI Logic 16-bit DSP Processor
    EM_MMIX         = 80,   // Donald Knuth's educational 64-bit processor
    EM_HUANY        = 81,   // Harvard University machine-independent object files
    EM_PRISM        = 82,   // SiTera Prism
    EM_AVR          = 83,   // Atmel AVR 8-bit microcontroller
    EM_FR30         = 84,   // Fujitsu FR30
    EM_D10V         = 85,   // Mitsubishi D10V
    EM_D30V         = 86,   // Mitsubishi D30V
    EM_V850         = 87,   // NEC v850
    EM_M32R         = 88,   // Mitsubishi M32R
    EM_MN10300      = 89,   // Matsushita MN10300
    EM_MN10200      = 90,   // Matsushita MN10200
    EM_PJ           = 91,   // picoJava
    EM_OPENRISC     = 92,   // OpenRISC 32-bit embedded processor
    EM_ARC_COMPACT  = 93,   // ARC International ARCompact processor (old spelling/synonym: EM_ARC_A5)
    EM_XTENSA       = 94,   // Tensilica Xtensa Architecture
    EM_VIDEOCORE    = 95,   // Alphamosaic VideoCore processor
    EM_TMM_GPP      = 96,   // Thompson Multimedia General Purpose Processor
    EM_NS32K        = 97,   // National Semiconductor 32000 series
    EM_TPC          = 98,   // Tenor Network TPC processor
    EM_SNP1K        = 99,   // Trebia SNP 1000 processor
    EM_ST200        = 100,  // STMicroelectronics (www.st.com) ST200 microcontroller
    EM_IP2K         = 101,  // Ubicom IP2xxx microcontroller family
    EM_MAX          = 102,  // MAX Processor
    EM_CR           = 103,  // National Semiconductor CompactRISC microprocessor
    EM_F2MC16       = 104,  // Fujitsu F2MC16
    EM_MSP430       = 105,  // Texas Instruments embedded microcontroller msp430
    EM_BLACKFIN     = 106,  // Analog Devices Blackfin (DSP) processor
    EM_SE_C33       = 107,  // S1C33 Family of Seiko Epson processors
    EM_SEP          = 108,  // Sharp embedded microprocessor
    EM_ARCA         = 109,  // Arca RISC Microprocessor
    EM_UNICORE      = 110,  // Microprocessor series from PKU-Unity Ltd. and MPRC of Peking University
    EM_EXCESS       = 111,  // eXcess: 16/32/64-bit configurable embedded CPU
    EM_DXP          = 112,  // Icera Semiconductor Inc. Deep Execution Processor
    EM_ALTERA_NIOS2 = 113,  // Altera Nios II soft-core processor
    EM_CRX          = 114,  // National Semiconductor CompactRISC CRX microprocessor
    EM_XGATE        = 115,  // Motorola XGATE embedded processor
    EM_C166         = 116,  // Infineon C16x/XC16x processor
    EM_M16C         = 117,  // Renesas M16C series microprocessors
    EM_DSPIC30F     = 118,  // Microchip Technology dsPIC30F Digital Signal Controller
    EM_CE           = 119,  // Freescale Communication Engine RISC core
    EM_M32C         = 120,  // Renesas M32C series microprocessors
    EM_TSK3000      = 131,  // Altium TSK3000 core
    EM_RS08         = 132,  // Freescale RS08 embedded processor
    EM_SHARC        = 133,  // Analog Devices SHARC family of 32-bit DSP processors
    EM_ECOG2        = 134,  // Cyan Technology eCOG2 microprocessor
    EM_SCORE7       = 135,  // Sunplus S+core7 RISC processor
    EM_DSP24        = 136,  // New Japan Radio (NJR) 24-bit DSP Processor
    EM_VIDEOCORE3   = 137,  // Broadcom VideoCore III processor
    EM_LATTICEMICO32= 138,  // RISC processor for Lattice FPGA architecture
    EM_SE_C17       = 139,  // Seiko Epson C17 family
    EM_TI_C6000     = 140,  // The Texas Instruments TMS320C6000 DSP family
    EM_TI_C2000     = 141,  // The Texas Instruments TMS320C2000 DSP family
    EM_TI_C5500     = 142,  // The Texas Instruments TMS320C55x DSP family
    EM_TI_ARP32     = 143,  // Texas Instruments Application Specific RISC Processor, 32bit fetch
    EM_TI_PRU       = 144,  // Texas Instruments Programmable Realtime Unit
    EM_MMDSP_PLUS   = 160,  // STMicroelectronics 64bit VLIW Data Signal Processor
    EM_CYPRESS_M8C  = 161,  // Cypress M8C microprocessor
    EM_R32C         = 162,  // Renesas R32C series microprocessors
    EM_TRIMEDIA     = 163,  // NXP Semiconductors TriMedia architecture family
    EM_QDSP6        = 164,  // QUALCOMM DSP6 Processor
    EM_8051         = 165,  // Intel 8051 and variants
    EM_STXP7X       = 166,  // STMicroelectronics STxP7x family of configurable and extensible RISC processors
    EM_NDS32        = 167,  // Andes Technology compact code size embedded RISC processor family
    EM_ECOG1        = 168,  // Cyan Technology eCOG1X family
    EM_ECOG1X       = 168,  // Cyan Technology eCOG1X family
    EM_MAXQ30       = 169,  // Dallas Semiconductor MAXQ30 Core Micro-controllers
    EM_XIMO16       = 170,  // New Japan Radio (NJR) 16-bit DSP Processor
    EM_MANIK        = 171,  // M2000 Reconfigurable RISC Microprocessor
    EM_CRAYNV2      = 172,  // Cray Inc. NV2 vector architecture
    EM_RX           = 173,  // Renesas RX family
    EM_METAG        = 174,  // Imagination Technologies META processor architecture
    EM_MCST_ELBRUS  = 175,  // MCST Elbrus general purpose hardware architecture
    EM_ECOG16       = 176,  // Cyan Technology eCOG16 family
    EM_CR16         = 177,  // National Semiconductor CompactRISC CR16 16-bit microprocessor
    EM_ETPU         = 178,  // Freescale Extended Time Processing Unit
    EM_SLE9X        = 179,  // Infineon Technologies SLE9X core
    EM_L10M         = 180,  // Intel L10M
    EM_K10M         = 181,  // Intel K10M
    EM_AARCH64      = 183,  // ARM 64-bit architecture (AARCH64)
    EM_AVR32        = 185,  // Atmel Corporation 32-bit microprocessor family
    EM_STM8         = 186,  // STMicroeletronics STM8 8-bit microcontroller
    EM_TILE64       = 187,  // Tilera TILE64 multicore architecture family
    EM_TILEPRO      = 188,  // Tilera TILEPro multicore architecture family
    EM_MICROBLAZE   = 189,  // Xilinx MicroBlaze 32-bit RISC soft processor core
    EM_CUDA         = 190,  // NVIDIA CUDA architecture
    EM_TILEGX       = 191,  // Tilera TILE-Gx multicore architecture family
    EM_CLOUDSHIELD  = 192,  // CloudShield architecture family
    EM_COREA_1ST    = 193,  // KIPO-KAIST Core-A 1st generation processor family
    EM_COREA_2ND    = 194,  // KIPO-KAIST Core-A 2nd generation processor family
    EM_ARC_COMPACT2 = 195,  // Synopsys ARCompact V2
    EM_OPEN8        = 196,  // Open8 8-bit RISC soft processor core
    EM_RL78         = 197,  // Renesas RL78 family
    EM_VIDEOCORE5   = 198,  // Broadcom VideoCore V processor
    EM_78KOR        = 199,  // Renesas 78KOR family
    EM_56800EX      = 200,  // Freescale 56800EX Digital Signal Controller (DSC)
    EM_BA1          = 201,  // Beyond BA1 CPU architecture
    EM_BA2          = 202,  // Beyond BA2 CPU architecture
    EM_XCORE        = 203,  // XMOS xCORE processor family
    EM_MCHP_PIC     = 204,  // Microchip 8-bit PIC(r) family
    EM_INTEL205     = 205,  // Reserved by Intel
    EM_INTEL206     = 206,  // Reserved by Intel
    EM_INTEL207     = 207,  // Reserved by Intel
    EM_INTEL208     = 208,  // Reserved by Intel
    EM_INTEL209     = 209,  // Reserved by Intel
    EM_KM32         = 210,  // KM211 KM32 32-bit processor
    EM_KMX32        = 211,  // KM211 KMX32 32-bit processor
    EM_KMX16        = 212,  // KM211 KMX16 16-bit processor
    EM_KMX8         = 213,  // KM211 KMX8 8-bit processor
    EM_KVARC        = 214,  // KM211 KVARC processor
    EM_CDP          = 215,  // Paneve CDP architecture family
    EM_COGE         = 216,  // Cognitive Smart Memory Processor
    EM_COOL         = 217,  // Bluechip Systems CoolEngine
    EM_NORC         = 218,  // Nanoradio Optimized RISC
    EM_CSR_KALIMBA  = 219,  // CSR Kalimba architecture family
    EM_Z80          = 220,  // Zilog Z80
    EM_VISIUM       = 221,  // Controls and Data Services VISIUMcore processor
    EM_FT32         = 222,  // FTDI Chip FT32 high performance 32-bit RISC architecture
    EM_MOXIE        = 223,  // Moxie processor family
    EM_AMDGPU       = 224,  // AMD GPU architecture
    EM_RISCV        = 243   // RISC-V
  }

  // Disk format for a file header
  struct Disk {
    align(1):
    ubyte[4] e_ident_magic;             // magic number: 0x7f, 'E', 'L', 'F'
    FileClass e_ident_file_class;       // 32- or 64-bit format
    DataEncoding e_ident_data_encoding; // byte order
    ubyte e_ident_file_version;         // format version number (same as e_version) must be 1
    OsAbi e_ident_osabi;                // operating system / ABI identification
    ubyte e_ident_abiversion;           // application binary interface version number
    ubyte[7] pad1;                      // zero padding
    ObjectType e_type;                  // object file type: relocatable, executable, library, core dump
    Machine e_machine;                  // required architecture for an individual file
    Elf!nBits.Word e_version;           // object file version, currently zero or one
    Elf!nBits.Addr e_entry;             // entry virtual address or zero if none
    Elf!nBits.Off e_phoff;              // file offset of program header table or zero if none
    Elf!nBits.Off e_shoff;              // file offset of secton header table or zero if none
    Elf!nBits.Word e_flags;             // processor-specific flags
    Elf!nBits.Half e_ehsize;            // size of ELF header in bytes
    Elf!nBits.Half e_phentsize;         // size of each entry in the program header table
    Elf!nBits.Half e_phnum;             // number of program headers
    Elf!nBits.Half e_shentsize;         // size of each entry in the section header table
    Elf!nBits.Half e_shnum;             // number of section headers, or zero for extended entries
    Elf!nBits.Half e_shstrndx;          // index of name section, or SHN_UNDEF or SHN_XINDEX
  }

  Disk disk;                             // file header disk format, in native byte order
  MemoryMap!(Word!nBits) fileBytes;      // content of the entire file
  MemoryMap!(Word!nBits) memBytes;       // memory mapping using preferred addresses
  ElfSegmentLoader!nBits segmentLoader;  // loads segments into the memBytes map
  ElfSectionLoader!nBits sectionLoader;  // loads sections on top of segments in the memBytes map

  this() {
    sections = new AstList!(ElfSection!nBits);
  }

  // Before parsing, you must assign a map to the "fileBytes" member. This map should contain the file contents to be
  // parsed starting at offset zero.
  void parse(ParseLocation parentLoc) {
    auto loc = fileLocation("", 0, parentLoc);
    parseHeader(loc);
    parseSegments(loc);
    loadSegments();
    parseSections(loc);
    loadSections();
    updatePermissions();
    parseDynamic(loc);          // must be after segments and sections are mapped into virtual memory
  }

  // Alternative parsing method where the user supplies a file name.
  void parseFile(string fileName) {
    fileBytes = new MemoryMap!(Word!nBits);
    fileBytes.insertFile(fileName, 0);
    parse(null);
  }

  // True if the file looks like it's probably an ELF file
  static bool testFile(string fileName) {
    auto fhdr = ElfFileHeader.instance();
    fhdr.fileBytes = new MemoryMap!(Word!nBits);
    fhdr.fileBytes.insertFile(fileName, 0);
    try {
      auto loc = fhdr.fileLocation("", 0, null);
      fhdr.parseHeader(loc);
      return fhdr.preOrder.map!(node => node.errors.length).fold!"a+b"(0uL) == 0;
    } catch {
      return false;
    }
  }

  // Parse just the ELF file header, nothing else
  void parseHeader(ParseLocation parentLoc) {
    auto loc = fileLocation(formatName ~ " header", 0, parentLoc);

    // Read the header
    if (!fileBytes.readObjectAt(0, disk))
      appendError(loc, "short read");
    
    // Check magic number
    if (!disk.e_ident_magic[].equal([127, 'E', 'L', 'F']))
      appendError(loc, "incorrect magic number (" ~ disk.e_ident_magic.to!string ~ ")");
    
    // Check file class
    FileClass expectedFileClass = (32 == nBits ? FileClass.ELFCLASS32 : FileClass.ELFCLASS64);
    if (disk.e_ident_file_class != expectedFileClass)
      appendError(loc, "incorrect file class (" ~ disk.e_ident_file_class.to!string ~")");
    
    // Decode file header
    switch (disk.e_ident_data_encoding) {
      case DataEncoding.ELFDATA2LSB:
        byteOrder = ByteOrder.LITTLE_ENDIAN;
        disk = fromLittleEndian(disk);
        break;
      case DataEncoding.ELFDATA2MSB:
        byteOrder = ByteOrder.BIG_ENDIAN;
        disk = fromBigEndian(disk);
        break;
      default:
        appendError(loc, "incorrect data encoding (" ~ disk.e_ident_data_encoding.to!string ~ ")");
        break;
    }

    // Check ident stuff
    if (disk.e_ident_file_version != 1)
      appendError(loc, "incorrect file version (" ~ disk.e_ident_file_version.to!string ~ ")");

    if (!isEnumMember!OsAbi(disk.e_ident_osabi))
      appendError(loc, "unrecognized OS ABI (" ~ disk.e_ident_osabi.to!string ~ ")");
    
    if (disk.e_version != 0 && disk.e_version != 1)
      appendError(loc, "incorrect version (" ~ disk.e_version.to!string ~ ")");
  }

  void parseSegments(ParseLocation parentLoc) {
    auto loc = fileLocation(formatName ~ " segment (program header) table", disk.e_phoff, parentLoc);
    parseSegmentTable(loc);
    createInterpSegments(loc);
    createNoteSegments(loc);
    createDynamicSegments(loc);
    createGenericSegments(loc);
  }

  // Create a loader (if the user hasn't already) and use it to load some segments into the memBytes map
  void loadSegments() {
    if (segmentLoader is null)
      segmentLoader = new ElfSegmentLoader!nBits;
    if (memBytes is null)
      memBytes = new MemoryMap!(Word!nBits);
    segmentLoader.loadSegments(memBytes, this);
  }
    
  void parseSections(ParseLocation parentLoc) {
    auto loc = fileLocation(formatName ~ " section table", disk.e_shoff, parentLoc);

    // Preparation
    parseSectionTable(loc);                      // parse the section table entries
    createStringSections(loc);                   // many sections have strings, so build string tables early
    nameSectionTableEntries(loc);                // names come from a string table

    // Create sections
    createRelocSections(loc);
    createInterpSections(loc);
    createSymbolSectionIndexSections(loc);
    createSymbolSections(loc);
    createNoteSections(loc);
    createDynamicSections(loc);
    createGenericSections(loc);

    // sh_link and sh_info can be done now that all sections are created
    setSectionLinkInfo(loc);
  }

  // Create a loader (if the user hasn't already) and use it to load some sections into the memBytes map.
  void loadSections() {
    if (sectionLoader is null)
      sectionLoader = new ElfSectionLoader!nBits;
    assert(memBytes !is null);
    sectionLoader.loadSections(memBytes, this);
  }

  // Re-precess the segment table to change permissions if necessary
  void updatePermissions() {
    if (segmentLoader is null)
      segmentLoader = new ElfSegmentLoader!nBits;
    assert(memBytes !is null);
    segmentLoader.updatePermissions(memBytes, this);
  }
    
  // (Re)initializes the segmentTable by parsing it from the file.
  void parseSegmentTable(ParseLocation parentLoc) {
    segmentTable = ElfSegmentTable!nBits.instance();
    if (disk.e_phoff > 0)
      segmentTable.parse(parentLoc);
  }

  // (Re)initializes the sectionTable by parsing it from the file.
  void parseSectionTable(ParseLocation parentLoc) {
    sectionTable = ElfSectionTable!nBits.instance();
    if (disk.e_shoff > 0)
      sectionTable.parse(parentLoc);
  }

  // Find a section already registered for this file and having the same file and preferred memory extents
  Section findExistingSection(Section)(Interval!(Word!nBits) fileExtent, Interval!(Word!nBits) memExtent)
  if (is(Section : ElfSection!nBits)) {
    foreach (section; sections[].map!(a => cast(Section) a).filter!"a !is null") {
      if (fileExtent == section.fileExtent && memExtent == section.preferredExtent)
        return section;
    }
    return null;
  }
      
  // Find a section already registered for this file that would be appropriate for the specified program header, or else
  // create such a section and register it.
  void findOrCreateSection(Section)(ElfSegmentTableEntry!nBits phent, ParseLocation parentLoc)
  if (is(Section : ElfSection!nBits)) {
    assert(phent !is null);
    assert(phent.section is null);

    // Look for an existing section
    if (auto found = findExistingSection!Section(phent.fileExtent, phent.preferredExtent)) {
      phent.section = found;
    } else {
      // Or create and register a new one
      auto created = Section.instance();
      sections.pushBack(created);
      created.createContent(phent);
      created.parse(parentLoc);
      phent.section = created;
    }
  }

  // Find a section already register for this file that would be appropriate for the specified section header, or else
  // create such a section and register it.
  void findOrCreateSection(Section)(ElfSectionTableEntry!nBits shent, ParseLocation parentLoc)
  if (is(Section : ElfSection!nBits)) {
    assert(shent !is null);
    assert(shent.section is null);

    // Look for existing section, and perhaps update some info (e.g., a section created from the ELF program header
    // (segment) table won't have names, but the corresponding entry in the ELF section table probably will have a
    // name).
    if (auto found = findExistingSection!Section(shent.fileExtent, shent.preferredExtent)) {
      if (found.name == "")
        found.name = shent.name;
      shent.section = found;
    } else {
      // Or create and register a new one
      auto created = Section.instance();
      shent.section = created;
      sections.pushBack(created);
      created.createContent(shent);
      created.parse(parentLoc);
    }
  }
    
  // ElfInterpSection from PT_INTERP program headers
  void createInterpSegments(ParseLocation parentLoc) {
    if (segmentTable !is null) {
      foreach (phent; segmentTable.entries[]) {
        if (phent.disk.p_type == phent.SegmentType.PT_INTERP)
          findOrCreateSection!(ElfInterpSection!nBits)(phent, parentLoc);
      }
    }
  }

  // ElfInterpSection from named section headers
  void createInterpSections(ParseLocation parentLoc) {
    if (sectionTable !is null) {
      foreach (shent; sectionTable.entries[]) {
        if (shent.name == ".interp") {
          findOrCreateSection!(ElfInterpSection!nBits)(shent, parentLoc);
        }
      }
    }
  }

  // ElfStringTable from SHT_STRTAB section headers.
  void createStringSections(ParseLocation parentLoc) {
    if (sectionTable !is null) {
      foreach (shent; sectionTable.entries[]) {
        if (shent.disk.sh_type == shent.SectionType.SHT_STRTAB)
          findOrCreateSection!(ElfStringTable!nBits)(shent, parentLoc);
      }
    }
  }

  // ElfSymbolSectionIndexTable from SHT_SYMTAB_SHNDX section headers
  void createSymbolSectionIndexSections(ParseLocation parentLoc) {
    if (sectionTable !is null) {
      foreach (shent; sectionTable.entries[]) {
        if (shent.disk.sh_type == shent.SectionType.SHT_SYMTAB_SHNDX)
          findOrCreateSection!(ElfSymbolSectionIndexTable!nBits)(shent, parentLoc);
      }
    }
  }
        
  // ElfSymbolTable from SHT_SYMTAB or SHT_DYNSYM section headers
  void createSymbolSections(ParseLocation parentLoc) {
    if (sectionTable !is null) {
      foreach (shent; sectionTable.entries[]) {
        if (shent.disk.sh_type == shent.SectionType.SHT_SYMTAB || shent.disk.sh_type == shent.SectionType.SHT_DYNSYM)
          findOrCreateSection!(ElfSymbolTable!nBits)(shent, parentLoc);
      }
    }
  }

  // ElfNoteSection from SHT_NOTE section headers
  void createNoteSections(ParseLocation parentLoc) {
    if (sectionTable !is null) {
      foreach (shent; sectionTable.entries[]) {
        if (shent.disk.sh_type == shent.SectionType.SHT_NOTE)
          findOrCreateSection!(ElfNoteSection!nBits)(shent, parentLoc);
      }
    }
  }

  // ElfNoteSection from PT_NOTE program headers
  void createNoteSegments(ParseLocation parentLoc) {
    if (segmentTable !is null) {
      foreach (phent; segmentTable.entries[]) {
        if (phent.disk.p_type == phent.SegmentType.PT_NOTE)
          findOrCreateSection!(ElfNoteSection!nBits)(phent, parentLoc);
      }
    }
  }

  // ElfRelocSection from SHT_REL and SHT_RELA section headers
  void createRelocSections(ParseLocation parentLoc) {
    if (sectionTable !is null) {
      foreach (shent; sectionTable.entries[]) {
        if (shent.disk.sh_type == shent.SectionType.SHT_REL) {
          findOrCreateSection!(ElfRelocSection!(nBits, ElfRelocHasAddend.NO))(shent, parentLoc);
        } else if (shent.disk.sh_type == shent.SectionType.SHT_RELA) {
          findOrCreateSection!(ElfRelocSection!(nBits, ElfRelocHasAddend.YES))(shent, parentLoc);
        }
      }
    }
  }

  // ElfDynamicSection from SHT_DYNAMIC section headers
  void createDynamicSections(ParseLocation parentLoc) {
    if (sectionTable !is null) {
      foreach (shent; sectionTable.entries[]) {
        if (shent.disk.sh_type == shent.SectionType.SHT_DYNAMIC)
          findOrCreateSection!(ElfDynamicSection!nBits)(shent, parentLoc);
      }
    }
  }

  // ElfDynamicSection from PT_DYNAMIC program headers
  void createDynamicSegments(ParseLocation parentLoc) {
    if (segmentTable !is null) {
      foreach (phent; segmentTable.entries[]) {
        if (phent.disk.p_type == phent.SegmentType.PT_DYNAMIC)
          findOrCreateSection!(ElfDynamicSection!nBits)(phent, parentLoc);
      }
    }
  }

  // Generic sections (ElfSection) for section headers still lacking a section object
  void createGenericSections(ParseLocation parentLoc) {
    if (sectionTable !is null) {
      foreach (shent; sectionTable.entries[]) {
        if (shent.section is null) {
          auto section = ElfSection!nBits.instance();
          shent.section = section;
          sections.pushBack(section);
          section.createContent(shent);
        }
      }
    }
  }

  // Generic sections (ElfSection) for program headers still lacking a section object
  void createGenericSegments(ParseLocation parentLoc) {
    if (segmentTable !is null) {
      foreach (phent; segmentTable.entries[]) {
        if (phent.section is null) {
          auto section = ElfSection!nBits.instance();
          phent.section = section;
          sections.pushBack(section);
          section.createContent(phent);
        }
      }
    }
  }

  // Now that string tables are parsed, we can use them to give names to the section table entries.
  // "parentLoc" is already the "section table"
  void nameSectionTableEntries(ParseLocation parentLoc) {
    if (sectionTable !is null && sectionTable.entries.length > 0) {

      // The file header e_shstrndx field usually holds the index for the section that serves as the section
      // table for the section entries, but if this is too high a value, the header stores SHN_XINDEX instead
      // and the true index is in the sh_link field of the first entry of the section table.
      auto section0 = sectionTable.entries[0];
      auto section0Loc = indexParseLocation(formatName ~ " section table entry", 0, parentLoc);
      size_t stringSectionIdx = disk.e_shstrndx;
      if (disk.e_shstrndx == ElfSectionTable!nBits.SectionIndex.SHN_XINDEX) {
        stringSectionIdx = section0.disk.sh_link;
      } else if (section0.disk.sh_link > 0) {
        appendError(section0Loc, "sh_link (" ~ section0.disk.sh_link.to!string ~ ")" ~
                    " is incompatible with file header e_shstrndx (" ~ disk.e_shstrndx.to!string ~ ")");
      }

      ElfStringTable!nBits stringTable;
      if (stringSectionIdx >= sectionTable.entries.length) {
        appendError(parentLoc, "section table string table index (" ~ stringSectionIdx.to!string ~ ")" ~
                    " is inconsistent with section table length (" ~ sectionTable.entries.length.to!string ~ " entries)");
      }
      if (stringSectionIdx > 0) { // 0 means no string section
        stringTable = cast(ElfStringTable!nBits)(sectionTable.entries[stringSectionIdx].section);
        if (stringTable is null)
          appendError(parentLoc, "string table index (" ~ stringSectionIdx.to!string ~ ") is not a string table");
      }

      // Now that we have a stringTable (or maybe null), use it to give names to all the section table entries
      foreach (shent; sectionTable.entries[]) {
        auto entryLoc = indexParseLocation(formatName ~ " section table entry", shent.index, parentLoc);
        if (shent.disk.sh_name > 0) {
          if (stringTable is null) {
            appendError(entryLoc, "section table entry has non-zero sh_name (" ~ shent.disk.sh_name.to!string ~ ")" ~
                        " but no string table");
          }
          shent.name = stringTable.stringAt(shent.disk.sh_name, shent, entryLoc);
          if (shent.section !is null)
            shent.section.name = shent.name;
        }
      }
    }
  }

  void setSectionLinkInfo(ParseLocation parentLoc) {
    foreach (shent; sectionTable.entries[]) {
      if (shent.section !is null)
        shent.section.setLinkInfo(shent, parentLoc);
    }
  }
      
  // Finish parsing the dynamic sections after the memory map is created
  void parseDynamic(ParseLocation parentLoc) {
    assert(segmentTable !is null);
    assert(memBytes !is null);

    auto dynSections = segmentTable.entries[]
      .map!(phent => cast(ElfDynamicSection!nBits) phent.section)
      .filter!"a !is null";

    foreach (dynsec; dynSections)
      dynsec.parseMemory(parentLoc);
  }

  // Return those program headers that completely enclose the specified section header's section in terms of preferred
  // memory addresses. The return value is an input range.
  auto segmentsContainingSection(ElfSectionTableEntry!nBits shent) {
    assert(shent !is null);
    assert(segmentTable !is null);

    return segmentTable
      .entries[]
      .filter!(phent => !shent.preferredExtent.empty && phent.preferredExtent.contains(shent.preferredExtent));
  }

  // Find the range of sections that are contained within the specified segment
  auto sectionsWithinSegment(ElfSegmentTableEntry!nBits phent) {
    assert(phent !is null);
    assert(sectionTable !is null);
    return sectionTable
      .entries[]
      .filter!(shent => !shent.preferredExtent.empty && phent.preferredExtent.contains(shent.preferredExtent));
  }

  // Returns the first registered section whose preferred mapping address is the specified address and which has the
  // specified dynamic type. Returns null if none found.
  Section sectionMappedAt(Section = ElfSection!nBits)(Word!nBits va)
  if (is(Section : ElfSection!nBits)) {
    foreach (section; sections[].map!(a => cast(Section) a).filter!"a !is null") {
      if (!section.preferredExtent.empty && section.preferredExtent.least == va)
        return section;
    }
    return null;
  }

  Tuple!(Section, "section", string, "error")
  sectionByIndex(Section = ElfSection!nBits)(size_t idx) {
    alias Found = Tuple!(Section, "section", string, "error");
    Section section;
    Exception error;

    if (sectionTable is null)
      return Found(section, "ELF file has no section table");
    if (idx > sectionTable.entries.length) {
      return Found(section,
                   "section index (" ~ to!string(idx) ~ ")" ~
                   " is out of range ([0, " ~ to!string(sectionTable.entries.length) ~ "])");
    }

    auto shent = sectionTable.entries[idx];
    if (shent.disk.sh_type == ElfSectionTableEntry!nBits.SectionType.SHT_NULL)
      return Found(section, "section index (" ~ to!string(idx) ~ ") points to an SHT_NULL entry");

    if (shent.section is null)
      return Found(section, "section index (" ~ to!string(idx) ~ ") has no section pointer");

    section = cast(Section) shent.section;
    if (section is null)
      return Found(section, "section index (" ~ to!string(idx) ~ ") is not the right type" ~
                   "(expected " ~ Section.stringof ~ " but is " ~ typeid(shent.section).stringof ~ ")");

    return Found(section, "");
  }

  Section sectionByIndex(Section = ElfSection!nBits)(size_t idx, Ast errorNode, ParseLocation loc) {
    auto found = sectionByIndex!Section(idx);
    if (found.error != "")
      errorNode.appendError(loc, found.error);
    return found.section;
  }

  ParseLocation fileLocation(string what, Word!nBits offset, ParseLocation parent) {
    auto global = Interval!(Word!nBits).whole;
    return new MapParseLocation!(MemoryMap!(Word!nBits))(what, AddressSpace.FILE, fileBytes, global, offset, "", parent);
  }
}
