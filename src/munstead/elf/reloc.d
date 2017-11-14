module munstead.elf.reloc;

import munstead.ast.base;
import munstead.core.byteorder;
import munstead.core.exception;
import munstead.core.interval;
import munstead.core.mmap;
import munstead.core.util;
import munstead.core.wordtypes;
import munstead.elf.files;
import munstead.elf.sections;
import munstead.elf.symbols;
import munstead.elf.types;
import std.conv: to;

enum ElfRelocHasAddend { NO, YES };

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class ElfRelocSection(size_t nBits, ElfRelocHasAddend hasAddend): ElfSection!nBits {
  mixin AstNodeFeatures;
  struct AstChildren {
    AstList!(ElfRelocEntry!(nBits, hasAddend)) entries;
  }

  ElfSymbolTable!nBits symtab; // from the section header sh_link field
  ElfSection!nBits targetSection; // from the section header sh_info field

  this() {
    entries = new AstList!(ElfRelocEntry!(nBits, hasAddend));
  }

  void parse(ParseLocation parentLoc) {
    auto fhdr = ancestor!(ElfFileHeader!nBits);
    assert(fhdr !is null);
    auto tableLoc = sectionLocation(fhdr.formatName ~ " relocation section", 0, parentLoc);

    enum bytesPerEntry = ElfRelocEntry!(nBits, hasAddend).Disk.sizeof;
    size_t nEntries = bytes.hull.length / bytesPerEntry;
    for (size_t index = 0; index < nEntries; ++index) {
      auto entryLoc = indexParseLocation(fhdr.formatName ~ " relocation entry", index, tableLoc);
      auto entry = ElfRelocEntry!(nBits, hasAddend).instance(this, entryLoc);
      entries.pushBack(entry);
      entry.parse(entryLoc);
    }
  }

  // Update symtab and targetSection members from the elf section table header's sh_link and sh_info fields
  override void setLinkInfo(ElfSectionTableEntry!nBits shent, ParseLocation parentLoc) {
    assert(shent !is null);

    auto fhdr = ancestor!(ElfFileHeader!nBits);
    assert(fhdr !is null);
    assert(fhdr.sectionTable !is null);
    auto loc = sectionLocation(fhdr.formatName ~ " relocation section", 0, parentLoc);

    if (shent.disk.sh_link > 0)
      symtab = fhdr.sectionByIndex!(ElfSymbolTable!nBits)(shent.disk.sh_link, this, loc);
    if (shent.disk.sh_link > 0)
      targetSection = fhdr.sectionByIndex(shent.disk.sh_link, this, loc);
  }    
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Base class for architecture-specific relocation entries
class ElfRelocEntry(size_t nBits, ElfRelocHasAddend hasAddend): Ast {
  mixin AstNodeFeatures;
  struct AstChildren {}

  struct Disk {
    Elf!nBits.Addr r_offset;
    Elf!nBits.Xword r_info;
    static if (hasAddend == ElfRelocHasAddend.YES)
      Elf!nBits.Sxword r_addend;
  }

  size_t index; // index of this entry in the relocation section
  Disk disk;

  // Create a new relocation entry based on the machine described in the ELF header
  static ElfRelocEntry instance(ElfRelocSection!(nBits, hasAddend) section, ParseLocation parentLoc) {
    auto loc = parentLoc;
    assert(section !is null);
    auto fhdr = section.ancestor!(ElfFileHeader!nBits);
    assert(fhdr !is null);
    with (ElfFileHeader!nBits.Machine) {
      switch (fhdr.disk.e_machine) {
        
        //======= 32-bit architectures ========//
        static if (nBits == 32) {
          case EM_386:
            return ElfRelocEntry386!hasAddend.instance();
        }

        //======= 64-bit architectures ========//
        static if (nBits == 64) {
          case EM_X86_64:
            return ElfRelocEntryX8664!hasAddend.instance();
        }
        
      default:
        section.appendError(loc, "cannot parse " ~ fhdr.disk.e_machine.to!string ~ " relocation entries (not implemented yet)");
        return ElfRelocEntryGeneric!(nBits, hasAddend).instance();
      }
    }
  }

  abstract void parse(ParseLocation parentLoc);

  protected void parseBase(ElfFileHeader!nBits fhdr, ElfRelocSection!(nBits, hasAddend) section, ParseLocation loc) {
    assert(fhdr !is null);
    assert(section !is null);

    index = section.entries.length - 1;
    assert(section.entries[index] is this);
    auto offset = cast(Word!nBits)(index * Disk.sizeof);
    if (!section.bytes.readObjectAt(offset, disk))
      appendError(loc, "short read");
    disk = disk.toNative(fhdr.byteOrder);
  }

  // Relocation type. Returned as an integer rather than an enum because each architecture has their own set of
  // relocation types and values overlap between architectures.
  uint typeNumber() const {
    static if (nBits == 32) {
      return disk.r_info & 0xff;
    } else {
      static assert (nBits == 64);
      return disk.r_info & 0xffffffff;
    }
  }
  
  // Index of associated symbol in a symbol table.
  size_t symbolIndex() const {
    static if (nBits == 32) {
      return disk.r_info >> 8;
    } else {
      static assert (nBits == 64);
      return disk.r_info >> 32;
    }
  }

  // Symbol associated with this reloction entry or null
  ElfSymbol!nBits symbol() {
    auto relocs = ancestor!(ElfRelocSection!(nBits, hasAddend));
    assert(relocs !is null);

    if (relocs.symtab is null)
      return null;
    if (symbolIndex > relocs.symtab.symbols.length)
      return null;
    return relocs.symtab.symbols[symbolIndex];
  }
    
  // Returns the string describing the relocation type.
  abstract string typeName()const ;

}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// x86 (32-bit, a.k.a., i386) relocation entry
class ElfRelocEntry386(ElfRelocHasAddend hasAddend): ElfRelocEntry!(32, hasAddend) {
  enum : size_t { nBits = 32 }
  mixin AstNodeFeatures;
  struct AstChildren {}
    
  enum RelocType {
    // i386 relocations.
    //
    //   A     The addend used to compute the value of the relocatable field.
    //   B     The base address at which a shared object has been loaded into memory.
    //   G     Offset into the GOT
    //   GOT   Address of the GOT
    //   L     The place (section offset or address) of the PLT entry for a symbol
    //   P     The place (section offset or address) of the storage unit being relocated (computed from r_offset)
    //   S     The value of the symbol whose index resides in the relocation entry
    //
    // i386                 Field_____ Calculation______
    R_386_NONE     =  0, // none       none
    R_386_32       =  1, // word32     S + A
    R_386_PC32     =  2, // word32     S + A - P
    R_386_GOT32    =  3, // word32     G + A - P
    R_386_PLT32    =  4, // word32     L + A - P
    R_386_COPY     =  5, // none       none
    R_386_GLOB_DAT =  6, // word32     S
    R_386_JMP_SLOT =  7, // word32     S
    R_386_RELATIVE =  8, // word32     B + A
    R_386_GOTOFF   =  9, // word32     S + A - GOT
    R_386_GOTPC    = 10  // word32     GOT + A - P
  }

  override void parse(ParseLocation parentLoc) {
    auto fhdr = ancestor!(ElfFileHeader!nBits);
    assert(fhdr !is null);
    auto table = ancestor!(ElfRelocSection!(nBits, hasAddend));
    assert(table !is null);
    parseBase(fhdr, table, parentLoc);
  }

  override string typeName() const {
    return to!string(cast(RelocType) typeNumber);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Amd64 (64-bit, a.k.a., x86_64) relocation entry
class ElfRelocEntryX8664(ElfRelocHasAddend hasAddend): ElfRelocEntry!(64, hasAddend) {
  enum : size_t { nBits = 64}
  mixin AstNodeFeatures;
  struct AstChildren {}

  enum RelocType {
    R_X86_64_NONE            =  0, // none       none
    R_X86_64_64              =  1, // word64     S + A
    R_X86_64_PC32            =  2, // word32     S + A - P
    R_X86_64_GOT32           =  3, // word32     G + A
    R_X86_64_PLT32           =  4, // word32     L + A - P
    R_X86_64_COPY            =  5, // none       none
    R_X86_64_GLOB_DAT        =  6, // word64     S
    R_X86_64_JUMP_SLOT       =  7, // word64     S
    R_X86_64_RELATIVE        =  8, // word64     B + A
    R_X86_64_GOTPCREL        =  9, // word32     G + GOT + A - P
    R_X86_64_32              = 10, // word32     S + A
    R_X86_64_32S             = 11, // word32     S + A
    R_X86_64_16              = 12, // word16     S + A
    R_X86_64_PC16            = 13, // word16     S + A - P
    R_X86_64_8               = 14, // word8      S + A
    R_X86_64_PC8             = 15, // word8      S + A - P
    R_X86_64_DTPMOD64        = 16, // word64
    R_X86_64_DTPOFF64        = 17, // word64
    R_X86_64_TPOFF64         = 18, // word64
    R_X86_64_TLSGD           = 19, // word32
    R_X86_64_TLSLD           = 20, // word32
    R_X86_64_DTPOFF32        = 21, // word32
    R_X86_64_GOTTPOFF        = 22, // word32
    R_X86_64_TPOFF32         = 23, // word32
    R_X86_64_PC64            = 24, // word64     S + A - P
    R_X86_64_GOTOFF64        = 25, // word64     S + A - GOT
    R_X86_64_GOTPC32         = 26, // word32     GOT + A - P
    // 26-31 unused?
    R_X86_64_SIZE32          = 32, // word32     Z + A
    R_X86_64_SIZE64          = 33, // word64     Z + A
    R_X86_64_GOTPC32_TLSDESC = 34, // word32
    R_X86_64_TLSDESC_CALL    = 35, // none
    R_X86_64_TLSDESC         = 36, // word64x2
    R_X86_64_IRELATIVE       = 37, // word64     indirect (B + A)
  }
  
  override void parse(ParseLocation parentLoc) {
    auto fhdr = ancestor!(ElfFileHeader!nBits);
    assert(fhdr !is null);
    auto table = ancestor!(ElfRelocSection!(nBits, hasAddend));
    assert(table !is null);
    parseBase(fhdr, table, parentLoc);
  }

  override string typeName() const {
    return to!string(cast(RelocType) typeNumber);
  }
}                                                               

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class ElfRelocEntryGeneric(size_t nBits, ElfRelocHasAddend hasAddend): ElfRelocEntry!(nBits, hasAddend) {
  mixin AstNodeFeatures;
  struct AstChildren {}
  
  override void parse(ParseLocation parentLoc) {
    auto fhdr = ancestor!(ElfFileHeader!nBits);
    assert(fhdr !is null);
    auto table = ancestor!(ElfRelocSection!(nBits, hasAddend));
    assert(table !is null);
    parseBase(fhdr, table, parentLoc);
  }

  override string typeName() const {
    return typeNumber.to!string;
  }
}
