module munstead.elf.symbols;

import munstead.ast.base;
import munstead.core.byteorder;
import munstead.core.exception;
import munstead.core.mmap;
import munstead.elf.files;
import munstead.elf.sections;
import munstead.elf.strings;
import munstead.elf.types;
import std.algorithm: map;
import std.array: array;
import std.conv: to;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// The SHT_SYMTAB_SHNDX table: an array of Elf32_Word values parallel to the array of symbols in a symbol table. If the
// corresponding symbol's ??? member contains the value SHN_XINDEX then its true value is contained in this
// table. Otherwise, this table contains SHN_UNDEF (zero).
class ElfSymbolSectionIndexTable(size_t nBits): ElfSection!nBits {
  mixin AstNodeFeatures;
  struct AstChildren {}

  Elf!32.Word[] sectionIndexes; // array is parallel with ElfSymbolTable!nBits.symbols

  static ElfSymbolSectionIndexTable
  parse(MemoryMap)(MemoryMap file, ElfFileHeader!nBits fhdr, ElfSectionTableEntry!nBits shent) {
    assert(fhdr !is null);
    assert(shent !is null);
    enum me = "ELF-" ~ to!string(nBits) ~ " symbol section-index table";
    auto ret = new ElfSymbolSectionIndexTable;
    ret.initFromSectionTableEntry(file, shent);
    
    ret.sectionIndexes = ret.mmap
      .segmentsAt(0).contiguous
      .byBuffer(Elf!32.Word.sizeof)
      .map!(node => toNative((cast(Elf!32.Word[])(node.buffer))[0], fhdr.byteOrder))
      .array;

    return ret;
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class ElfSymbolTable(size_t nBits): ElfSection!nBits {
  mixin AstNodeFeatures;

  struct AstChildren {
    AstList!(ElfSymbol!nBits) symbols;
  }

  ElfStringTable!nBits stringTable;
  ElfSymbolSectionIndexTable!nBits sectionIndexTable;

  this() {
    symbols = new AstList!(ElfSymbol!nBits);
  }

  // Create a new symbol table by parsing the symbols starting at the specified file offset.
  static ElfSymbolTable
  parse(MemoryMap)(MemoryMap file, ElfFileHeader!nBits fhdr, ElfSectionTableEntry!nBits shent) {
    enum me = "ELF-" ~ to!string(nBits) ~ " symbol table";
    auto ret = new ElfSymbolTable;
    ret.initFromSectionTableEntry(file, shent);

    // sh_link is the index of the section containing the string table for these symbols.
    if (0 != shent.disk.sh_link) {
      if (shent.disk.sh_link >= fhdr.sectionTable.entries.length) {
	ret.appendError(new SyntaxError(me ~ " sh_link (" ~ to!string(shent.disk.sh_link) ~ ") is out or range",
					file.name, 0)); // FIXME: location
      } else {
	ret.stringTable = cast(ElfStringTable!nBits) fhdr.sectionTable.entries[shent.disk.sh_link].section;
	if (ret.stringTable is null)
	  ret.appendError(new SyntaxError(me ~ " sh_link (" ~ to!string(shent.disk.sh_link) ~ ") is not a string table",
					  file.name, 0)); // FIXME: location
      }
    }

    // Is there an associated SHT_SYMTAB_SHNDX table for this symbol table? Section-index tables point to symbol tables,
    // not vice versa -- and they do so by section table index since they were created before this symbol table. Therefore,
    // we need to scan the symbol table entries created thus far to see if any section-index tables exist.
    foreach (testEnt; fhdr.sectionTable.entries[]) {
      auto shndx = cast(ElfSymbolSectionIndexTable!nBits) testEnt.section;
      if (shndx !is null && testEnt.disk.sh_link == shent.index) {
	if (ret.sectionIndexTable !is null) {
	  ret.appendError(new SyntaxError(me ~ " has multiple section index tables", file.name, 0));
	} else {
	  ret.sectionIndexTable = shndx;
	}
      }
    }

    // Calculate number of symbols
    size_t bytesPerSymbol = ElfSymbol!nBits.Disk.sizeof;
    size_t nSymbols = ret.fileExtent.length / bytesPerSymbol;
    if (0 == nSymbols)
      ret.appendError(new SyntaxError(me ~ " must contain at least one symbol of for STN_UNDEF",
				      shent.name, 0));

    // Parse each symbol
    foreach (symbolNumber; 0 .. nSymbols) {
      auto symbolOffset = cast(MemoryMap.Address)(symbolNumber * bytesPerSymbol);
      auto symbol = ElfSymbol!nBits.parse!MemoryMap(ret, symbolNumber, symbolOffset, fhdr.byteOrder);
      if (symbol.errors.length > 0)
        ret.appendError(new SyntaxError(me~" problem parsing symbol #" ~ to!string(symbolNumber),
					shent.name, symbolOffset));
      ret.symbols.pushBack(symbol);
    }

    return ret;
  }

}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class ElfSymbol(size_t nBits): Ast {
  mixin AstNodeFeatures;
  struct AstChildren {}

  enum Binding { STB_LOCAL=0, STB_GLOBAL=1, STB_WEAK=2, STB_LOOS=10, STB_HIOS=12, STB_LOPROC=13, STB_HIPROC=15 }

  enum Type { STT_NOTYPE=0, STT_OBJECT=1, STT_FUNC=2, STT_SECTION=3, STT_FILE=4, STT_COMMON=5,
	      STT_TLS=6, STT_LOOS=10, STT_HIOS=12, STT_LOPROC=13, STT_HIPROC=15 }

  enum Visibility { STV_DEFAULT=0, STV_INTERNAL=1, STV_HIDDEN=2, STV_PROTECTED=3 }

  static if (32 == nBits) {
    struct Disk {
      Elf!32.Word st_name;
      Elf!32.Addr st_value;
      Elf!32.Word st_size;
      ubyte st_info;
      ubyte st_other;
      Elf!32.Half st_shndx;
    }
  } else {
    static assert(64 == nBits);
    struct Disk {
      Elf!64.Word st_name;
      ubyte st_info;
      ubyte st_other;
      Elf!64.Half st_shndx;
      Elf!64.Addr st_value;
      Elf!64.Xword st_size;
    }
  }

  Disk disk;
  string name;
  Binding binding;
  Type type;
  Visibility visibility;
  size_t linkedSectionIndex;

  static ElfSymbol
  parse(MemoryMap)(ElfSymbolTable!nBits symtab, size_t symbolIndex, MemoryMap.Address offset, ByteOrder byteOrder) {
    assert(symtab !is null);
    enum me = "ELF-" ~ to!string(nBits) ~ " symbol";
    auto ret = new ElfSymbol;

    if (!symtab.mmap.readObjectAt(offset, ret.disk))
      ret.appendError(new SyntaxError(me ~ " short read", symtab.mmap.name, offset));
    ret.disk = ret.disk.toNative(byteOrder);

    // Name
    if (ret.disk.st_name > 0) {
      if (symtab.stringTable is null) {
	ret.appendError(new SyntaxError(me ~ " st_name (" ~ to!string(ret.disk.st_name) ~ ") " ~
					"non-zero but no string table",
					symtab.mmap.name, offset));
      } else {
	ret.name = symtab.stringTable.stringAt(ret.disk.st_name);
      }
    }

    // Linked section index. We can't store ptrs to linked sections yet because we might not have parsed all the linked
    // sections, but we can at least store the linked section's index in the section table.
    if (ret.disk.st_shndx == ElfSectionTable!nBits.SectionIndex.SHN_XINDEX) {
      if (symtab.sectionIndexTable is null) {
	ret.appendError(new SyntaxError(me ~ " st_shndx is SHN_XINDEX but no section index table",
					symtab.mmap.name, offset));
      } else if (symbolIndex >= symtab.sectionIndexTable.sectionIndexes.length) {
	ret.appendError(new SyntaxError(me ~ " st_shndx is SHN_XINDEX but section index table is too short",
					symtab.mmap.name, offset));
      } else {
	ret.linkedSectionIndex = symtab.sectionIndexTable.sectionIndexes[symbolIndex];
      }
    } else if (symtab.sectionIndexTable !is null &&
	       symbolIndex < symtab.sectionIndexTable.sectionIndexes.length &&
	       symtab.sectionIndexTable.sectionIndexes[symbolIndex] != 0) {
      ret.appendError(new SyntaxError(me ~ " st_shndx is not SHN_XINDEX but section index table has a non-zero value",
				      symtab.mmap.name, offset));
      ret.linkedSectionIndex = ret.disk.st_shndx;
    } else {
      ret.linkedSectionIndex = ret.disk.st_shndx;
    }      
     
    // Bind, type, and visibility
    ret.binding = cast(Binding)(ret.disk.st_info >> 4);
    ret.type = cast(Type)(ret.disk.st_info & 0x0f);
    ret.visibility = cast(Visibility)(ret.disk.st_other & 0x03);

    if ((ret.disk.st_other & ~ 0x03) != 0)
      ret.appendError(new SyntaxError(me ~ " st_other (0x" ~ to!string(ret.disk.st_other, 16) ~ ") " ~
				      "high order bits should be zero",
				      symtab.mmap.name, offset));
								     
    return ret;
  }
}
