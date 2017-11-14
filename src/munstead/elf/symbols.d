module munstead.elf.symbols;

import munstead.ast.base;
import munstead.core.byteorder;
import munstead.core.exception;
import munstead.core.mmap;
import munstead.core.util;
import munstead.core.wordtypes;
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

  void parse(ParseLocation parentLoc) {
    auto fhdr = ancestor!(ElfFileHeader!nBits);
    assert(fhdr !is null);
    assert(bytes !is null);

    sectionIndexes = bytes
      .segmentsAt(0).contiguous
      .byBuffer(Elf!32.Word.sizeof)
      .map!(node => toNative((cast(Elf!32.Word[])(node.buffer))[0], fhdr.byteOrder))
      .array;
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

  void parse(ParseLocation parentLoc) {
    auto fhdr = ancestor!(ElfFileHeader!nBits);
    assert(fhdr !is null);
    auto tableLoc = sectionLocation(fhdr.formatName ~ " symbol table", 0, parentLoc);

    // sh_link is the index of the section containing the string table for these symbols.
    auto shent = sectionTableEntry();
    assert(shent !is null);
    if (0 != shent.disk.sh_link) {
      if (shent.disk.sh_link >= fhdr.sectionTable.entries.length) {
        appendError(tableLoc, "sh_link (" ~ shent.disk.sh_link.to!string ~ ") is out or range");
      } else {
        stringTable = cast(ElfStringTable!nBits) fhdr.sectionTable.entries[shent.disk.sh_link].section;
        if (stringTable is null)
          appendError(tableLoc, "sh_link (" ~ shent.disk.sh_link.to!string ~ ") is not a string table");
      }
    }

    // Is there an associated SHT_SYMTAB_SHNDX table for this symbol table? Section-index tables point to symbol tables,
    // not vice versa -- and they do so by section table index since they were created before this symbol table. Therefore,
    // we need to scan the symbol table entries created thus far to see if any section-index tables exist.
    foreach (testEnt; fhdr.sectionTable.entries[]) {
      auto shndx = cast(ElfSymbolSectionIndexTable!nBits) testEnt.section;
      if (shndx !is null && testEnt.disk.sh_link == shent.index) {
        if (sectionIndexTable !is null) {
          appendError(tableLoc, "has multiple section index tables");
        } else {
          sectionIndexTable = shndx;
        }
      }
    }

    // Calculate number of symbols
    size_t bytesPerSymbol = ElfSymbol!nBits.Disk.sizeof;
    size_t nSymbols = fileExtent.length / bytesPerSymbol;
    if (0 == nSymbols)
      appendError(tableLoc, "empty symbol table is missing STN_UNDEF entry");

    // Parse each symbol
    foreach (symbolNumber; 0 .. nSymbols) {
      auto entryLoc = indexParseLocation(fhdr.formatName ~ " symbol", symbolNumber, tableLoc);
      auto entry = ElfSymbol!nBits.instance();
      symbols.pushBack(entry);
      entry.parse(entryLoc);
    }
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
  size_t index;                 // index of this symbol in the parent table
  string name;
  Binding binding;
  Type type;
  Visibility visibility;
  size_t linkedSectionIndex; // zero means no linked section

  void parse(ParseLocation parentLoc) {
    auto fhdr = ancestor!(ElfFileHeader!nBits);
    assert(fhdr !is null);
    auto symtab = ancestor!(ElfSymbolTable!nBits);
    assert(symtab !is null);

    index = symtab.symbols.length - 1;
    assert(symtab.symbols[index] is this);
    auto offset = cast(Word!nBits) (index * Disk.sizeof);
    auto loc = symtab.sectionLocation(fhdr.formatName ~ " symbol table entry", offset, parentLoc);

    if (!symtab.bytes.readObjectAt(offset, disk))
      appendError(loc, "short read");
    disk = disk.toNative(fhdr.byteOrder);

    // Name
    if (disk.st_name > 0) {
      if (symtab.stringTable is null) {
        appendError(loc, "st_name (" ~ disk.st_name.to!string ~ ") is non-zero but no string table");
      } else {
        name = symtab.stringTable.stringAt(disk.st_name, this, loc);
      }
    }

    // Linked section index. We can't store ptrs to linked sections yet because we might not have parsed all the linked
    // sections, but we can at least store the linked section's index in the section table.
    with (ElfSectionTable!nBits.SectionIndex) {
      switch (disk.st_shndx) {
      case SHN_UNDEF:
      case SHN_ABS:
      case SHN_COMMON:
        // These entries are special. They don't refer to linked sections.
        break;

      case SHN_XINDEX:
        // The linked section is not stored in st_shndx, but rather in a parallel array called the section index table.
        if (symtab.sectionIndexTable is null) {
          appendError(loc, "st_shndx is SHN_XINDEX but no section index table");
        } else if (index >= symtab.sectionIndexTable.sectionIndexes.length) {
          appendError(loc, "st_shndx is SHN_XINDEX but section index table is too short");
        } else {
          linkedSectionIndex = symtab.sectionIndexTable.sectionIndexes[index];
        }
        break;

      default:
        // Plain old section index.
        if (symtab.sectionIndexTable !is null &&
            index < symtab.sectionIndexTable.sectionIndexes.length &&
            symtab.sectionIndexTable.sectionIndexes[index] != 0) {
          appendError(loc, "st_shndx is not SHN_XINDEX but section index table has a non-zero value");
          linkedSectionIndex = disk.st_shndx;
        } else {
          linkedSectionIndex = disk.st_shndx;
        }
        break;
      }
    }

    // Bind, type, and visibility
    binding = cast(Binding)(disk.st_info >> 4);
    type = cast(Type)(disk.st_info & 0x0f);
    visibility = cast(Visibility)(disk.st_other & 0x03);

    if ((disk.st_other & ~ 0x03) != 0)
      appendError(loc, "st_other (" ~ disk.st_other.hexStr ~ ") high order bits should be zero");
  }

  // Returns a pointer to the linked section if there is one, otherwise null. This function is only available after all
  // sections have been parsed.
  ElfSection!nBits linkedSection() {
    if (linkedSectionIndex == 0)
      return null; // no link

    auto fhdr = ancestor!(ElfFileHeader!nBits);
    assert(fhdr !is null);
    assert(fhdr.sectionTable !is null);

    if (linkedSectionIndex >= fhdr.sectionTable.entries.length)
      return null; // out of range

    return fhdr.sectionTable.entries[linkedSectionIndex].section;
  }
}
