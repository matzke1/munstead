module munstead.elf.sections;

import core.bitop: popcnt;
import munstead.ast.base;
import munstead.ast.sections;
import munstead.core.bitflags;
import munstead.core.byteorder;
import munstead.core.exception;
import munstead.core.interval;
import munstead.core.mmap;
import munstead.core.wordtypes;
import munstead.elf.files;
import munstead.elf.segments;
import munstead.elf.types;
import std.algorithm: joiner, map, max, until;
import std.conv: to;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// A section table contains zero or more section table entries

class ElfSectionTable(size_t nBits): Ast {
  mixin AstNodeFeatures;

  struct AstChildren {
    AstList!(ElfSectionTableEntry!nBits) entries;
  }

  enum SectionIndex { SHN_UNDEF=0, SHN_LORESERVE=0xff00, SHN_LOPROC=0xff00, SHN_HIPROC=0xff1f, SHN_LOOS=0xff20,
		      SHN_HIOS=0xff3f, SHN_ABS=0xfff1, SHN_COMMON=0xfff2, SHN_XINDEX=0xffff, SHN_HIRESERVE=0xffff }

  this() {
    entries = new AstList!(ElfSectionTableEntry!nBits);
  }

  static ElfSectionTable
  parse(MemoryMap)(MemoryMap file, ElfFileHeader!nBits fhdr) {
    assert(fhdr !is null);
    enum me = "ELF-" ~ to!string(nBits) ~ " section table";
    auto ret = new ElfSectionTable;
    MemoryMap.Address firstOffset = fhdr.disk.e_shoff;
    size_t nEntries = fhdr.disk.e_shnum;
    size_t bytesPerEntry = fhdr.disk.e_shentsize;
   
    // Table entry #0 is always SHT_NULL and usually zero filled. However, two fields are special:
    //   1. If sh_size is non-zero then it is the actual number of table entries. The file header's e_shnum field is
    //      normally zero in this case, and this happens when the total number of sections is >= SHN_LORESERVE (0xff00)
    //      since those section numbers have special meanings.
    //
    //   2. If sh_link is non-zero, then it's the index of the section table's string section and the file header should
    //      contain SHN_XINDEX.

    foreach (entryNumber; 0 .. max(1, nEntries)) {
      auto entryOffset = cast(MemoryMap.Address)(firstOffset + entryNumber * bytesPerEntry);
      auto entry = ElfSectionTableEntry!nBits.parse!MemoryMap(file, entryOffset, bytesPerEntry, fhdr.byteOrder);
      entry.index = entryNumber;
      if (entry.errors.length > 0)
        ret.appendError(new SyntaxError(me~" problem parsing entry #" ~ to!string(entryNumber), file.name, entryOffset));
      ret.entries.pushBack(entry);

      if (0 == entryNumber && entry.disk.sh_size > 0) {
	if (nEntries != 0)
	  ret.appendError(new SyntaxError(me~" entry #0 sh_size and header e_shnum are both non-zero", file.name, entryOffset));
	nEntries = entry.disk.sh_size; // standard says to use this if present
      }
    }
    return ret;
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class ElfSectionTableEntry(size_t nBits): Ast {
  mixin AstNodeFeatures;

  struct AstChildren {
    ElfSection!nBits section; 	// section created for this table entry
  }

  enum SectionType : Elf!nBits.Word {
    SHT_NULL=0, SHT_PROGBITS=1, SHT_SYMTAB=2, SHT_STRTAB=3, SHT_RELA=4, SHT_HASH=5, SHT_DYNAMIC=6, SHT_NOTE=7,
    SHT_NOBITS=8, SHT_REL=9, SHT_SHLIB=10, SHT_DYNSYM=11, /*12  & 13 are missing*/ SHT_INIT_ARRAY=14,
    SHT_FINI_ARRAY=15, SHT_PREINIT_ARRAY=16, SHT_GROUP=17, SHT_SYMTAB_SHNDX=18,
    SHT_LOOS=0x60000000, SHT_HIOS=0x6fffffff, SHT_LOPROC=0x70000000, SHT_HIPROC=0x7fffffff,
    SHT_LOUSER=0x80000000, SHT_HIUSER=0xffffffff }

  enum SectionFlags : Elf!nBits.Xword {
    SHF_WRITE=1, SHF_ALLOC=2, SHF_EXECINSTR=4, SHF_MERGE=0x10, SHF_STRINGS=0x20, SHF_INFO_LINK=0x40,
    SHF_LINK_ORDER=0x80, SHF_OS_NONCONFORMING=0x100, SHF_GROUP=0x200, SHF_TLS=0x400, SHF_COMPRESSED=0x800,
    SHF_MASKOS=0x0ff00000, SHF_MASKPROC=0xf0000000 }

  struct Disk {
    align(1):
    Elf!nBits.Word sh_name;
    SectionType sh_type;
    BitFlags!SectionFlags sh_flags;
    Elf!nBits.Addr sh_addr;
    Elf!nBits.Off sh_offset;
    Elf!nBits.Xword sh_size;
    Elf!nBits.Word sh_link;
    Elf!nBits.Word sh_info;
    Elf!nBits.Xword sh_addralign;
    Elf!nBits.Xword sh_entsize;
  }

  Disk disk;
  string name;
  size_t index; // index in the section table

  // Parse an entry from some part of a file
  static ElfSectionTableEntry
  parse(MemoryMap)(MemoryMap file, MemoryMap.Address offset, size_t bytesPerEntry, ByteOrder byteOrder) {
    enum me = "ELF-" ~ to!string(nBits) ~ " section table entry";
    auto ret = new ElfSectionTableEntry;

    if (bytesPerEntry < ret.disk.sizeof)
      ret.appendError(new SyntaxError(me ~ " actual size (" ~ to!string(ret.disk.sizeof) ~ ")" ~
                                      " is larger than header-specified size (" ~ to!string(bytesPerEntry) ~ ")",
                                      file.name, offset));

    if (!file.readObjectAt(offset, ret.disk))
      ret.appendError(new SyntaxError(me ~ " short read", file.name, offset));
    ret.disk = ret.disk.toNative(byteOrder);

    Elf!nBits.Xword alignment = max(1, ret.disk.sh_addralign);
    if (popcnt(alignment) > 1)
      ret.appendError(new SyntaxError(me ~ " sh_addralign (" ~ to!string(ret.disk.sh_addralign) ~ ") should be a power of two",
                                      file.name, offset));

    return ret;
  }

  // Location of section within file
  Interval!(Elf!nBits.Off) fileExtent() @property {
    if (disk.sh_type == SectionType.SHT_NOBITS) {
      return Interval!(Elf!nBits.Off)();
    } else {
      return Interval!(Elf!nBits.Off).baseSize(disk.sh_offset, disk.sh_size);
    }
  }

  // Preferred location of section within memory
  Interval!(Elf!nBits.Addr) preferredExtent() @property {
    if (disk.sh_flags.isSet(SectionFlags.SHF_ALLOC)) {
      return Interval!(Elf!nBits.Addr).baseSize(disk.sh_addr, disk.sh_size);
    } else {
      return Interval!(Elf!nBits.Addr)();
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Any section from a file.
class ElfSection(size_t nBits): AsmSection!nBits {
  mixin AstNodeFeatures;
  struct AstChildren {}

  // Initialize file and memory offsets and sizes and other such things from the ELF section table entry
  void initFromSectionTableEntry(MemoryMap)(MemoryMap file, ElfSectionTableEntry!nBits shent) {
    assert(shent !is null);

    fileExtent = shent.fileExtent;
    createSectionMmap!MemoryMap(file, fileExtent);
    preferredExtent = Interval!(Word!nBits).baseSize(shent.disk.sh_addr, shent.disk.sh_size);
    name = shent.name;
  }

  void initFromSegmentTableEntry(MemoryMap)(MemoryMap file, ElfSegmentTableEntry!nBits phent) {
    assert(phent !is null);

    fileExtent = phent.fileExtent;
    createSectionMmap!MemoryMap(file, fileExtent);
    preferredExtent = phent.preferredExtent;
    name = "segment #" ~ to!string(phent.index);
  }

  // Index of section within the ELF section table.
  size_t sectionTableIndex() {
    auto shent = this.ancestor!(ElfSectionTableEntry!nBits);
    assert(shent !is null);
    return shent.index;
  }

  // Return the string starting at the specified section-relative address. ELF string tables store 8-bit NUL-terminated
  // strings.
  string stringAt(Word!nBits sra, size_t maxBytes = 1024*1024*1024) {
    string me = "ELF-" ~ to!string(nBits) ~ (name=="" ? "" : " " ~ name) ~ " section";
    string s;

    if (!mmap) {
      appendError(new SyntaxError(me ~ " has no memory map", mmap.name, sra));

    } else {
      import std.range;
      enum chunkSize = 128 /*arbitrary*/;
      size_t maxChunks = (maxBytes + chunkSize - 1) / chunkSize;

      auto chars = mmap.segmentsAt(sra).contiguous
	.byBuffer(chunkSize).map!"a.buffer"
	.take(maxChunks)
	.joiner.until!"a == 0"(No.openRight)
	.array;

      if (0 == chars.length)
	appendError(new SyntaxError(me ~ " string starting address " ~ to!string(sra) ~ " is out of range",
				    mmap.name, sra));

      if (chars[$-1] != 0)
	appendError(new SyntaxError(me ~ " string starting at sra " ~ to!string(sra) ~
				    " is not NUL-terminated", "", 0)); // FIXME: location?

      s = (cast(char[]) chars[0..$-1]).idup;
    }
    return s;
  }
}
