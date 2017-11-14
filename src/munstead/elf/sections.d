module munstead.elf.sections;

import core.bitop: popcnt;
import munstead.ast.base;
import munstead.ast.sections;
import munstead.core.bitflags;
import munstead.core.byteorder;
import munstead.core.exception;
import munstead.core.interval;
import munstead.core.mmap;
import munstead.core.util;
import munstead.core.wordtypes;
import munstead.elf.files;
import munstead.elf.segments;
import munstead.elf.types;
import std.algorithm: filter, joiner, map, max, until;
import std.array: array;
import std.conv: to;
import std.range: enumerate, take;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// A region of a file or virtual memory
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class ElfSection(size_t nBits): AsmSection!nBits {
  mixin AstNodeFeatures;
  struct AstChildren {}

  // Initialize file and memory offsets and sizes and other such things from the ELF section table entry
  void createContent(ElfSectionTableEntry!nBits shent) {
    assert(shent !is null);
    auto fhdr = ancestor!(ElfFileHeader!nBits);
    assert(fhdr !is null);

    fileExtent = shent.fileExtent;
    createSectionBytes!(MemoryMap!(Word!nBits))(fhdr.fileBytes, fileExtent, AddressSpace.FILE);

    preferredExtent = Interval!(Word!nBits).baseSizeTrunc(shent.disk.sh_addr, shent.disk.sh_size);

    name = shent.name;
  }

  void createContent(ElfSegmentTableEntry!nBits phent) {
    assert(phent !is null);
    auto fhdr = ancestor!(ElfFileHeader!nBits);
    assert(fhdr !is null);

    fileExtent = phent.fileExtent;
    createSectionBytes!(MemoryMap!(Word!nBits))(fhdr.fileBytes, fileExtent, AddressSpace.FILE);

    preferredExtent = phent.preferredExtent;
  }

  void createContent(MemoryMap!(Word!nBits) mmap, Interval!(Word!nBits) interval) {
    preferredExtent = interval;
    createSectionBytes!(MemoryMap!(Word!nBits))(mmap, interval, AddressSpace.MEMORY);
  }

  // Section table entry associated with section, or null
  ElfSectionTableEntry!nBits sectionTableEntry() {
    auto fhdr = ancestor!(ElfFileHeader!nBits);
    assert(fhdr !is null);
    if (fhdr.sectionTable !is null) {
      foreach (shent; fhdr.sectionTable.entries[]) {
        if (shent.section is this)
          return shent;
      }
    }
    return null;
  }

  // Set properties based on sh_link and sh_info fields.  This function should only be called after the entire
  // ELF section table is parsed and all sections have been created. Subclasses will generally override this.
  void setLinkInfo(ElfSectionTableEntry!nBits shent, ParseLocation parentLoc) {};

  // Printable name of section
  override string printableName() {
    string[] parts;

    if (auto fhdr = ancestor!(ElfFileHeader!nBits)) {
      if (fhdr.sectionTable !is null)
        parts ~= fhdr.sectionTable.indexesOf(this).take(1).map!(i => "section " ~ to!string(i)).array;

      if (fhdr.segmentTable !is null)
        parts ~= fhdr.segmentTable.indexesOf(this).take(1).map!(i => "segment " ~ to!string(i)).array;
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
// A section table contains zero or more section table entries

class ElfSectionTable(size_t nBits): Ast {
  mixin AstNodeFeatures;

  struct AstChildren {
    AstList!(ElfSectionTableEntry!nBits) entries;
  }

  enum SectionIndex { SHN_UNDEF=0, SHN_LORESERVE=0xff00, SHN_LOPROC=0xff00, SHN_HIPROC=0xff1f, SHN_LOOS=0xff20,
                      SHN_HIOS=0xff3f, SHN_ABS=0xfff1, SHN_COMMON=0xfff2, SHN_XINDEX=0xffff, SHN_HIRESERVE=0xffff }

  Word!nBits startOffset;

  this() {
    entries = new AstList!(ElfSectionTableEntry!nBits);
  }

  void parse(ParseLocation parentLoc) {
    auto fhdr = ancestor!(ElfFileHeader!nBits);
    assert(fhdr !is null);

    startOffset = fhdr.disk.e_shoff;
    auto tableLoc = fhdr.fileLocation(fhdr.formatName ~ " section table", startOffset, parentLoc);
   
    // Table entry #0 is always SHT_NULL and usually zero filled. However, two fields are special:
    //   1. If sh_size is non-zero then it is the actual number of table entries. The file header's e_shnum field is
    //      normally zero in this case, and this happens when the total number of sections is >= SHN_LORESERVE (0xff00)
    //      since those section numbers have special meanings.
    //
    //   2. If sh_link is non-zero, then it's the index of the section table's string section and the file header should
    //      contain SHN_XINDEX.
    size_t nEntries = fhdr.disk.e_shnum;
    foreach (entryNumber; 0 .. max(1, nEntries)) {
      auto entryLoc = indexParseLocation(fhdr.formatName ~ " section table entry", entryNumber, tableLoc);
      auto entry = ElfSectionTableEntry!nBits.instance();
      entries.pushBack(entry);
      entry.parse(entryLoc);

      if (0 == entryNumber && entry.disk.sh_size > 0) {
        if (nEntries != 0)
          appendError(entryLoc, "entry #0 sh_size (" ~ entry.disk.sh_size.to!string ~ ")" ~
                      " and header e_shnum (" ~ fhdr.disk.e_shnum.to!string ~ ") are both non-zero");
        nEntries = entry.disk.sh_size; // standard says to use this if present
      }
    }
  }

  // Returns the indexes of all the section table entries that point to the specified section. The return value is an
  // input range.
  auto indexesOf(ElfSection!nBits section) {
    return entries[]
      .enumerate
      .filter!(a => a.value.section is section)
      .map!"a.index";
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class ElfSectionTableEntry(size_t nBits): Ast {
  mixin AstNodeFeatures;
  struct AstChildren {}

  // Optional section associated with this table entry. The section is not part of the AST here because we need to
  // support all of the following:
  //   1. A section created due to its description in this section table
  //   2. A section created due to its description in the segment (program header) table
  //   3. A section being described by both the section table and the segment table
  //   4. A section not appearing in either table (such as some in the .dynamic section in virtual memory)
  //
  // Therefore, sections are linked into the the AST as children of the ElfFileHeader instead.
  ElfSection!nBits section;

  enum SectionType : Elf!nBits.Word {
    SHT_NULL=0, SHT_PROGBITS=1, SHT_SYMTAB=2, SHT_STRTAB=3, SHT_RELA=4, SHT_HASH=5, SHT_DYNAMIC=6, SHT_NOTE=7,
    SHT_NOBITS=8, SHT_REL=9, SHT_SHLIB=10, SHT_DYNSYM=11, /*12  & 13 are missing*/ SHT_INIT_ARRAY=14,
    SHT_FINI_ARRAY=15, SHT_PREINIT_ARRAY=16, SHT_GROUP=17, SHT_SYMTAB_SHNDX=18,

    SHT_GNU_HASH = 0x6ffffff6,
    SHT_VERNEED =  0x6ffffffe,
    SHT_VERSYM =   0x6fffffff,
  
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
  size_t index;                 // index in section table
  Word!nBits startOffset;

  void parse(ParseLocation parentLoc) {
    auto fhdr = ancestor!(ElfFileHeader!nBits);
    assert(fhdr !is null);
    auto table = ancestor!(ElfSectionTable!nBits);
    assert(table !is null);

    index = table.entries.length - 1;
    assert(table.entries[index] is this);
    startOffset = cast(Word!nBits)(table.startOffset + index * Disk.sizeof);
    auto loc = fhdr.fileLocation(fhdr.formatName ~ " section table entry", startOffset, parentLoc);
    
    if (fhdr.disk.e_shentsize < Disk.sizeof)
      appendError(loc, "section table entry size (" ~ Disk.sizeof.to!string ~ ")" ~
                  " is larger than header e_shentsize (" ~ fhdr.disk.e_shentsize.to!string ~ ")");

    if (!fhdr.fileBytes.readObjectAt(startOffset, disk))
      appendError(loc, "short read");
    disk = disk.toNative(fhdr.byteOrder);

    Elf!nBits.Xword alignment = max(1, disk.sh_addralign);
    if (popcnt(alignment) > 1)
      appendError(loc, "sh_addralign (" ~ disk.sh_addralign.to!string ~ ") should be a power of two");
  }

  // Location of section within file
  Interval!(Elf!nBits.Off) fileExtent() @property {
    if (disk.sh_type == SectionType.SHT_NOBITS) {
      return Interval!(Elf!nBits.Off)();
    } else {
      return Interval!(Elf!nBits.Off).baseSizeTrunc(disk.sh_offset, disk.sh_size);
    }
  }

  // Preferred location of section within memory
  Interval!(Elf!nBits.Addr) preferredExtent() @property {
    if (disk.sh_flags.isSet(SectionFlags.SHF_ALLOC)) {
      return Interval!(Elf!nBits.Addr).baseSizeTrunc(disk.sh_addr, disk.sh_size);
    } else {
      return Interval!(Elf!nBits.Addr)();
    }
  }

  // Printable name of a section table entry, properly escaped
  string printableName() {
    if (section)
      return section.printableName;
    return "section " ~ to!string(index) ~ ", " ~ name.cEscape;
  }
}

