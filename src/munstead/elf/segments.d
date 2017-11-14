module munstead.elf.segments;

import core.bitop: popcnt;
import munstead.ast.base;
import munstead.core.bitflags;
import munstead.core.byteorder;
import munstead.core.exception;
import munstead.core.interval;
import munstead.core.util;
import munstead.core.wordtypes;
import munstead.elf.files;
import munstead.elf.sections;
import munstead.elf.types;
import std.algorithm: filter, map, max;
import std.conv: to;
import std.range: enumerate;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// A segment table (a.k.a., program header) contains zero or more segment table entries
class ElfSegmentTable(size_t nBits): Ast {
  mixin AstNodeFeatures;

  struct AstChildren {
    AstList!(ElfSegmentTableEntry!nBits) entries;
  }

  Word!nBits startOffset;

  this() {
    entries = new AstList!(ElfSegmentTableEntry!nBits);
  }

  void parse(ParseLocation parentLoc) {
    auto fhdr = ancestor!(ElfFileHeader!nBits);
    assert(fhdr !is null);

    startOffset = fhdr.disk.e_phoff;
    auto tableLoc = fhdr.fileLocation(fhdr.formatName ~ " segment table", startOffset, parentLoc);

    size_t nEntries = fhdr.disk.e_phnum;
    foreach (entryNumber; 0 .. nEntries) {
      auto entryLoc = indexParseLocation(fhdr.formatName ~ " segment table entry", entryNumber, tableLoc);
      auto entry = ElfSegmentTableEntry!nBits.instance();
      entries.pushBack(entry);
      entry.parse(entryLoc);
    }
  }

  // Returns the indexes of all the program header entries that point to the specified section. The return value is an
  // input range.
  auto indexesOf(ElfSection!nBits section) {
    return entries[]
      .enumerate
      .filter!(a => a.value.section is section)
      .map!"a.index";
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// One entry of a segment table (a.k.a., program header table entry)
class ElfSegmentTableEntry(size_t nBits): Ast {
  mixin AstNodeFeatures;
  struct AstChildren {}

  // Optional section associated with this table entry. The section is not part of the AST here because we need to
  // support all of the following:
  //   1. A section created due to its description in this segment (program header) table
  //   2. A section created due to its description in the section table
  //   3. A section being described by both the section table and the segment table
  //   4. A section not appearing in either table (such as some in the .dynamic section in virtual memory)
  //
  // Therefore, sections are linked into the the AST as children of the ElfFileHeader instead.
  ElfSection!nBits section;     // section created for this table entry

  enum SegmentType : Elf!nBits.Word {
    PT_NULL=0,
    PT_LOAD=1,
    PT_DYNAMIC=2,
    PT_INTERP=3,
    PT_NOTE=4,
    PT_SHLIB=5,
    PT_PHDR=6,
    PT_TLS=7,
      
    PT_GNU_EH_FRAME = 0x6474e550,
    PT_GNU_STACK    = 0x6474e551,
    PT_GNU_RELRO    = 0x6474e552, // pages to be mapped read-only after dynamic relocations are applied
      
    PT_LOOS=0x60000000, PT_HIOS=0x6fffffff, PT_LOPROC=0x70000000, PT_HIPROC=0x7fffffff
  }

  enum SegmentFlags : Elf!nBits.Word {
    PF_X=1, PF_W=2, PF_R=4, PF_MASKOS=0x0ff00000, PF_MASKPROC=0xf0000000
  }

  struct Disk {
    align(1):
    SegmentType p_type;         // type of segment
    static if (64 == nBits)
      BitFlags!SegmentFlags p_flags; // segment dependent flags
    Elf!nBits.Off p_offset;     // file offset for start of segment
    Elf!nBits.Addr p_vaddr;     // desired mapped virtual address
    Elf!nBits.Addr p_paddr;     // physical address, where supported
    Elf!nBits.Xword p_filesz;   // number of bytes in the file
    Elf!nBits.Xword p_memsz;    // number of bytes mapped in virtual memory
    static if (32 == nBits)
      BitFlags!SegmentFlags p_flags; // see above for 64-bit struct
    Elf!nBits.Xword p_align;    // alignment in file and memory, always a power of two. 0 and 1 both mean none
  }

  Disk disk;
  size_t index;                 // index in segment table
  Word!nBits startOffset;

  // Parse an entry from some part of the file
  void parse(ParseLocation parentLoc) {
    auto fhdr = ancestor!(ElfFileHeader!nBits);
    assert(fhdr !is null);
    auto table = ancestor!(ElfSegmentTable!nBits);
    assert(table !is null);

    index = table.entries.length - 1;
    assert(table.entries[index] is this);
    startOffset = cast(Word!nBits)(table.startOffset + index * Disk.sizeof);
    auto loc = fhdr.fileLocation(fhdr.formatName ~ " segment table entry", startOffset, parentLoc);

    if (fhdr.disk.e_phentsize < Disk.sizeof)
      appendError(loc, "segment table entry size (" ~ Disk.sizeof.to!string ~ ")" ~
                  " is larger than header e_phentsize (" ~ fhdr.disk.e_phentsize.to!string ~ ")");

    if (!fhdr.fileBytes.readObjectAt(startOffset, disk))
      appendError(loc, "short read");
    disk  = disk.toNative(fhdr.byteOrder);

    Elf!nBits.Xword alignment = max(1, disk.p_align);
    if (popcnt(alignment) > 1)
      appendError(loc, "p_align (" ~ disk.p_align.to!string ~ ") should be a power of two");

    if (disk.p_vaddr % alignment != disk.p_offset % alignment)
      appendError(loc, "inconsistent alignment (" ~ alignment.hexStr ~")" ~
                  " for p_vaddr (" ~ disk.p_vaddr.hexStr ~ ")" ~
                  " and p_offset (" ~ disk.p_offset.hexStr ~")");
  }

  // Location of segment within file
  Interval!(Elf!nBits.Off) fileExtent() @property {
    return Interval!(Elf!nBits.Off).baseSizeTrunc(disk.p_offset, disk.p_filesz);
  }

  Interval!(Elf!nBits.Addr) preferredExtent() @property {
    return Interval!(Elf!nBits.Addr).baseSizeTrunc(disk.p_vaddr, disk.p_memsz);
  }

  string printableName() {
    return "segment " ~ to!string(index);
  }
}

