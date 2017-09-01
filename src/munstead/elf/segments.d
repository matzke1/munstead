module munstead.elf.segments;

import core.bitop: popcnt;
import munstead.ast.base;
import munstead.core.bitflags;
import munstead.core.byteorder;
import munstead.core.exception;
import munstead.core.interval;
import munstead.elf.files;
import munstead.elf.sections;
import munstead.elf.types;
import std.algorithm: max;
import std.conv: to;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// A segment table (a.k.a., program header) contains zero or more segment table entries
class ElfSegmentTable(size_t nBits): Ast {
  mixin AstNodeFeatures;

  struct AstChildren {
    AstList!(ElfSegmentTableEntry!nBits) entries;
  }

  this() {
    entries = new AstList!(ElfSegmentTableEntry!nBits);
  }

  // Create a new segment table by parsing the table entries starting at the specified file offset.
  static ElfSegmentTable
  parse(MemoryMap)(MemoryMap file, ElfFileHeader!nBits fhdr) {
    assert(fhdr !is null);
    enum me = "ELF-" ~ to!string(nBits) ~ " segment table";
    auto ret = new ElfSegmentTable;

    MemoryMap.Address firstOffset = fhdr.disk.e_phoff;
    size_t nEntries = fhdr.disk.e_phnum;
    size_t bytesPerEntry = fhdr.disk.e_phentsize;

    foreach (entryNumber; 0 .. nEntries) {
      MemoryMap.Address entryOffset = cast(MemoryMap.Address)(firstOffset + entryNumber * bytesPerEntry);
      auto entry = ElfSegmentTableEntry!nBits.parse!MemoryMap(file, entryOffset, bytesPerEntry, fhdr.byteOrder);
      entry.index = entryNumber;
      if (entry.errors.length > 0)
        ret.appendError(new SyntaxError(me~" problem parsing entry #" ~ to!string(entryNumber), file.name, entryOffset));
      ret.entries.pushBack(entry);
    }

    return ret;
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// One entry of a segment table (a.k.a., program header table entry)
class ElfSegmentTableEntry(size_t nBits): Ast {
  mixin AstNodeFeatures;
  struct AstChildren {
    ElfSection!nBits section; 	// section created for this table entry
  }

  enum SegmentType : Elf!nBits.Word {
    PT_NULL=0, PT_LOAD=1, PT_DYNAMIC=2, PT_INTERP=3, PT_NOTE=4, PT_SHLIB=5, PT_PHDR=6,
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
  size_t index; 		// index in segment table

  // Parse an entry from some part of the file
  static ElfSegmentTableEntry
  parse(MemoryMap)(MemoryMap file, MemoryMap.Address offset, size_t bytesPerEntry, ByteOrder byteOrder) {
    enum me = "ELF-" ~ to!string(nBits) ~ " segment table entry";
    auto ret = new ElfSegmentTableEntry;

    if (bytesPerEntry < ret.disk.sizeof)
      ret.appendError(new SyntaxError(me ~ " actual size (" ~ to!string(ret.disk.sizeof) ~ ")" ~
                                      " is larger than header-specified size (" ~ to!string(bytesPerEntry) ~ ")",
                                      file.name, offset));

    if (!file.readObjectAt(offset, ret.disk))
      ret.appendError(new SyntaxError(me ~ " short read", file.name, offset));
    ret.disk  = ret.disk.toNative(byteOrder);

    Elf!nBits.Xword alignment = max(1, ret.disk.p_align);
    if (popcnt(alignment) > 1)
      ret.appendError(new SyntaxError(me ~ " p_align (" ~ to!string(ret.disk.p_align) ~ ") should be a power of two",
                                      file.name, offset));
    if (ret.disk.p_vaddr % alignment != ret.disk.p_offset % alignment)
      ret.appendError(new SyntaxError(me ~ " inconsistent alignment (0x" ~ to!string(alignment, 16) ~")" ~
                                      " for p_vaddr (0x" ~ to!string(ret.disk.p_vaddr, 16) ~ ")" ~
                                      " and p_offset (0x" ~ to!string(ret.disk.p_offset, 16) ~")",
                                      file.name, offset));
    return ret;
  }

  // Location of segment within file
  Interval!(Elf!nBits.Off) fileExtent() @property {
    return Interval!(Elf!nBits.Off).baseSize(disk.p_offset, disk.p_filesz);
  }

  Interval!(Elf!nBits.Addr) preferredExtent() @property {
    return Interval!(Elf!nBits.Addr).baseSize(disk.p_vaddr, disk.p_memsz);
  }
}

