module munstead.elf.notes;

import munstead.ast.base;
import munstead.core.byteorder;
import munstead.core.interval;
import munstead.core.mmap;
import munstead.core.wordtypes;
import munstead.core.util: alignUp;
import munstead.elf.files;
import munstead.elf.sections;
import munstead.elf.segments;
import munstead.elf.types;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class ElfNoteSection(size_t nBits): ElfSection!nBits {
  mixin AstNodeFeatures;
  struct AstChildren {
    AstList!(ElfNote!nBits) entries;
  }

  this() {
    entries = new AstList!(ElfNote!nBits);
  }

  static ElfNoteSection
  parse(MemoryMap)(MemoryMap file, ElfFileHeader!nBits fhdr, ElfSectionTableEntry!nBits shent) {
    assert(shent !is null);
    auto ret = new ElfNoteSection;
    ret.initFromSectionTableEntry(file, shent);
    ret.parse(fhdr);
    return ret;
  }

  static ElfNoteSection
  parse(MemoryMap)(MemoryMap file, ElfFileHeader!nBits fhdr, ElfSegmentTableEntry!nBits phent) {
    assert(phent !is null);
    auto ret = new ElfNoteSection;
    ret.initFromSegmentTableEntry(file, phent);
    ret.parse(fhdr);
    return ret;
  }

  void
  parse(ElfFileHeader!nBits fhdr) {
    size_t index = 0;
    Word!nBits offset = 0;
    while (offset < mmap.length) {
      auto note = ElfNote!nBits.parse(this, offset, fhdr.byteOrder);
      note.index = index++;
      entries.pushBack(note);
      offset += note.location.length;
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class ElfNote(size_t nBits): Ast {
  mixin AstNodeFeatures;
  struct AstChildren {}

  // Documentation says that these fields are either 4 bytes or 8 bytes depending on whether the file is ELFCLASS32 or
  // ELFCLASS64 (in the ELF file header), but it seems that even ELFCLASS64 files use 32-bit fields here.
  struct Disk {
    Word!32 namesz;
    Word!32 descsz;
    Word!32 type;
    // name -- NUL-terminated ASCII string padded to a multiple of 4 or 8 bytes
    // desc -- payload padded to a multiple of 4 or 8 bytes
  }

  alias Address = Word!nBits;

  Interval!Address location; // location of this entire note within the containing notes section
  size_t index; // index of this note within the containing notes section
  Disk hdr;
  string name;
  Interval!Address descLocation; // location of desc field (no padding) w.r.t. containing notes section
  ubyte[] desc;

  static ElfNote
  parse(ElfNoteSection!nBits notes, Word!nBits offset, ByteOrder byteOrder) {
    // Parse header
    auto ret = new ElfNote;
    notes.mmap.readObjectAt(offset, ret.hdr);
    ret.hdr = toNative(ret.hdr, byteOrder);
    ret.location = Interval!Address.baseSize(offset, Disk.sizeof); // name and desc are added later

    // Parse name
    if (ret.hdr.namesz > 0) {
      Word!nBits nameOffset = ret.location.greatest + 1;
      Address nameAlignedSize = alignUp(ret.hdr.namesz, 4); // 4-byte padding regardless of word size
      ret.name = notes.stringAt(nameOffset, ret.hdr.namesz);
      ret.location = Interval!Address.hull(ret.location.least, ret.location.greatest + nameAlignedSize);
    }

    // Parse desc
    if (ret.hdr.descsz > 0) {
      Address descOffset = ret.location.greatest + 1;
      Address descAlignedSize = alignUp(ret.hdr.descsz, 4); // 4-byte padding regardless of word size
      ret.descLocation = Interval!Address.baseSize(descOffset, ret.hdr.descsz);
      ret.location = Interval!Address.hull(ret.location.least, ret.location.greatest + descAlignedSize);
      ret.desc = notes.mmap.segmentsWithin(ret.descLocation).byBuffer(ret.descLocation.length).front.buffer;
    }

    return ret;
  }
}
