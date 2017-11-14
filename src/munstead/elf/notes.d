module munstead.elf.notes;

import munstead.ast.base;
import munstead.core.byteorder;
import munstead.core.exception;
import munstead.core.interval;
import munstead.core.mmap;
import munstead.core.wordtypes;
import munstead.core.util;
import munstead.elf.files;
import munstead.elf.sections;
import munstead.elf.segments;
import munstead.elf.strings;
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

  void parse(ParseLocation parentLoc) {
    auto fhdr = ancestor!(ElfFileHeader!nBits);
    assert(fhdr !is null);

    auto tableLoc = sectionLocation(fhdr.formatName ~ " notes section", 0, parentLoc);

    size_t tableSize = bytes.hull.length;
    Word!nBits offset = 0;
    for (size_t idx = 0; offset < tableSize; ++idx) {
      auto entryLoc = indexParseLocation(fhdr.formatName ~ " note", idx, tableLoc);
      auto entry = ElfNote!nBits.instance();
      entries.pushBack(entry);
      entry.parse(entryLoc);
      offset += entry.location.length;
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
  Disk disk;
  string name;
  Interval!Address descLocation; // location of desc field (no padding) w.r.t. containing notes section
  ubyte[] desc;

  void parse(ParseLocation parentLoc) {
    auto fhdr = ancestor!(ElfFileHeader!nBits);
    assert(fhdr !is null);
    auto table = ancestor!(ElfNoteSection!nBits);
    assert(table !is null);

    index = table.entries.length - 1;
    assert(table.entries[index] is this);
    Word!nBits start = index > 0 ? table.entries[index-1].location.greatest + 1 : 0;
    auto loc = table.sectionLocation(fhdr.formatName ~ " note", start, parentLoc);
   
    table.bytes.readObjectAt(start, disk);
    disk = disk.toNative(fhdr.byteOrder);
    location = Interval!Address.baseSizeTrunc(start, Disk.sizeof); // name and desc are added later

    // Parse name
    if (disk.namesz > 0) {
      Word!nBits nameOffset = location.greatest + 1;
      Address nameAlignedSize = alignUp(disk.namesz, 4); // 4-byte padding regardless of word size
      name = table.bytes.stringAt(nameOffset, this, loc, disk.namesz);
      location = Interval!Address.hull(location.least, location.greatest + nameAlignedSize);
    }

    // Parse desc
    if (disk.descsz > 0) {
      Address descOffset = location.greatest + 1;
      Address descAlignedSize = alignUp(disk.descsz, 4); // 4-byte padding regardless of word size
      descLocation = Interval!Address.baseSizeTrunc(descOffset, disk.descsz);
      location = Interval!Address.hull(location.least, location.greatest + descAlignedSize);
      desc = table.bytes.segmentsWithin(descLocation).byBuffer(descLocation.length).front.buffer;
    }
  }
}
