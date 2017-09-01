module munstead.elf.interp;

import munstead.ast.base;
import munstead.elf.sections;
import munstead.elf.segments;
import std.conv: to;

// Represents the ".interp" section
class ElfInterpSection(size_t nBits): ElfSection!nBits {
  mixin AstNodeFeatures;
  struct AstChildren {}

  string interpreter;

  // Parse the ".interp" section from the section table
  static ElfInterpSection
  parse(MemoryMap)(MemoryMap file, ElfSectionTableEntry!nBits shent) {
    assert(shent !is null);
    auto ret = new ElfInterpSection;
    ret.initFromSectionTableEntry(file, shent);
    ret.parse();
    return ret;
  }

  // Parse the ".interp" section from the segment table
  static ElfInterpSection
  parse(MemoryMap)(MemoryMap file, ElfSegmentTableEntry!nBits phent) {
    assert(phent !is null);
    auto ret = new ElfInterpSection;
    ret.initFromSegmentTableEntry(file, phent);
    ret.parse();
    return ret;
  }

  private void parse() {
    interpreter = stringAt(0);
  }
}
