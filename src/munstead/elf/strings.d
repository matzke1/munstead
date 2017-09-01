module munstead.elf.strings;

import munstead.ast.base;
import munstead.elf.sections;

class ElfStringTable(size_t nBits): ElfSection!nBits {
  mixin AstNodeFeatures;
  struct AstChildren {}

  static ElfStringTable
  parse(MemoryMap)(MemoryMap file, ElfSectionTableEntry!nBits shent) {
    assert(shent !is null);
    ElfStringTable ret = new ElfStringTable;
    ret.initFromSectionTableEntry(file, shent);
    return ret;
  }
}
