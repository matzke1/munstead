module munstead.elf.strings;

import munstead.ast.base;
import munstead.core.exception;
import munstead.core.interval;
import munstead.core.mmap;
import munstead.core.wordtypes;
import munstead.elf.files;
import munstead.elf.sections;
import std.conv: to;

class ElfStringTable(size_t nBits): ElfSection!nBits {
  mixin AstNodeFeatures;
  struct AstChildren {}

  void parse(ParseLocation parentLoc) {
    auto fhdr = ancestor!(ElfFileHeader!nBits);
    assert(fhdr !is null);
    assert(bytes !is null);
  }

  auto stringAt(Word!nBits offset, Ast node, ParseLocation loc, size_t maxBytes = 1024*1024*1024) {
    assert(bytes !is null);
    return bytes.stringAt(offset, node, loc, maxBytes);
  }
}
