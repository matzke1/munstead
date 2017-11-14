module munstead.elf.interp;

import munstead.ast.base;
import munstead.core.exception;
import munstead.core.util;
import munstead.elf.files;
import munstead.elf.sections;
import munstead.elf.segments;
import munstead.elf.strings;
import std.conv: to;

// Represents the ".interp" section
class ElfInterpSection(size_t nBits): ElfSection!nBits {
  mixin AstNodeFeatures;
  struct AstChildren {}

  string interpreter;

  // Parse the ".interp" section from the section table
  void parse(ParseLocation parentLoc) {
    auto fhdr = ancestor!(ElfFileHeader!nBits);
    assert(fhdr !is null);
    assert(bytes !is null);

    auto loc = sectionLocation(fhdr.formatName ~ " interp section", 0, parentLoc);
    interpreter = bytes.stringAt(0, this, loc);
  }
}
