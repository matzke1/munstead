module munstead.elf.array;

import munstead.ast.base;
import munstead.core.exception;
import munstead.core.interval;
import munstead.core.mmap;
import munstead.core.util;
import munstead.core.wordtypes;
import munstead.elf.files;
import munstead.elf.sections;
import std.algorithm: map;
import std.array: array;
import std.conv: to;

// An array of elements read from a file or memory
class ElfArray(size_t nBits, T = Word!nBits): ElfSection!nBits {
  mixin AstNodeFeatures;
  struct AstChildren {}

  alias Value = T;
  Value[] values;

  void parse(ParseLocation parentLoc) {
    auto fhdr = ancestor!(ElfFileHeader!nBits);
    assert(fhdr !is null);
    auto loc = sectionLocation(fhdr.formatName ~ " array", 0, parentLoc);
    size_t nBytes = bytes.hull.length;
    size_t alignedSize = alignDown(nBytes, Value.sizeof);
    if (alignedSize < nBytes)
      appendError(loc, "array size (" ~ nBytes.to!string ~ ")" ~
                  " is not a multiple of the element size (" ~ Value.sizeof.to!string ~ ")");
    
    comment = "address list generated from virtual memory";
    values = cast(Value[]) bytes.bySegment.byBuffer(alignedSize).map!"a.buffer".front.array;
  }
}
