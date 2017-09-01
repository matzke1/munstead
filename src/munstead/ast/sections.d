module munstead.ast.sections;

import munstead.ast.base;
import munstead.core.interval;
import munstead.core.mmap;
import munstead.core.wordtypes;

// Some structure of a file. E.g., for ELF these are segments and sections
class AsmSection(size_t nBits): Ast {
  mixin AstNodeFeatures;
  struct AstChildren {}

  string name;

  Interval!(Word!nBits) fileExtent; // location within file
  Interval!(Word!nBits) preferredExtent; // preferred location in memory
  Interval!(Word!nBits) actualExtent; // actual location in memory

  // optional section-relative memory. This is the data for just this section mapped starting at zero.
  MemoryMap!(Word!nBits, ubyte) mmap;

  // Create the memory map for this section by copying it from another map.  The section-relative memory starts at
  // address zero.
  void createSectionMmap(MemoryMap)(MemoryMap srcMap, MemoryMap.Interval srcAddrs) {
    assert(srcMap !is null);
    mmap = new MemoryMap!(Word!nBits, ubyte);
    if (!srcAddrs.empty) {
      foreach (node; srcMap.segmentsWithin(srcAddrs)) {
	auto targetAddrs = MemoryMap.Interval.hull(node.interval.least - srcAddrs.least,
						   node.interval.greatest - srcAddrs.least);
	mmap.insert(targetAddrs, node.segment);
      }
    }
  }
}
