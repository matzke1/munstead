/** API for working with local regions of address spaces. */
module munstead.ast.sections;

import munstead.ast.base;
import munstead.core.exception;
import munstead.core.interval;
import munstead.core.mmap;
import munstead.core.util;
import munstead.core.wordtypes;


/** Represents an address space of file or memory.
 *
 *  A section is an address space mapped onto a file or virtual memory or both. For instance, an ELF string table
 *  is a section that appears at a particular offset in the file and has a specific size, and strings are indicated
 *  by their offset with respect to the beginning of the section. Reads and writes to the section use section-local
 *  addresses and are checked to ensure they don't extend beyond the limits of the section. */
class AsmSection(size_t nBits): Ast {
  mixin AstNodeFeatures;
  struct AstChildren {}

  /** Optional name of section.
   *
   *  Section names contain arbitrary file data and should always be escaped before displaying them. */
  string name;

  /** Optional comment.
   *
   *  Comments are arbitrary text assigned by the library (or user) and are assumed to contain only printable
   *  characters. */
  string comment;

  /** Optional location of section within file.
   *
   *  If this section contains data from a file, then this data member should indicate where such data came from in terms
   *  of file offsets. */
  Interval!(Word!nBits) fileExtent;

  /** Optional location of section in memory.
   *
   *  If the section has a preferred address in memory, then this data member should describe it.  This would be used,
   *  for instance, by sections created from some entries in the ELF program header table since those entries contain
   *  information about where certain parts of the file are mapped in memory. */
  Interval!(Word!nBits) preferredExtent;

  /** Optional location of section in memory.
   *
   *  This data member describes where a section's address space is located in virtual memory in the final memory
   *  map. */
  Interval!(Word!nBits) actualExtent;

  /** Optional section-relative memory.
   *
   *  This is the data for just this section mapped starting at zero. It's normally a reference to some part of a file,
   *  but may contain non-file data as well (such as zero padding).  Sometimes it's useful to know whether the bytes
   *  represent part of virtual memory or part of a file, so the "addressSpace" data
   *  member can be used for that. */
  MemoryMap!(Word!nBits, ubyte) bytes;

  /** From whence does content come.
   *
   *  Sometimes it's useful to know whether the bytes of this section represent part of virtual memory or part of a file,
   *  and this data member holds that info.   One thing the addressSpace member is used for is printing error messages. */
  AddressSpace addressSpace;

  /** Create content by incorporating it from somewhere else.
   *
   *  Creates the local-content `bytes` memory map for this section by copying it from another map.  The data within the
   *  map is not actually copied, only the descriptors that point to that data are copied. Section-local content always
   *  starts at local address zero. */
  void createSectionBytes(MemoryMap)(MemoryMap srcMap, MemoryMap.Interval srcAddrs, AddressSpace space) {
    assert(srcMap !is null);
    bytes = new MemoryMap!(Word!nBits, ubyte);
    addressSpace = space;
    if (!srcAddrs.empty) {
      foreach (node; srcMap.segmentsWithin(srcAddrs)) {
        auto targetAddrs = MemoryMap.Interval.hull(node.interval.least - srcAddrs.least,
                                                   node.interval.greatest - srcAddrs.least);
        bytes.insert(targetAddrs, node.segment);
      }
    }
  }

  /** Specimen name for printing.
   *
   *  Returns a detailed name suitable for printing in error messages, tables, etc.  The returned string might consist
   *  of the section name and/or command and its location in various tables. The returned string will have ony printable
   *  characters. */
  abstract string printableName();

  /** Returns a location record for error reporting.
   *
   *  Any parser or analysis that needs to report nested error locations (like error in symbol, at a certain index in
   *  a symbol table, which is at a certain address in a symbol table section, which is at a certain address in the
   *  specimen's file) can use this method to push a new record onto the location chain.
   *
   *  Params:
   *    what = A string describing the entity at this location, such as "ELF string table".
   *    localAddr = A section-local address for this location.
   *    parent = The optional parent location in which this new location is nested. The nesting can be logical; it
   *             need not be based on addresses. */
  ParseLocation sectionLocation(string what, Word!nBits localAddr, ParseLocation parent) {
    auto global = AddressSpace.FILE == addressSpace ? fileExtent : preferredExtent;
    return new MapParseLocation!(MemoryMap!(Word!nBits))(what, addressSpace, bytes, global, localAddr,
                                                             printableName, parent);
  }
}
