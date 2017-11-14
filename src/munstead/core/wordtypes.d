module munstead.core.wordtypes;

/** A clearer way to write fixed size integral types.
 *
 *  When writing code for binary analysis, and especially when defining data structures that must match documented
 *  file formats, the size of data members is of utmost importance. One incorrect size means all remaining data
 *  members will be parsed incorrectly.  It is too easy to overlook a wrong size when you have a long list of ubyte,
 *  ushort, uint, and ulong members, so it's better to include the size as part of the name of the type, like
 *  in C++: uint8_t, uint16_t, uint32_t, and uint64_t.
 *
 *  This template improves further by using consistent capitalization and, because nBits is so frequently a
 *  template parameter in other parts of this library, we make it a template parameter here also. */
template Word(size_t nBits) {
  static if (nBits == 8) {
    alias Word = ubyte;
  } else static if (nBits == 16) {
    alias Word = ushort;
  } else static if (nBits == 32) {
    alias Word = uint;
  } else static if (nBits == 64) {
    alias Word = ulong;
  } else {
    static assert(0);
  }
}

unittest {
  import std.stdio;
  writeln("unit tests: ", __FILE__);

  assert(is(Word!8 == ubyte));
  assert(is(Word!16 == ushort));
  assert(is(Word!32 == uint));
  assert(is(Word!64 == ulong));
}
