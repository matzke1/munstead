module munstead.core.wordtypes;

template Word(size_t nBits) {
  static if (nBits == 16) {
    alias Word = ushort;
  } else static if (nBits == 32) {
    alias Word = uint;
  } else static if (nBits == 64) {
    alias Word = ulong;
  } else {
    static assert(0);
  }
}
