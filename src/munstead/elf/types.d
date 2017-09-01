module munstead.elf.types;

template Elf(size_t nBits) {
  static if (nBits == 32) {
    alias Addr = uint;
    alias Off = uint;
    alias Half = ushort;
    alias Word = uint;
    alias SWord = int;
    alias Xword = uint;
    alias Sxword = int;
  } else {
    static assert(nBits == 64);
    alias Addr = ulong;
    alias Off = ulong;
    alias Half = ushort;
    alias Word = uint;
    alias SWord = int;
    alias Xword = ulong;
    alias Sxword = long;
  }
}
