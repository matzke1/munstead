module munstead.core.byteorder;

import std.algorithm: swap;
import std.traits: isIntegral;

enum ByteOrder { BIG_ENDIAN, LITTLE_ENDIAN }

// Swap bytes in an array in place.
void swapEndian(ubyte[] array) {
  for (size_t i=0; 2*i < array.length; ++i)
    swap(array[i], array[$-(i+1)]);
}
  
// Swap bytes of an integral type. Note that it is not possible to swap the bytes of floating point types unless they're
// cast to an array of bytes first.
T swapEndian(T)(T val)
if (isIntegral!T) {
  static if (val.sizeof == 1) {
    return val;
  } else static if (val.sizeof == 2) {
    return cast(T)(((val & 0xff00) >> 8) | ((val & 0x00ff) << 8));
  } else static if (val.sizeof == 4) {
    import core.bitop: bswap;
    return cast(T)(bswap(val));
  } else static if (val.sizeof == 8) {
    import core.bitop: bswap;
    ulong hi = bswap(cast(uint)val);
    ulong lo = bswap(cast(uint)(val >> 32));
    return cast(T)((hi << 32) | lo);
  } else {
    assert(0);
  }
}

// Swap all integral data members of a struct. For arrays, bytes of each element are swapped by the elements themselves
// are not rearranged within the array.
T swapEndian(T)(ref const(T) val)
if (!isIntegral!T) {
  T ret;
  foreach (m; __traits(allMembers, T)) {
    static if (isIntegral!(typeof(__traits(getMember, val, m)))) {
      __traits(getMember, ret, m) = swapEndian(__traits(getMember, val, m));
    } else {
      __traits(getMember, ret, m) = __traits(getMember, val, m);
    }
  }
  return ret;
}

T fromLittleEndian(T)(T val) {
  version (LittleEndian) {
    return val;
  } else {
    return swapEndian(val);
  }
}

T fromBigEndian(T)(T val) {
  version(LittleEndian) {
    return swapEndian(val);
  } else {
    return val;
  }
}

T toNative(T)(T val, ByteOrder fromOrder) {
  final switch (fromOrder) {
    case ByteOrder.LITTLE_ENDIAN:
      return fromLittleEndian(val);
    case ByteOrder.BIG_ENDIAN:
      return fromBigEndian(val);
  }
}

T toLittleEdian(T)(ref const(T) val);
T toBigEndian(T)(ref const(T) val);
