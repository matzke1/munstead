module munstead.core.byteorder;

import std.algorithm: swap;
import std.traits: isIntegral;

/** Order of bytes in multi-byte values. */
enum ByteOrder {
  BIG_ENDIAN,                   /** Lowest memory address holds most significant byte. */
  LITTLE_ENDIAN                 /** Lowest memory address holds least significant byte. */
}

/** Swap bytes of array elements in place. */
void swapEndian(ubyte[] array) {
  for (size_t i=0; 2*i < array.length; ++i)
    swap(array[i], array[$-(i+1)]);
}
  
/** Swap bytes of an integral type.
 *
 * Note that it is not possible to swap the bytes of floating point types unless they're cast to an array of bytes first. */
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

/** Swap all integral data members of a struct.
 *
 *  For arrays, bytes of each element are swapped by the elements themselves are not rearranged within the array. */
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

/** Convert a value from little-endian to host order. */
T fromLittleEndian(T)(T val) {
  version (LittleEndian) {
    return val;
  } else {
    return swapEndian(val);
  }
}

/** Convert a value from big-endian to host order. */
T fromBigEndian(T)(T val) {
  version(LittleEndian) {
    return swapEndian(val);
  } else {
    return val;
  }
}

/** Convert a value of specified order to host order. */
T toNative(T)(T val, ByteOrder fromOrder) {
  final switch (fromOrder) {
    case ByteOrder.LITTLE_ENDIAN:
      return fromLittleEndian(val);
    case ByteOrder.BIG_ENDIAN:
      return fromBigEndian(val);
  }
}

/** Convert a value from host order to little-endian. */
T toLittleEdian(T)(ref const(T) val);

/** Convert a value from host order to big-endian. */
T toBigEndian(T)(ref const(T) val);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
unittest {} // to prevent the following unit test from being documented.

unittest {
  import std.conv;
  import std.stdio;
  writeln("unit tests: ", __FILE__);

  uint i1 = 0x11223344;
  auto i2 = i1.swapEndian;
  assert(is(typeof(i2) == uint), typeof(i2).stringof);
  assert(i2 == 0x44332211, i2.to!string(16));

  struct S1 {
    ulong a;
    uint b;
    ushort c;
    ubyte d, e;
  }
  const s1 = S1(0x1122334455667788, 0x99aabbcc, 0xeeff, 0x11, 0x22);
  const s2 = s1.swapEndian;
  assert(s2.a == 0x8877665544332211, s2.a.to!string(16));
  assert(s2.b == 0xccbbaa99, s2.b.to!string(16));
  assert(s2.c == 0xffee, s2.c.to!string(16));
  assert(s2.d == 0x11, s2.d.to!string(16));
  assert(s2.e == 0x22, s2.e.to!string(16));
}
