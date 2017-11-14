/** Treats enums as bit flags.
 *
 *  This is similar to std.typecons.BitFlags except uses an API with names rather than operators in order
 *  to improve readability of the code that uses it. */
module munstead.core.bitflags;

import core.bitop: popcnt;
import std.conv;
import std.format;
import std.traits;

/** Type that wraps an enum as a set of bit flags. */
struct BitFlags(E)
if (is(E == enum)) {

  alias Value = E;
  alias Base = ulong;

  /** Raw value, union of enums. */
  public Value value;

  /** Contruct with specified enum value. */
  this(Value v) {
    value = v;
  }

  /** Construct union of enums.
   *
   *  You could just as easily use the single-argument constructor and separate all the values with the `\` operator, but
   *  then you'd also need a cast. */
  this(Value[] v...) {
    foreach (bits; v)
      value = cast(E)(value | bits);
  }

  /** True if the specified value is a bit flag.
   *
   *  A bit flag is an enum constant that has only a single bit set.  Constants with other than exactly one bit set are
   *  non-flags (zero), masks (multiple bits), or unions of flags (multiple bits). */
  static bool isFlag(Value flag) {
    return popcnt(flag) == 1;
  }

  /** True if the specified value is a bit mask.
   *
   *  A mask is any constant with more than one bit set, including unions of single-bit flags. */
  static bool isMask(Value flag) {
    return popcnt(flag) > 1;
  }

  /** True if the specified value is the special zero flag.
   *
   *  Such a constant is neither a flag nor a mask. */
  static bool isZero(Value flag) {
    return flag == 0;
  }

  /** True if no bits are set in this vector. */
  bool empty() const {
    return 0 == value;
  }

  /** True if any of the bits in flag are set in this vector. */
  bool isSet(Value flag) const {
    return (value & flag) != 0;
  }

  /** True if none of the bits in flag are set in this vector. */
  bool isClear(Value flag) const {
    return (value & flag) == 0;
  }

  /** Union of specified values with this vector.
   *
   *  Creates a new bit vector whose value is the union of this vector and the specified values. */
  BitFlags including(Value[] values...) const {
    BitFlags ret = this;
    foreach (v; values)
      ret.value = cast(Value)(ret.value | v);
    return ret;
  }

  /** ditto */
  BitFlags including(BitFlags[] others...) const {
    BitFlags ret = this;
    foreach (other; others)
      ret.value = cast(Value)(ret.value | other.value);
    return ret;
  }

  /** Difference of this vector and specified values.
   *
   *  Returns a new vector whose value is the same as this vector but with all the specified bits removed. */
  BitFlags excluding(Value[] values...) const {
    BitFlags ret = this;
    foreach (v; values)
      ret.value = cast(Value)(ret.value & ~v);
    return ret;
  }

  /** ditto */
  BitFlags excluding(BitFlags[] others...) const {
    BitFlags ret = this;
    foreach (other; others)
      ret.value = cast(Value)(ret.value & ~other.value);
    return ret;
  }

  /** Array of the individual set flags.
   *
   *  Returns an array of all the bit flags that are set in this vector. It returns only those bits that have
   *  names in the enum. */
  Value[] areSet() const @property {
    Value[] ret;
    Base remaining = value;

    foreach (flag; EnumMembers!E) {
      if (isFlag(flag) && (remaining & flag) != 0) {
        ret ~= [ flag ];
        remaining &= ~flag;
      }
    }
    if (remaining != 0)
      ret ~= [ cast(Value) remaining ];
    return ret;
  }

  /** Return those bits that don't correspond to bit flags. */
  Value nonFlagsSet() const {
    return excluding(areSet()).value;
  }
    

  /** Convert this vector to a string.
   *
   *  Returns a string that contains the names of all set bits separated by a `|` symbol, and followed by
   *  an optional enum cast for the remaining bits that don't correspond to named enum flags. */
  void toString(scope void delegate(const(char)[]) sink, FormatSpec!char fmt) const {
    size_t nfound = 0;
    foreach (flag; areSet()) {
      if (nfound++ > 0)
        sink("|");
      sink(to!string(flag));
    }
    if (0 == nfound)
      sink("0");
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
unittest {} // to prevent the following tests from being documented

unittest {
  import std.algorithm;
  import std.stdio;
  import std.conv;
  
  writeln("unit tests: ", __FILE__);

  enum Color { NONE = 0, RED = 1, GREEN = 2, BLUE = 4, WHITE = 8, BLACK = 16 }

  alias ColorFlags = BitFlags!Color;

  assert(is(ColorFlags.Value == Color));

  with (Color) {
    assert(ColorFlags.isFlag(RED));
    assert(ColorFlags.isFlag(BLACK));
    assert(!ColorFlags.isFlag(RED|GREEN));
    assert(!ColorFlags.isFlag(NONE));

    assert(!ColorFlags.isMask(RED));
    assert(!ColorFlags.isMask(GREEN));
    assert(ColorFlags.isMask(RED|GREEN));
    assert(!ColorFlags.isMask(NONE));

    assert(!ColorFlags.isZero(RED));
    assert(ColorFlags.isZero(NONE));

    const ColorFlags cf0;
    assert(cf0.value == NONE, cf0.value.to!string(16));
    assert(cf0.empty);

    const cf1 = ColorFlags(RED);
    assert(cf1.value == RED, cf1.value.to!string(16));

    const cf2 = ColorFlags(RED | GREEN);
    assert(cf2.value == (RED | GREEN), cf2.value.to!string(16));

    const cf3 = ColorFlags(GREEN, BLUE);
    assert(cf3.value == (GREEN | BLUE), cf3.value.to!string(16));
    assert(cf3.isSet(GREEN));
    assert(!cf3.isClear(GREEN));
    assert(!cf3.isSet(RED));
    assert(cf3.isClear(RED));
    assert(cf3.areSet.equal([GREEN, BLUE]), cf3.areSet.to!string);

    const cf4 = cf3.including(BLUE);
    assert(cf3 == cf4, cf4.value.to!string(16));

    const cf5 = cf3.including(BLACK, WHITE);
    assert(cf5.value == (GREEN | BLUE | BLACK | WHITE), cf5.value.to!string(16));

    const cf6 = cf5.excluding(BLUE, WHITE);
    assert(cf6.value == (GREEN | BLACK), cf6.value.to!string(16));

    auto cf7 = cf6.including(cast(Color) 0x20);
    assert(cf7.to!string == "GREEN|BLACK|cast(Color)32", cf7.to!string);
  }

}
