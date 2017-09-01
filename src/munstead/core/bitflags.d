module munstead.core.bitflags;

import core.bitop: popcnt;
import std.conv;
import std.format;
import std.traits;

struct BitFlags(E)
if (is(E == enum)) {

  alias Value = E;
  alias Base = ulong;

  public Value value;

  this(Value v) {
    value = v;
  }

  // True if the specified value is a bit flag (i.e., has one bit set)
  static bool isFlag(Value flag) {
    return popcnt(flag) == 1;
  }

  // True if the specified value is a bit mask (more than one bit set)
  static bool isMask(Value flag) {
    return popcnt(flag) > 1;
  }

  // True if the specified value is the special zero flag (not a flag and not a mask)
  static bool isZero(Value flag) {
    return flag == 0;
  }

  // True if any of the bits in flag are set in the value.
  bool isSet(Value flag) const {
    return (value & flag) != 0;
  }

  // True if none of the bits in flag are set in the value.
  bool isClear(Value flag) const {
    return (value & flag) == 0;
  }
  
  // The flags that are set.
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
