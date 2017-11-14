/** Miscellaneous things. */
module munstead.core.util;

import munstead.core.interval;
import std.format: format;
import std.range: ElementType, isInputRange;

// This is here because DMD won't run unit tests if they're in a library, so we need some way to
// visually check that they're being run.
unittest {
  import std.stdio;
  writeln("unit tests: ", __FILE__);
}

/** Type of address space. */
enum AddressSpace {
  FILE,                         /** Addresses are really offsets into a file. */
  MEMORY                        /** Addresses are virtual memory addresses. */
};

/** True if the value is an enum member. */
bool isEnumMember(E, I)(I value) {
  import std.traits: EnumMembers;
  foreach (member; EnumMembers!E) {
    if (member == value)
      return true;
  }
  return false;
}

///
unittest {
  enum Color { RED, GREEN, BLUE }

  static assert(isEnumMember!Color(Color.RED));
  static assert(isEnumMember!Color(1));
  static assert(!isEnumMember!Color(3));
}

/** Round value up to the next multiple of alignment.
 *
 *  An alignment less than one is treated as if it were one. */
T alignUp(T)(T value, T alignment) {
  return alignment <= 1 ? value : alignment * ((value + alignment - 1) / alignment);
}

///
unittest {
  assert(alignUp(0,3) == 0);
  assert(alignUp(1,3) == 3);
  assert(alignUp(2,3) == 3);
  assert(alignUp(3,3) == 3);
  assert(alignUp(4,3) == 6);

  assert(alignUp(5,1) == 5);
  assert(alignUp(5,0) == 5);
}

/** Round value down to the next multiple of alignment.
 *
 *  An alignment less than one is treated as if it were one. */
T alignDown(T)(T value, T alignment) {
  return alignment <= 1 ? value : alignment * (value / alignment);
}

///
unittest {
  assert(alignDown(0,3) == 0);
  assert(alignDown(1,3) == 0);
  assert(alignDown(2,3) == 0);
  assert(alignDown(3,3) == 3);
  assert(alignDown(4,3) == 3);

  assert(alignUp(5,1) == 5);
  assert(alignUp(5,0) == 5);
}

/** Escape a C string and optionally add double quotes. */
string cEscape(string input, bool quote = true) {
  string ret;
  foreach (ch; input) {
    switch (ch) {
    case '\a':
      ret ~= "\\a";
      break;
    case '\b':
      ret ~= "\\b";
      break;
    case '\t':
      ret ~= "\\t";
      break;
    case '\n':
      ret ~= "\\n";
      break;
    case '\v':
      ret ~= "\\v";
      break;
    case '\f':
      ret ~= "\\f";
      break;
    case '\r':
      ret ~= "\\r";
      break;
    case '"':
      ret ~= "\\\"";
      break;
    default:
      if (ch < ' ' || ch > '~') {
        ret ~= format!"\\%03o"(ch);
      } else {
        ret ~= ch;
      }
      break;
    }
  }
  if (quote)
    ret = "\"" ~ ret ~ "\"";
  return ret;
}

///
unittest {
  assert("abc".cEscape == `"abc"`);
  assert("a\nb\000".cEscape == `"a\nb\000"`);
  assert("abc".cEscape(false) == "abc");
}
  
/** Convert an address to a fixed-length hex string with underscores for readability. */
string hexStr(uint value) {
  return format!"0x%08x"(value);
}

/** ditto */
string hexStr(ulong value) {
  return format!"0x%08x_%08x"(value >> 32, value & 0xffffffff);
}

/** ditto */
string hexStr(T)(Interval!T interval) {
  if (interval.empty) {
    return "[empty]";
  } else {
    return "[" ~ interval.least.hexStr ~ ", " ~ interval.greatest.hexStr ~ "]";
  }
}
  
///
unittest {
  assert(32.hexStr == "0x00000020");
  assert(32uL.hexStr == "0x00000000_00000020");
}

/** Given a range, return the front element or a default value. */
ElementType!Range frontOrDflt(Range)(Range range)
if (isInputRange!Range) {
  return range.empty ? ElementType!Range.init : range.front;
}

///
unittest {
  import std.range;

  struct S { int x = 100; }
  S s;
  s.x = 1;

  auto r1 = s.only;
  assert(r1.frontOrDflt.x == 1);
  r1.popFront();
  assert(r1.frontOrDflt.x == 100);

  class C {}
  auto c = new C;
  auto r2 = c.only;

  assert(r2.frontOrDflt is c);
  r2.popFront();
  assert(r2.empty);
  assert(r2.frontOrDflt is null);
}

/** Given a range of objects, return the front or something else. */
ElementType!Range frontOrElse(Range, T)(Range range, T somethingElse)
if (isInputRange!Range) {
  return range.empty ? somethingElse : range.front;
}

///
unittest {
  import std.range;
  auto range = 1.only;

  assert(range.frontOrElse(911) == 1);
  range.popFront();
  assert(range.frontOrElse(911) == 911);
}

/** Print a number followed by a plural noun phrase. If the number is one, then make the phrase singular by following some
 *  common English rules. */
string plural(T)(T n, string pluralNoun, string singularNoun="") {
  import std.algorithm;
  import std.conv;

  if (n != 1)
    return n.to!string ~ " " ~ pluralNoun;

  if (singularNoun != "")
    return "1 " ~ singularNoun;

  if (pluralNoun.endsWith("vertices"))                   // vertices => vertex
    return "1 " ~ pluralNoun[0 .. $-4] ~ "ex";

  if (pluralNoun.endsWith("sses"))                       // addresses => address
    return "1 " ~ pluralNoun[0 .. $-2];
  
  if (pluralNoun.endsWith("ies"))                        // entries => entry
    return "1 " ~ pluralNoun[0 .. $-3] ~ "y";

  if (pluralNoun.endsWith("s"))                          // edges => edge
    return "1 " ~ pluralNoun[0 .. $-1];

  return "1 " ~ pluralNoun;                              // unknown; no change
}
