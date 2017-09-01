module munstead.core.interval;

import core.exception;
import std.algorithm: min, max, minElement, maxElement;
import std.array;
import std.range.primitives: ElementType, isForwardRange, isInputRange;
import std.typecons;

// Contiguous ranges of values of type T.
//
// Unlike most other implementations, this one can represent intervals that include T.max.
struct Interval(T) if (T.sizeof <= size_t.sizeof) {
public:
  alias Value = T;

private:
  Value lo_ = 1;		// low value, inclusive
  Value hi_ = 0;		// high value, inclusive. lo_==1 && hi_==0 implies empty

  invariant {
    assert(lo_ <= hi_ || (lo_==1 && hi_==0));
  }

public:
  // Constructs an empty interval.
  static Interval opCall() {
    Interval retval;
    return retval;
  }

  // Constructs a singleton interval.
  static Interval opCall(Value v) {
    Interval retval;
    retval.lo_ = retval.hi_ = v;
    return retval;
  }

  static Interval from(S)(Interval!S other) {
    Interval retval;
    if (!other.empty) {
      retval.lo_ = other.lo_;
      retval.hi_ = other.hi_;
    }
    return retval;
  }

  // Intentionally omitted:
  // static Interval opCall(Value a, Value b) -- is b the max value, max+1, or a size?!? Use hull or baseSize instead.

  // Construct an interval from two endpoints given in either order
  static Interval hull(Value v1, Value[] values...) {
    Interval retval;
    retval.lo_ = minElement([v1] ~ values);
    retval.hi_ = maxElement([v1] ~ values);
    return retval;
  }

  // Constructs the minimum interval that spans all specified intervals.
  static Interval hull(Interval i1, Interval[] intervals...) {
    Interval retval;
    foreach (interval; [i1] ~ intervals) {
      if (!interval.empty) {
	if (retval.empty) {
	  retval = interval;
	} else {
	  retval.lo_ = min(retval.lo_, interval.lo_);
	  retval.hi_ = max(retval.hi_, interval.hi_);
	}
      }
    }
    return retval;
  }

  // For completeness, hull() returns an empty interval
  static Interval hull() {
    return Interval();
  }

  // Construct an interval containing all possible values.
  static Interval whole() {
    Interval retval;
    retval.lo_ = Value.min;
    retval.hi_ = Value.max;
    return retval;
  }

  // Construct an interval from a lo value and a size.  Try to avoid using this factory because it has some diesign
  // issues that will eventually bite you:
  //
  //   (1) If size is zero, then lo is ignored.
  //   (2) size_t may be too small to represent all values, such as Interval!ulong.hull
  //   (3) lo + size - 1 might be larger than Value.max
  //   (4) overflow might not result in an exception--the Interval will be valid, just wrong
  //
  // Nonetheless, this method is provided because it can be useful in situations where the caller asserts that none of
  // these edge cases are possible.
  static Interval baseSize(Value lo, size_t size) {
    Interval retval;
    if (0 == size)
      return retval;
    Value hi = cast(Value)(lo + size - 1);
    if (hi < lo)
      throw new RangeError;
    retval.lo_ = lo;
    retval.hi_ = hi;
    return retval;
  }

  // True if this interval is empty.
  bool empty() const @property {
    return 1 == lo_ && 0 == hi_;
  }

  // Length of interval if possible, exception otherwise.
  size_t length() const @property {
    if (empty)
      return 0;
    size_t n = cast(size_t)(hi_) - cast(size_t)(lo_);
    if (n == size_t.max)
      throw new RangeError;
    return n + 1UL;
  }

  // Return interval minimum if not empty.
  Value least() const @property {
    assert(!empty());
    return lo_;
  }

  // Return interval maximum if not empty.
  Value greatest() const @property {
    assert(!empty());
    return hi_;
  }

  // True if this and the other have no elements in common. An empty interval is disjoint with any other interval since
  // they have no elements in common.
  bool disjoint(Interval other) const {
    return empty || other.empty || this.hi_ < other.lo_ || this.lo_ > other.hi_;
  }
  bool disjoint(Value other) const {
    return empty || hi_ < other || lo_ > other;
  }

  // True if this and the other have at least one element in common.  An empty interval overlaps with no other interval
  // since it has no elements in common with any other.
  bool overlaps(Interval other) const {
    return !disjoint(other);
  }
  bool overlaps(Value other) const {
    return !empty && lo_ <= other && hi_ >= other;
  }

  // True if this interval contains all of other (i.e., subset relationship). An empty interval is contained in all
  // other intervals since the empty interval is a subset of all intervals.
  bool contains(Interval other) const {
    return other.empty || (!empty && least <= other.least && greatest >= other.greatest);
  }
  bool contains(Value other) const {
    return !empty && least <= other && greatest >= other;
  }

  // The elements in common between this and the other interval.  The intersection of an empty interval and any other
  // interval is an empty interval.
  Interval intersect(Interval other) const {
    Interval retval;
    if (disjoint(other))
      return retval;
    retval.lo_ = max(this.lo_, other.lo_);
    retval.hi_ = min(this.hi_, other.hi_);
    return retval;
  }
  Interval intersect(Value other) const {
    return overlaps(other) ? Interval(other) : Interval();
  }

  // Returns an interval that starts at the same value but has at most N elements.
  Interval limit(size_t n) const {
    return empty ? this : hull(least, min(greatest, cast(Value)(least + n - 1)));
  }
    
  // True if this interval's least value is less than the other interval's least value. An empty interval is considered
  // to start before a non-empty interval so that this method defines a strict weak ordering.
  bool startsBefore(Interval other) const {
    return empty || other.empty ? empty && !other.empty : least < other.least;
  }
  bool startsBefore(Value other) const {
    return empty ? true : least < other;
  }

  // True if this interval's greatest value is greater than the other interval's greatest value. An empty interval is
  // considered to end before a non-empty interval so that this method defines a strict weak ordering.
  bool endsAfter(Interval other) const {
    return empty || other.empty ? !empty && other.empty : greatest > other.greatest;
  }
  bool endsAfter(Value other) const {
    return empty ? false : greatest > other;
  }

  // True if this interval ends one before the other interval begins. False if either interval is empty.
  bool leftAdjacent(Interval other) const {
    return empty || other.empty ? false : greatest < Value.max && cast(Value)(greatest + 1) == other.least;
  }
  bool leftAdjacent(Value other) const {
    return leftAdjacent(Interval(other));
  }

  // True if this interval begins one after the other interval ends. False if either interval is empty.
  bool rightAdjacent(Interval other) const {
    return other.leftAdjacent(this);
  }
  bool rightAdjacent(Value other) const {
    return Interval(other).leftAdjacent(this);
  }

  // Split this interval into two adjacent intervals: the left and right parts. If "at" is not a member of this
  // interval, then one of the returned intervals will equal this interval and the other will be empty: if "at" is left
  // of this interval then the left interval will be empty, otherwise the right interval will be empty.
  Tuple!(Interval, Interval) split(Value at) const {
    Interval left, right;
    if (empty) {
    } else if (at <= this.lo_) {
      right = this;
    } else if (at > this.hi_) {
      left = this;
    } else {
      left.lo_ = this.lo_;
      left.hi_ = cast(Value)(at - 1);
      right.lo_ = at;
      right.hi_ = this.hi_;
    }
    return tuple!(Interval, Interval)(left, right);
  }

  // The values of this interval that are less than or equal to v.
  pure Interval lessThanEqual(Value v) const @safe {
    return empty ? Interval() : intersect(Interval.hull(Value.min, v));
  }

  // The values of this interval that are less than v.
  pure Interval lessThan(Value v) const @safe {
    return empty || v == Value.min ? Interval() : intersect(Interval.hull(Value.min, cast(Value)(v-1)));
  }

  // The values of this interval that are greater than or equal to v.
  pure Interval greaterThanEqual(Value v) const @safe {
    return empty ? Interval() : intersect(Interval.hull(v, Value.max));
  }

  // The values of this interval that are greater than v.
  pure Interval greaterThan(Value v) const @safe {
    return empty || v == Value.max ? Interval() : intersect(Interval.hull(cast(Value)(v+1), Value.max));
  }

  // The value of this interval that is equal to v.
  pure Interval equalTo(Value v) const @safe {
    return empty || v < least || v > greatest ? Interval() : Interval(v);
  }

  // Make an interval bigger by extending it by one in each direction as possible.
  pure Interval grow() const @safe {
    assert(!empty);
    Value lo = least > Value.min ? cast(Value)(least - 1) : least;
    Value hi = greatest < Value.max ? cast(Value)(greatest + 1) : greatest;
    return hull(lo, hi);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
template isInterval(I) {
  static if (is(I T: Interval!T))
    enum isInterval = true;
  else
    enum isInterval = false;
}

// Given a list of non-overlapping intervals in increasing order, return the complement list of intervals.
pure auto inverseIntervals(Range)(Range range) @safe
if (isInputRange!Range && isInterval!(ElementType!Range)) {

  static struct InverseIntervals {
    alias Interval = munstead.core.interval.Interval!(ElementType!Range.Value); // no const or immutable
    private Interval current;
    private Range range;

    private this(Range range) {
      this.range = range;
      current = Interval.whole;
      fillCurrent();
    }

    pure bool empty() @safe {
      return current.empty;
    }

    pure Interval front() @safe {
      assert(!empty);
      return current;
    }

    pure void popFront() @safe {
      assert(!empty);
      current = Interval.whole.greaterThan(current.greatest);
      if (!current.empty)
	fillCurrent();
    }

    static if (isForwardRange!Range) {
      pure InverseIntervals save() @safe {
	auto copy = InverseIntervals(range.save);
	copy.current = current;
	return copy;
      }
    }

    // Adjust current to be the next hole.  Current's least value is already initialized, so we just need to advance it
    // to the right (unless it's already at a hole) and then trim the right end of it.
    private pure void fillCurrent() @safe {
      assert(!current.empty);

      // Advance current to the right until we reach a hole or the end
      while (!range.empty && range.front.least == current.least) {
	current = current.greaterThan(range.front.greatest);
	range.popFront();
      }

      // Trim the right side of current so it's *just* the hole.
      if (!range.empty)
	current = current.lessThan(range.front.least);
    }
  }

  return InverseIntervals(range);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//-------- isInterval --------
@safe pure unittest {
  alias I1 = Interval!ubyte;
  static assert(isInterval!I1);
  static assert(!isInterval!int);
}

//-------- opCall constructors --------

@safe pure unittest {
  // Construct empty
  Interval!int empty;
  assert(empty.empty);
  assert(empty.length == 0);

  // Construct singleton
  const s1 = Interval!int(100);
  assert(!s1.empty);
  assert(s1.length == 1);
  assert(s1.least == 100);
  assert(s1.greatest == 100);
}

//-------- hull constructors --------

@safe pure unittest {
  // Construct empty
  const h1 = Interval!int.hull();
  assert(h1.empty);

  // Construct singleton
  auto h2 = Interval!int.hull(5);
  assert(h2.least == 5);
  assert(h2.greatest == 5);

  // Construct given in forward order
  auto h3 = Interval!int.hull(-2, 1);
  assert(h3.least == -2);
  assert(h3.greatest == 1);
  assert(!h3.empty);
  assert(h3.length == 4);
  
  // Construct in reverse order
  auto h4 = Interval!int.hull(-5, -8);
  assert(h4.least == -8);
  assert(h4.greatest == -5);

  // Construct from multiple values
  auto h5 = Interval!int.hull(4, 3, 8, -2, 1);
  assert(h5.least == -2);
  assert(h5.greatest == 8);

  // Construct from intervals
  auto h6 = Interval!int.hull(h5, h4, h3, h2, h1);
  assert(h6.least == -8);
  assert(h6.greatest == 8);

  // Narrow types
  auto h7 = Interval!ubyte.hull(200, 255);
  assert(h7.least == 200);
  assert(h7.greatest == 255);
}

//-------- whole constructors --------

pure unittest {
  Interval!T testWhole(T)() {
    const x = Interval!T.whole();
    assert(x.least == T.min);
    assert(x.greatest == T.max);
    return x;
  }

  void safeTests() @safe {
    assert(testWhole!ubyte().length == 2^^8);
    assert(testWhole!byte().length == 2^^8);
    assert(testWhole!ushort().length == 2^^16);
    assert(testWhole!short().length == 2^^16);
    assert(testWhole!uint().length == 2UL^^32);
    assert(testWhole!int().length == 2UL^^32);
  }

  void unsafeTests() {
    import std.exception;
    assertThrown!RangeError(testWhole!ulong().length);
    assertThrown!RangeError(testWhole!long().length);
  }
}

//-------- base + size constructors --------

@safe pure unittest {
  auto bs1 = Interval!ubyte.baseSize(0, 0);
  assert(bs1.empty);

  auto bs2 = Interval!ubyte.baseSize(255, 0);
  assert(bs2.empty);

  auto bs3 = Interval!byte.baseSize(-128, 256);
  assert(bs3.least == -128);
  assert(bs3.greatest == 127);
}

pure unittest {
  import std.exception;
  assertThrown!RangeError(Interval!byte.baseSize(-127, 256));
}

//-------- equality --------

@safe pure unittest {
  auto empty  = Interval!int();
  auto single = Interval!int(5);
  auto multi  = Interval!int.hull(5, 10);

  assert(empty == empty);
  assert(single == single);
  assert(multi == multi);

  assert(empty != single);
  assert(empty != multi);
  assert(single != multi);

  assert(single != empty);
  assert(multi != empty);
  assert(multi != single);
}
  
//-------- disjoint and overlaps --------

@safe pure unittest {
  auto empty = Interval!int();
  auto i1 = Interval!int.hull(10, 19);
  auto i2 = Interval!int.hull(20, 29);
  auto i3 = Interval!int.hull(25, 34);
  auto i4 = Interval!int.hull(20, 39);

  // empties are always disjoint
  assert(empty.disjoint(empty));
  assert(empty.disjoint(i1));
  assert(i1.disjoint(empty));

  assert(!empty.overlaps(empty));
  assert(!empty.overlaps(i1));
  assert(!i1.overlaps(empty));

  // i1 and either i2 or i3
  assert(i1.disjoint(i2));
  assert(i2.disjoint(i1));
  assert(i1.disjoint(i3));
  assert(i3.disjoint(i1));

  assert(!i1.overlaps(i2));
  assert(!i2.overlaps(i1));
  assert(!i1.overlaps(i3));
  assert(!i3.overlaps(i1));

  // i2 and i3 overlap
  assert(!i2.disjoint(i3));
  assert(!i3.disjoint(i2));

  assert(i2.overlaps(i3));
  assert(i3.overlaps(i2));

  // i3 is a subset of i4
  assert(!i3.disjoint(i4));
  assert(!i4.disjoint(i3));

  assert(i3.overlaps(i4));
  assert(i4.overlaps(i3));
}

//-------- intersect --------
  
@safe pure unittest {
  auto empty = Interval!int();
  auto i1 = Interval!int.hull(10, 19);
  auto i2 = Interval!int.hull(20, 29);
  auto i3 = Interval!int.hull(25, 34);
  auto i4 = Interval!int.hull(20, 39);

  // empties intersected are always empty
  assert(empty.intersect(empty).empty);
  assert(empty.intersect(i1).empty);
  assert(i1.intersect(empty).empty);

  // i1 doesn't intersect with i2 or i3
  assert(i1.intersect(i2).empty);
  assert(i2.intersect(i1).empty);
  assert(i1.intersect(i3).empty);
  assert(i3.intersect(i1).empty);

  // i2 and i3 intersect
  assert(i2.intersect(i3) == Interval!int.hull(25, 29));
  assert(i3.intersect(i2) == Interval!int.hull(25, 29));

  // i3 and i4 intersect
  assert(i3.intersect(i4) == i3);
  assert(i4.intersect(i3) == i3);
}

//-------- startsBefore and endsAfter --------

@safe pure unittest {
  auto empty = Interval!int();
  auto i1 = Interval!int.hull(10, 12);
  auto i2 = Interval!int.hull(11, 13);

  // empties start and end before non-empties
  assert(empty.startsBefore(i1));
  assert(!i1.startsBefore(empty));
  assert(!empty.endsAfter(i1));
  assert(i1.endsAfter(empty));

  // non-reflexive
  assert(!empty.startsBefore(empty));
  assert(!i1.startsBefore(i1));
  assert(!empty.endsAfter(empty));
  assert(!i1.endsAfter(i1));

  assert(i1.startsBefore(i2));
  assert(!i2.startsBefore(i1));
  assert(i2.endsAfter(i1));
  assert(!i1.endsAfter(i2));
}

//-------- leftAdjacent and rightAdjacent --------

@safe pure unittest {
  auto empty = Interval!int();
  auto i1 = Interval!int.hull(10, 19);
  auto i2 = Interval!int.hull(20, 29);

  // empties are never adjacent to anything
  assert(!empty.leftAdjacent(empty));
  assert(!empty.rightAdjacent(empty));
  assert(!empty.leftAdjacent(i1));
  assert(!i1.leftAdjacent(empty));
  assert(!empty.rightAdjacent(i1));
  assert(!i1.rightAdjacent(empty));

  assert(i1.leftAdjacent(i2));
  assert(!i2.leftAdjacent(i1));

  assert(i2.rightAdjacent(i1));
  assert(!i1.rightAdjacent(i2));
}

//-------- split --------

@safe pure unittest {
  void testSplit(Interval!int input, int pivot, Interval!int left, Interval!int right) {
    Tuple!(Interval!int, Interval!int) got = input.split(pivot);
    assert(got[0] == left);
    assert(got[1] == right);
  }

  auto empty = Interval!int();
  auto i1 = Interval!int.hull(-5, 8);

  // splitting empty results in empties
  testSplit(empty, 5, empty, empty);

  testSplit(i1, -6, empty, i1);
  testSplit(i1, -5, empty, i1);
  testSplit(i1, -4, Interval!int(-5), Interval!int.hull(-4, 8));
  testSplit(i1, 7, Interval!int.hull(-5, 6), Interval!int.hull(7, 8));
  testSplit(i1, 8, Interval!int.hull(-5, 7), Interval!int(8));
  testSplit(i1, 9, i1, empty);
}

//-------- lessThan, etc. --------

@safe pure unittest {
  void testComp(string op)(Interval!int input, int val, Interval!int answer) {
    mixin("auto got = input." ~ op ~ "(val);");
    assert(got == answer);
  }

  auto empty = Interval!int();
  auto i1 = Interval!int.hull(1,10);

  // lessThanEqual
  testComp!"lessThanEqual"(i1, 0, empty);
  testComp!"lessThanEqual"(i1, 1, Interval!int(1));
  testComp!"lessThanEqual"(i1, 2, Interval!int.hull(1, 2));
  testComp!"lessThanEqual"(i1, 10, Interval!int.hull(1, 10));
  testComp!"lessThanEqual"(i1, 11, Interval!int.hull(1, 10));

  // lessThan
  testComp!"lessThan"(i1, 0, empty);
  testComp!"lessThan"(i1, 1, empty);
  testComp!"lessThan"(i1, 2, Interval!int(1));
  testComp!"lessThan"(i1, 3, Interval!int.hull(1, 2));
  testComp!"lessThan"(i1, 10, Interval!int.hull(1, 9));
  testComp!"lessThan"(i1, 11, Interval!int.hull(1, 10));
  testComp!"lessThan"(i1, 12, Interval!int.hull(1, 10));

  // greaterThanEqual
  testComp!"greaterThanEqual"(i1, 0, Interval!int.hull(1, 10));
  testComp!"greaterThanEqual"(i1, 1, Interval!int.hull(1, 10));
  testComp!"greaterThanEqual"(i1, 2, Interval!int.hull(2, 10));
  testComp!"greaterThanEqual"(i1, 9, Interval!int.hull(9, 10));
  testComp!"greaterThanEqual"(i1, 10, Interval!int(10));
  testComp!"greaterThanEqual"(i1, 11, empty);

  // greaterThan
  testComp!"greaterThan"(i1, 0, Interval!int.hull(1, 10));
  testComp!"greaterThan"(i1, 1, Interval!int.hull(2, 10));
  testComp!"greaterThan"(i1, 2, Interval!int.hull(3, 10));
  testComp!"greaterThan"(i1, 9, Interval!int(10));
  testComp!"greaterThan"(i1, 10, empty);
  testComp!"greaterThan"(i1, 11, empty);

  // equalTo
  testComp!"equalTo"(i1, 0, empty);
  testComp!"equalTo"(i1, 1, Interval!int(1));
  testComp!"equalTo"(i1, 2, Interval!int(2));
  testComp!"equalTo"(i1, 10, Interval!int(10));
  testComp!"equalTo"(i1, 11, empty);
}

//-------- inverse list --------
@safe pure unittest {
  import std.algorithm: equal;

  // Inverse of the empty list is all intervals.
  Interval!int[] list1;
  auto ans1 = [ Interval!int.whole ];
  auto inv1 = list1[].inverseIntervals;
  assert(equal(inv1, ans1));

  auto list2 = [ Interval!ubyte.hull(100, 200) ];
  auto ans2  = [ Interval!ubyte.hull(0, 99), Interval!ubyte.hull(201, 255) ];
  auto inv2  = list2.inverseIntervals;
  assert(equal(inv2, ans2));

  auto list3 = [ Interval!ubyte.hull(0, 10), Interval!ubyte.hull(100, 199), Interval!ubyte.hull(250, 255) ];
  auto ans3  = [ Interval!ubyte.hull(11, 99), Interval!ubyte.hull(200, 249) ];
  auto inv3  = list3.inverseIntervals;
  assert(equal(inv3, ans3));

  auto inv4 = inv3.save();
  
}
