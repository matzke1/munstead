module munstead.core.iset;

import core.exception;
import munstead.core.interval;
import munstead.core.itree: IntervalTree;
import std.algorithm;
import std.container.rbtree;
import std.algorithm.searching;
import std.range;

class IntervalSet(T)
  : IntervalTree!(Interval!T, "a") {

public:
  alias Value = T;

public:
    
  // Insert into this set those values contained in the specified interval.
  pure void insert(Interval interval) @safe {
    if (interval.empty)
      return;
    auto overlaps = overlapRange(interval.grow());

    Interval left, right; // what to put back
    foreach (i; overlaps.take(1))
      left = i.lessThan(interval.least());
    foreach (i; overlaps.tail(1)) // FIXME: This is O(n)
      right = i.greaterThan(interval.greatest());

    removeTreeNodes(overlaps);
    Interval[] toInsert = [ Interval.hull(left, interval, right) ];
    insertTreeNodes(toInsert);
  }

  // Insert into this set a single value.
  pure void insert(Interval.Value v) @safe {
    insert(Interval(v));
  }

  // Remove from this set those values contained in the specified interval.
  pure void remove(Interval interval) @safe {
    auto overlaps = overlapRange(interval);

    Interval left, right; // what to put back
    foreach (i; overlaps.take(1))
      left = i.lessThan(interval.least());
    foreach (i; overlaps.tail(1)) // FIXME: This is O(n)
      right = i.greaterThan(interval.greatest());

    removeTreeNodes(overlaps);
    insertTreeNodes([left, right].filter!"!a.empty");
  }

  // Remove from this set a single value
  pure void remove(Interval.Value v) @safe {
    remove(Interval(v));
  }

  // Return the complement of this set.
  pure IntervalSet complement() const @safe {
    IntervalSet retval = new IntervalSet;
    retval.insertTreeNodes(nodes().inverseIntervals);
    return retval;
  }

  alias exists = existsAll;
}

//-------- constructors --------
pure @safe unittest {
  auto set = new IntervalSet!int;
  assert(set.empty);
  assert(set.length == 0);
  assert(set.nIntervals == 0);
  assert(set.hull.empty);
}

//-------- insertions --------
pure @safe unittest {
  auto set = new IntervalSet!int;

  // insert empty interval is a no-op
  set.insert(set.Interval());
  assert(set.empty);

  // insert a singleton
  set.insert(-5);		// {-5}
  assert(!set.empty);
  assert(set.length == 1);
  assert(set.nIntervals == 1);
  assert(set.hull.length == 1);
  assert(set.hull.least == -5);

  // insert a range not adjacent
  set.insert(set.Interval.hull(1, 4)); // {-5, 1, 2, 3, 4}
  assert(set.length == 5);
  assert(set.nIntervals == 2);
  assert(set.hull.least == -5);
  assert(set.hull.greatest == 4);

  // insert a range that joins to the right and overlaps
  set.insert(set.Interval.hull(0, 3)); // {-5, 0, 1, 2, 3, 4}
  assert(set.length == 6);
  assert(set.nIntervals == 2);
  assert(set.hull == set.Interval.hull(-5, 4));

  // insert a range that joins to the left
  set.insert(-4);		//  {-5, -4, 0, 1, 2, 3, 4}
  assert(set.length == 7);
  assert(set.nIntervals == 2);
  assert(set.hull == set.Interval.hull(-5, 4));

  // insert a range that joins to the left and right
  set.insert(set.Interval.hull(-3, -1)); // {-5, -4, -3, -2, -1, 0, 1, 2, 3, 4}
  assert(set.length == 10);
  assert(set.nIntervals == 1);
  assert(set.hull == set.Interval.hull(-5, 4));
}

//-------- clearing --------
pure @safe unittest {
  auto set = new IntervalSet!int;

  set.insert(set.Interval.hull(-5, 4)); // {-5, -4, -3, -2, -1, 0, 1, 2, 3, 4}
  assert(set.length == 10);
  assert(set.nIntervals == 1);
  assert(set.hull == set.Interval.hull(-5, 4));

  set.clear();			// {}
  assert(set.empty);
  assert(set.length == 0);
  assert(set.nIntervals == 0);
}

//-------- removals --------
pure @safe unittest {
  auto set = new IntervalSet!int;

  set.insert(set.Interval.hull(-5, 4)); // {-5, -4, -3, -2, -1, 0, 1, 2, 3, 4}
  assert(set.length == 10);
  assert(set.nIntervals == 1);
  assert(set.hull == set.Interval.hull(-5, 4));

  // remove nothing
  set.remove(set.Interval());	// {-5, -4, -3, -2, -1, 0, 1, 2, 3, 4}
  assert(set.length == 10);
  assert(set.nIntervals == 1);
  assert(set.hull == set.Interval.hull(-5, 4));
  
  // remove something from the middle
  set.remove(set.Interval.hull(-3, -1)); // {-5, -4, 0, 1, 2, 3, 4}
  assert(set.length == 7);
  assert(set.nIntervals == 2);
  assert(set.hull == set.Interval.hull(-5, 4));

  // remove from the left
  set.remove(-5);		// {-4, 0, 1, 2, 3, 4}
  assert(set.length == 6);
  assert(set.nIntervals == 2);
  assert(set.hull == set.Interval.hull(-4, 4));

  // remove from the right
  set.remove(set.Interval.hull(4, 6)); // {-4, 0, 1, 2, 3}
  assert(set.length == 5);
  assert(set.nIntervals == 2);
  assert(set.hull == set.Interval.hull(-4, 3));

  // remove from the from middle again
  set.remove(2); 		// {-4, 0, 1, 3}
  assert(set.length == 4);
  assert(set.nIntervals == 3);
  assert(set.hull == set.Interval.hull(-4, 3));

  // remove everything
  set.remove(set.Interval.hull(-100, 100)); // {}
  assert(set.length == 0);
  assert(set.nIntervals == 0);
  assert(set.hull.empty);
}

//-------- set with min and/or max values --------
pure @safe unittest {
  auto set = new IntervalSet!byte;

  set.insert(set.Interval.hull(byte.min, 0)); // {[-128,0]}
  assert(set.length == 129);
  assert(set.nIntervals == 1);
  assert(set.hull == set.Interval.hull(-128, 0));

  set.insert(set.Interval.hull(125,127));  // {[-128,0], 125, 126, 127}
  assert(set.length == 132);
  assert(set.nIntervals == 2);
  assert(set.hull == set.Interval.whole());

  set.insert(set.Interval.hull(-10, 126)); // {[-128,127]}
  assert(set.length == 256);
  assert(set.nIntervals == 1);
  assert(set.hull == set.Interval.whole());

  set.remove(-127);		// {-128, [-126,127]}
  assert(set.length == 255);
  assert(set.nIntervals == 2);
  assert(set.hull == set.Interval.whole());

  set.remove(-128); 		// {[-126,127]}
  assert(set.length == 254);
  assert(set.nIntervals == 1);
  assert(set.hull == set.Interval.hull(-126, 127));

  set.remove(127); 		// {[-126,126]}
  assert(set.length == 253);
  assert(set.nIntervals == 1);
  assert(set.hull == set.Interval.hull(-126, 126));
}

//-------- integer set containing whole space -------
pure @safe unittest {
  auto set = new IntervalSet!int;

  set.insert(set.Interval.hull(-(2^^31-1), (2^^31-2)));
  assert(set.length == 2UL^^32-2);
  assert(set.nIntervals == 1);

  set.insert(-(2^^31));
  assert(set.length == 2UL^^32-1);
  assert(set.nIntervals == 1);

  set.insert(2^^31-1);
  assert(set.length == 2UL^^32);
  assert(set.nIntervals == 1);
}

//-------- length overflow --------
pure unittest {
  auto set = new IntervalSet!ulong;

  set.insert(set.Interval.whole());
  import std.exception;
  assertThrown!RangeError(set.length);
}

//-------- complement --------
pure @safe unittest {
  auto empty = new IntervalSet!ulong;

  // complement of the empty set is everything and vice versa
  auto all = empty.complement();
  assert(all.hull == all.Interval.whole());
  assert(all.complement().empty);

  // complement singleton
  auto singleton = new IntervalSet!ubyte;
  singleton.insert(100);
  auto comp1 = singleton.complement();
  assert(comp1.length == 255);
  assert(comp1.nIntervals == 2);
  assert(comp1.hull == comp1.Interval.whole());

  // complement multi
  auto multi = new IntervalSet!ubyte;
  multi.insert(multi.Interval.hull(50, 74));
  multi.insert(multi.Interval.hull(100, 124));
  auto comp2 = multi.complement();
  assert(comp2.length == 256 - 50);
  assert(comp2.nIntervals == 3);
  assert(comp2.hull == comp1.Interval.whole());
}

//-------- existence --------
pure @safe unittest {
  auto set = new IntervalSet!ulong;
  const empty = new IntervalSet!ulong;

  assert(empty.existsAll(empty.Interval()));
  assert(!empty.existsAny(empty.Interval()));
  assert(!empty.existsAll(empty.Interval.whole()));
  assert(!empty.existsAny(empty.Interval.whole()));

  set.insert(2UL^^64-1);
  assert(set.existsAll(set.Interval()));
  assert(set.existsAll(set.Interval(2UL^^64-1)));
  assert(!set.existsAll(set.Interval(0)));
  assert(!set.existsAll(set.Interval.whole()));

  assert(!set.existsAny(set.Interval()));
  assert(set.existsAny(set.Interval(2UL^^64-1)));
  assert(!set.existsAny(set.Interval(0)));
  assert(set.existsAny(set.Interval.whole()));

  auto multi = new IntervalSet!ubyte;
  multi.insert(multi.Interval.hull(100, 124));
  multi.insert(multi.Interval.hull(50, 74));
  assert(multi.existsAll(multi.Interval.hull(100, 124)));
  assert(multi.existsAny(multi.Interval.hull(100, 124)));
  assert(multi.existsAll(multi.Interval.hull(110, 120)));
  assert(multi.existsAny(multi.Interval.hull(110, 120)));
  assert(!multi.existsAll(multi.Interval.hull(50, 120)));
  assert(multi.existsAny(multi.Interval.hull(50, 120)));
  assert(!multi.existsAll(multi.Interval.hull(125, 130)));
  assert(!multi.existsAny(multi.Interval.hull(125, 130)));
}
