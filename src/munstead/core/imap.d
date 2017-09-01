module munstead.core.imap;

import munstead.core.interval: Interval, inverseIntervals;
import munstead.core.itree: IntervalTree;
import std.algorithm: isStrictlyMonotonic, map, sort;
import std.range: array, empty, retro, take;
import std.typecons: Tuple, tuple;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class DefaultPolicy(Key, Value) {
  // Split the value (associated with the key) into two values, the first associated with the part of the key less than
  // pivot, and the second associated with the part of the key greater than or equal to the pivot.
  static Tuple!(Value, Value) split(Interval!Key key, Value value, Key pivot) {
    assert(key.contains(pivot));
    return tuple(value, value);
  }
    
  // If possible, merge val1 (associated with key1) and val2 (associated with key2) to create one larger value
  // associated with the union of key1 and key2.
  static Tuple!(bool, Value) merge(Interval!Key key1, Value val1, Interval!Key key2, Value val2) {
    assert(key1.leftAdjacent(key2));
    return val1 == val2 ? tuple(true, val1) : tuple(false, val1/*unimportant*/);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
struct IntervalMapRange(K, V, Policy) {
  alias Key = K;
  alias Value = V;
  alias Node = IntervalMap!(K, V, Policy).Node;

private:
  Node[] nodes_;

public:
  private this(Node[] nodes) {
    nodes_= nodes;
  }
  
  pure bool empty() const @property {
    return nodes_.length == 0;
  }

  pure Node font() @property {
    assert(nodes_.length > 0);
    return nodes_[0];
  }

  pure void popFront() {
    assert(nodes_.length > 0);
    auto retval = nodes_[0];
    nodes_ = nodes[1..$];
    return retval;
  }

  pure IntervalMapRange save() @property {
    return this;
  }

  pure Node back() @property {
    assert(nodes_.length > 0);
    return nodes_[$-1];
  }

  pure void popBack() {
    assert(nodes_.length > 0);
    assert(nodes_.length > 0);
    auto retval = nodes_[$-1];
    nodes_ = nodes[0..$-1];
    return retval;
  }
}
    
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Mapping from K to T.
class IntervalMap(K, V, Policy = DefaultPolicy!(K, V))
  : IntervalTree!(Tuple!(Interval!K, "interval",  V, "value"), "a.interval") {

  alias Key = K;
  alias Value = V;
  alias Interval = munstead.core.interval.Interval!Key;
  alias Node = Tuple!(Interval, "interval", Value, "value");

public:
  // Insert into this set those values contained in the specified interval.
  pure void insert(Interval interval, Value value) @safe {
    if (interval.empty)
      return;
    
    auto overlaps = overlapWithAdjacentRange(interval);
    Node[] toInsert = edges(interval, overlaps);
    toInsert ~= [ Node(interval, value) ];
    sort!"a.interval.startsBefore(b.interval)"(toInsert);
    toInsert = mergeNodes(toInsert);

    removeTreeNodes(overlaps);
    insertTreeNodes(toInsert);
  }

  // Insert into this set a single value.
  pure void insert(Interval.Value key, Value value) @safe {
    insert(Interval(key), value);
  }

  // Remove from this map those values contained in the specified interval.
  pure void remove(Interval interval) @safe {
    if (interval.empty)
      return;

    Value tmpval;
    Node needle = Node(interval, tmpval);

    auto overlaps = overlapRange(needle);
    Node[] toInsert = edges(interval, overlaps);
    assert(toInsert.length >= 0 && toInsert.length <= 2);

    removeTreeNodes(overlaps);
    insertTreeNodes(toInsert);
  }

  // Remove from this set a single value
  pure void remove(Interval.Value key) @safe {
    remove(Interval(key));
  }

  // Return the complement of this map, setting all elements to the specified value
  pure IntervalMap complement(Value value) const @safe {
    IntervalMap retval = new IntervalMap;
    retval.insertTreeNodes(nodes()
			   .map!(node => node.interval)
			   .inverseIntervals
			   .map!(interval => Node(interval, value)));
    return retval;
  }

  pure bool existsAll(Interval interval) const @safe {
    Value tmpval;
    return super.existsAll(Node(interval, tmpval));
  }

  alias exists = existsAll;

  // True if any of the specified values exist. False if the interval is empty.
  pure bool existsAny(Interval interval) const @safe {
    Value tmpval;
    return super.existsAny(Node(interval, tmpval));
  }

  // Returns the intersection of the map and the specified interval as a range.
  auto intersect(Interval interval) {
    debug import std.stdio;
    Value tmpval;
    Node needle = Node(interval, tmpval);
    return overlapRange(needle).map!(delegate Node(Node node) {
	if (node.interval.startsBefore(interval)) {
	  Interval rightInterval = node.interval.greaterThanEqual(interval.least);
	  Value rightValue = Policy.split(node.interval, node.value, interval.least)[1];
	  node = Node(rightInterval, rightValue);
	}
	if (node.interval.endsAfter(interval)) {
	  Interval leftInterval = node.interval.lessThanEqual(interval.greatest);
	  Value leftValue = Policy.split(node.interval, node.value, leftInterval.grow.greatest)[0];
	  node = Node(leftInterval, leftValue);
	}
	return node;
      });
  }
	  
private:
  // Range of nodes that overlap with some interval, prefixed by a left-adjacent node (if any) and suffixed by a
  // right-adjacent node (if any).
  pure overlapWithAdjacentRange(Interval interval) const @safe {
    Value tmpval;
    return overlapRange(Node(interval.grow(), tmpval));
  }
  pure overlapWithAdjacentRange(Interval interval) @safe { // FIXME: inout?
    Value tmpval;
    return overlapRange(Node(interval.grow(), tmpval));
  }

  // Gets left and right nodes trimmed to exclude some area
  static pure Node[] edges(Range)(Interval exclude, Range overlaps) @safe {
    Node[] retval;
    if (exclude.empty)
      return retval;
    foreach (node; overlaps) {
      if (node.interval.startsBefore(exclude)) {
	assert(retval.length == 0);
	if (node.interval.overlaps(exclude)) {
	  Interval leftInterval = node.interval.lessThan(exclude.least);
	  Value leftValue = Policy.split(node.interval, node.value, exclude.least)[0];
	  retval ~= [ Node(leftInterval, leftValue) ];
	} else {
	  retval ~= [ node ];
	}
      }
      if (node.interval.endsAfter(exclude)) {
	if (node.interval.overlaps(exclude)) {
	  Interval rightInterval = node.interval.greaterThan(exclude.greatest);
	  Value rightValue = Policy.split(node.interval, node.value, rightInterval.least)[1];
	  retval ~= [ Node(rightInterval, rightValue) ];
	} else {
	  retval ~= [ node ];
	}
      }
    }
    return retval;
  }

  // Return a new list of nodes by merging as many as possible
  static pure Node[] mergeNodes(Node[] nodes) @safe {
    assert(isStrictlyMonotonic!"a.interval.startsBefore(b.interval)"(nodes));
    Node[] retval;
    foreach (node; nodes) {
      if (retval.empty) {
	retval = [ node ];
      } else if (retval[$-1].interval.leftAdjacent(node.interval)) {
	auto merged = Policy.merge(retval[$-1].interval, retval[$-1].value, node.interval, node.value);
	if (merged[0]) {
	  retval[$-1] = Node(Interval.hull(retval[$-1].interval, node.interval), merged[1]);
	} else {
	  retval ~= [ node ];
	}
      } else {
	retval ~= [ node ];
      }
    }
    return retval;
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//-------- constructors --------
pure @safe unittest {
  const map = new IntervalMap!(int, string);
  assert(map.empty);
  assert(map.length == 0);
  assert(map.nIntervals == 0);
  assert(map.hull.empty);
}

//-------- insertions --------
pure @safe unittest {
  auto map = new IntervalMap!(int, string);
  const cmap = map;

  // insert empty interval is a no-op
  map.insert(map.Interval(), "x");
  assert(cmap.empty);

  // insert a singleton
  map.insert(-5, "a");		// {-5 "a"}
  assert(!cmap.empty);
  assert(cmap.length == 1);
  assert(cmap.nIntervals == 1);
  assert(cmap.hull.length == 1);
  assert(cmap.hull.least == -5);

  // insert a range not adjacent
  map.insert(map.Interval.hull(1, 4), "b"); // {-5 "a", [1,4] "b"}
  assert(cmap.length == 5);
  assert(cmap.nIntervals == 2);
  assert(cmap.hull.least == -5);
  assert(cmap.hull.greatest == 4);

  // insert a range that doesn't join due to a different value
  map.insert(map.Interval.hull(0, 3), "c"); // {-5 "a", [0,3] "c", 4 "b"}
  assert(cmap.length == 6);
  assert(cmap.nIntervals == 3);
  assert(cmap.hull == map.Interval.hull(-5, 4));

  // insert a range that joins to the left
  map.insert(-4, "a");		//  {[-5,-4] "a", [0,3] "c", 4 "b"}
  assert(cmap.length == 7);
  assert(cmap.nIntervals == 3);
  assert(cmap.hull == map.Interval.hull(-5, 4));

  // set [4] to "c", causing it to join to the left
  map.insert(4, "c");		// {[-5,-4] "a", [0,4] "c"}
  assert(cmap.length == 7);
  assert(cmap.nIntervals == 2);
  assert(cmap.hull == map.Interval.hull(-5, 4));

  // set [-6,0] to "b" causing everything to join
  map.insert(map.Interval.hull(-6, 0), "c");
  assert(cmap.length == 11);
  assert(cmap.nIntervals == 1);
  assert(cmap.hull == map.Interval.hull(-6, 4));
}

//-------- clearing --------
pure @safe unittest {
  auto map = new IntervalMap!(int,string);
  const cmap = map;

  map.insert(map.Interval.hull(-5, 4), "A"); // {[-5,4] "A"}
  assert(cmap.length == 10);
  assert(cmap.nIntervals == 1);
  assert(cmap.hull == map.Interval.hull(-5, 4));

  map.clear();			// {}
  assert(cmap.empty);
  assert(cmap.length == 0);
  assert(cmap.nIntervals == 0);
}

//-------- removals --------
pure @safe unittest {
  auto map = new IntervalMap!(int,string);
  const cmap = map;

  map.insert(map.Interval.hull(-5, 4), "A"); // {[-5,4] "A"}
  assert(cmap.length == 10);
  assert(cmap.nIntervals == 1);
  assert(cmap.hull == map.Interval.hull(-5, 4));

  // remove nothing
  map.remove(map.Interval());	// {[-5,4] "A"}
  assert(cmap.length == 10);
  assert(cmap.nIntervals == 1);
  assert(cmap.hull == map.Interval.hull(-5, 4));
  
  // remove something from the middle
  map.remove(map.Interval.hull(-3, -1)); // {[-5,-4] "A", [0,4] "A"}
  assert(cmap.length == 7);
  assert(cmap.nIntervals == 2);
  assert(cmap.hull == map.Interval.hull(-5, 4));

  // remove from the left
  map.remove(-5);		// {-4 "A", [0,4] "A"}
  assert(cmap.length == 6);
  assert(cmap.nIntervals == 2);
  assert(cmap.hull == map.Interval.hull(-4, 4));

  // remove from the right
  map.remove(map.Interval.hull(4, 6)); // {-4 "A", [0,3] "A"}
  assert(cmap.length == 5);
  assert(cmap.nIntervals == 2);
  assert(cmap.hull == map.Interval.hull(-4, 3));

  // remove from the from middle again
  map.remove(2); 		// {-4 "A", [0,1] "A", 3 "A"}
  assert(cmap.length == 4);
  assert(cmap.nIntervals == 3);
  assert(cmap.hull == map.Interval.hull(-4, 3));

  // remove everything
  map.remove(map.Interval.hull(-100, 100)); // {}
  assert(cmap.length == 0);
  assert(cmap.nIntervals == 0);
  assert(cmap.hull.empty);
}

//-------- map with min and/or max values --------
pure @safe unittest {
  auto map = new IntervalMap!(byte, string);
  const cmap = map;

  map.insert(map.Interval.hull(byte.min, 0), "A"); // {[-128,0]}
  assert(cmap.length == 129);
  assert(cmap.nIntervals == 1);
  assert(cmap.hull == map.Interval.hull(-128, 0));

  map.insert(map.Interval.hull(125,127), "A");  // {[-128,0], 125, 126, 127}
  assert(cmap.length == 132);
  assert(cmap.nIntervals == 2);
  assert(cmap.hull == map.Interval.whole());

  map.insert(map.Interval.hull(-10, 126), "A"); // {[-128,127]}
  assert(cmap.length == 256);
  assert(cmap.nIntervals == 1);
  assert(cmap.hull == map.Interval.whole());

  map.remove(-127);		// {-128, [-126,127]}
  assert(cmap.length == 255);
  assert(cmap.nIntervals == 2);
  assert(cmap.hull == map.Interval.whole());

  map.remove(-128); 		// {[-126,127]}
  assert(cmap.length == 254);
  assert(cmap.nIntervals == 1);
  assert(cmap.hull == map.Interval.hull(-126, 127));

  map.remove(127); 		// {[-126,126]}
  assert(cmap.length == 253);
  assert(cmap.nIntervals == 1);
  assert(cmap.hull == map.Interval.hull(-126, 126));
}

//-------- integer map containing whole space -------
pure @safe unittest {
  auto map = new IntervalMap!(int,string);
  const cmap = map;

  map.insert(map.Interval.hull(-(2^^31-1), (2^^31-2)), "A");
  assert(cmap.length == 2UL^^32-2);
  assert(cmap.nIntervals == 1);

  map.insert(-(2^^31), "A");
  assert(cmap.length == 2UL^^32-1);
  assert(cmap.nIntervals == 1);

  map.insert(2^^31-1, "A");
  assert(cmap.length == 2UL^^32);
  assert(cmap.nIntervals == 1);
}

//-------- length overflow --------
pure unittest {
  auto map = new IntervalMap!(ulong,string);
  const cmap = map;

  map.insert(map.Interval.whole(), "A");
  import std.exception;
  import core.exception;
  assertThrown!RangeError(cmap.length);
}

//-------- complement --------
pure @safe unittest {
  const empty = new IntervalMap!(ulong, string);

  // complement of the empty map is everything and vice versa
  const all = empty.complement("X");
  assert(all.hull == all.Interval.whole());
  assert(all.complement("Y").empty);

  // complement singleton
  auto singleton = new IntervalMap!(ubyte, string);
  singleton.insert(100, "A");
  const comp1 = singleton.complement("X");
  assert(comp1.length == 255);
  assert(comp1.nIntervals == 2);
  assert(comp1.hull == comp1.Interval.whole());

  // complement multi
  auto multi = new IntervalMap!(ubyte, string);
  multi.insert(multi.Interval.hull(50, 74), "A");
  multi.insert(multi.Interval.hull(100, 124), "A");
  const comp2 = multi.complement("X");
  assert(comp2.length == 256 - 50);
  assert(comp2.nIntervals == 3);
  assert(comp2.hull == comp1.Interval.whole());
}

//-------- existence --------
pure @safe unittest {
  auto map = new IntervalMap!(ulong, string);
  const cmap = map;
  const empty = new IntervalMap!(ulong, string);

  assert(empty.existsAll(empty.Interval.init));
  assert(!empty.existsAny(empty.Interval.init));
  assert(!empty.existsAll(empty.Interval.whole));
  assert(!empty.existsAny(empty.Interval.whole));

  map.insert(2UL^^64-1, "A");
  assert(cmap.existsAll(map.Interval()));
  assert(cmap.existsAll(map.Interval(2UL^^64-1)));
  assert(!cmap.existsAll(map.Interval(0)));
  assert(!cmap.existsAll(map.Interval.whole()));

  assert(!cmap.existsAny(map.Interval()));
  assert(cmap.existsAny(map.Interval(2UL^^64-1)));
  assert(!cmap.existsAny(map.Interval(0)));
  assert(cmap.existsAny(map.Interval.whole()));

  auto multi = new IntervalMap!(ubyte, string);
  multi.insert(multi.Interval.hull(100, 124), "A");
  multi.insert(multi.Interval.hull(50, 74), "A");
  assert(multi.existsAll(multi.Interval.hull(100, 124)));
  assert(multi.existsAny(multi.Interval.hull(100, 124)));
  assert(multi.existsAll(multi.Interval.hull(110, 120)));
  assert(multi.existsAny(multi.Interval.hull(110, 120)));
  assert(!multi.existsAll(multi.Interval.hull(50, 120)));
  assert(multi.existsAny(multi.Interval.hull(50, 120)));
  assert(!multi.existsAll(multi.Interval.hull(125, 130)));
  assert(!multi.existsAny(multi.Interval.hull(125, 130)));
}
