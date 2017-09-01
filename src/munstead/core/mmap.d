module munstead.core.mmap;

import munstead.core.imap;
import munstead.core.interval;
import munstead.core.iobuf;
import std.algorithm: map, min, until;
import std.exception: enforce;
import std.range: choose, take;
import std.range.primitives: isInputRange, ElementType;
import std.regex: Regex;
import std.traits: Unsigned;
import std.typecons: Nullable, Tuple, tuple;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
enum Access: uint {
  READABLE              = 0x00000001,
  WRITABLE              = 0x00000002,
  EXECUTABLE            = 0x00000004,
  IMMUTABLE             = 0x00000008, // write operations may fail (e.g., underlying storage is read-only)

  // Useful shortcuts
  READ_WRITE            = 0x00000003,
  READ_WRITE_EXECUTE    = 0x00000007,
  READ_EXECUTE          = 0x00000005,

  // Other bits
  RESERVED_BIT_MASK     = 0x00000fff, // bits that are given special meaningn
  USER_BIT_MASK         = 0xfffff000, // bits that the user can define however they want
  USER_BIT_0            = 0x00001000  // first bit available for user
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Describes data for a contiguous region of memory.
class Segment(A, T) {
  alias Address = A;
  alias Value = T;
  alias Size = Unsigned!Address;

public:
  uint access; // "Access" bits
  string name;

private:
  IoBuffer!(Address, Value) buffer_;
  Size offset_;

  invariant {
    assert(buffer_ !is null);
    assert(buffer_.available(offset_) > 0);
  }

public:
  this(IoBuffer!(Address, Value) buffer, Size offset, uint accessBits, string name) {
    buffer_ = buffer;
    offset_ = offset;
    access = accessBits;
    this.name = name;
  }

  // Buffer holding the bytes for this segment
  pure IoBuffer!(Address, Value) buffer() @property @safe {
    return buffer_;
  }

  // Byte offset into buffer for first byte of this segment
  pure Size offset() const @property @safe {
    return offset_;
  }

  // Total length of this segment, which might be larger than the address interval to which it's mapped in a MemoryMap.
  pure size_t length() const @property @safe {
    return buffer_.available(offset_);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Operations on memory map segments. See munstead.core.imap.DefaultPolicy.
class MemoryMapPolicy(A, T) {
  alias Address = A;
  alias Value = T;
  alias Size = Unsigned!Address;
  alias Segment = .Segment!(Address, Value);

  // Split segment into two with pivot being the low address of the second segment.
  static Tuple!(Segment, Segment) split(Interval!Address interval, Segment segment, Address pivot) {
    assert(interval.contains(pivot));
    auto right = new Segment(segment.buffer,
			     cast(Size)(segment.offset + interval.lessThan(pivot).length),
			     segment.access,
			     segment.name);
    return tuple(segment, right);
  }

  // Join two adjacent segments if possible.
  static Tuple!(bool, Segment) merge(Interval!Address i1, Segment a, Interval!Address i2, Segment b) {
    assert(i1.leftAdjacent(i2));
    if (a.access == b.access && a.name == b.name && a.buffer is b.buffer && a.offset + i1.length == b.offset) {
      assert(a.length >= Interval!Address.hull(i1,i2).length);
      return tuple(true, a);
    } else {
      return tuple(false, a/*unimportant*/);
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Maps values into virtual address space.
class MemoryMap(A, T = ubyte): IntervalMap!(A, Segment!(A, T), MemoryMapPolicy!(A, T)) {
  alias Address = A;
  alias Value = T;
  alias Segment = .Segment!(Address, Value);
  //alias Selector = MemoryMapSelector!(Address, Value);

public:
  alias Node = Tuple!(Interval, "interval", Segment, "segment");

  // Useful for debugging
  string name;
  
  override void insert(Interval interval, Segment segment) {
    enforce(!interval.empty);
    enforce(interval.length <= segment.length);
    if (name == "" && this.length == 0)
      name = segment.name;
    super.insert(interval, segment);
  }

  // Insert the entire contents of a file into the memory starting at the specified map address.
  void insertFile(string fileName, Address start) {
    auto buffer = new MmapBuffer!(Address,Value)(fileName);
    auto segment = new Segment(buffer, 0, Access.READABLE | Access.IMMUTABLE, fileName);
    auto interval = Interval.baseSize(start, buffer.available(0));
    insert(interval, segment);
  }

  bool readObjectAt(T)(Address start, ref T obj) {
    foreach (node; segmentsAt(start).byBuffer(T.sizeof).take(1)) {
      if (node.buffer.length == T.sizeof) {
	obj = (cast(T[]) node.buffer)[0];
	return true;
      }
    }
    return false;
  }
    
  // List of all segments
  auto bySegment() {
    return segmentsWithin(Interval.whole);
  }

  // Return a range of memory map nodes trimmed to intersect with the specified interval.  The specified interval is a
  // superset of the union of all the returned nodes.
  auto segmentsWithin(Interval interval) {
    return intersect(interval)
      .map!(imapNode => Node(imapNode.interval, imapNode.value));
  }

  // Returns a range of memory map nodes that begin at the specified address and continue to the end of the map.  If the
  // map does not contain the specified address then an empty range is returned.
  auto segmentsAt(Address va) {
    auto range1 = segmentsWithin(Interval.whole.greaterThanEqual(va));
    auto range2 = segmentsWithin(Interval()); // empty range of segments
    return choose(!range1.empty && range1.front.interval.least == va, range1, range2);
  }
}

// Return a range whose elements are tuples containing an "interval" and a "buffer" array of values. The arrays are up
// to maxElmts elements each and may span across segments provided the segments are contiguous.
auto byBuffer(Range)(Range range, size_t maxElmts)
  if (isInputRange!Range) // FIXME: should be more restrictive
{
  alias Address = ElementType!Range.segment.Address;
  alias Value = ElementType!Range.segment.Value;
  alias Size = ElementType!Range.segment.Size;

  struct IntervalBuffer {
    Interval!Address interval;
    Value[] buffer;
  }

  struct ByBuffer(Range) {
    private size_t maxElmts; 	// max number of elements to return at a time
    private Range range;	// the incoming range
    Interval!Address taken;	// what we've already taken from range.front
    IntervalBuffer current;	// what's returned by font()

    this(Range range, size_t maxElmts) {
      assert(maxElmts > 0);
      this.range = range;
      this.maxElmts = maxElmts;
      fillBuffer();
    }

    bool empty() @property {
      return current.interval.empty && range.empty;
    }

    private void fillBuffer() {
      current.interval = Interval!Address();
      current.buffer.length = 0;
      while (current.interval.length < maxElmts && !range.empty &&
	     (current.interval.empty || current.interval.leftAdjacent(range.front.interval))) {
	Interval!Address chunkInterval = taken.empty ?
	  range.front.interval.limit(maxElmts) :
	  range.front.interval.greaterThan(taken.greatest).limit(maxElmts);
	assert(current.buffer.length + chunkInterval.length <= maxElmts);
	Size bufOffset = range.front.segment.offset + (chunkInterval.least - range.front.interval.least);
	assert(range.front.segment.buffer.available(bufOffset) >= chunkInterval.length);
	Value[] values = range.front.segment.buffer.read(bufOffset, chunkInterval.length);
	assert(values.length == chunkInterval.length);
	current.buffer ~= values;
	current.interval = Interval!Address.hull(current.interval, chunkInterval);

	taken = Interval!Address.hull(taken, chunkInterval);
	if (taken.greatest == range.front.interval.greatest) {
	  range.popFront();
	  taken = Interval!Address();
	}
      }
    }

    IntervalBuffer front() @property{
      assert(!empty);
      return current;
    }

    void popFront() {
      assert(!empty);
      fillBuffer();
    }
  }

  return ByBuffer!Range(range, maxElmts);
}

// Return range of segments until encountering one that's not contiguous with the previons one.
auto contiguous(Range)(Range range)
if (isInputRange!Range && is(typeof(ElementType!Range.interval))) {
  alias Interval = typeof(ElementType!Range.interval);
  Interval previous;

  bool foundGap(Node)(Node node) {
    if (previous.empty) {
      previous = node.interval;
      return false;
    } else if (!previous.leftAdjacent(node.interval)) {
      return false;
    } else {
      previous = node.interval;
      return true;
    }
  }
    
  return range.until!(node => foundGap(node));
}
    
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//unittest {
//  debug import std.stdio;
//  
////  auto buf = new MmapBuffer!(ulong, ubyte)("/etc/passwd");
////  auto seg = new Segment!(ulong, ubyte)(buf, 0, 0, "");
////  debug writeln(seg.length);
//
//  auto map = new MemoryMap!(ulong, ubyte);
//  map.insertFile("/etc/passwd", 0);
//  map.insertFile("/etc/passwd", 4000);
//
//  import std.array;
//  import std.range;
//  import std.algorithm;
//
//  // Process one buffer at a time of readable data in part of the map
//  writeln("buffer-at-a-time:");
//  map
//    .segmentsWithin(Interval!ulong.hull(1000,5000))
//    .filter!(a => (a.segment.access & Access.READABLE) != 0)
//    .byBuffer(100)
//    .each!(a => writeln("  buffer at ", a.interval, "; length = ", a.buffer.length));
//
//  // Print all the free space in the whole map
//  writeln("free space:");
//  map
//    .bySegment
//    .map!(node => node.interval)
//    .inverseIntervals
//    .each!(interval => writeln("  ", interval));
//
//  // Count used space the hard way
//  size_t nUsed = map
//    .bySegment
//    .map!(node => node.interval.length)
//    .fold!((a, b) => a + b);
//  writeln("nUsed = ", nUsed);
//  assert(nUsed == map.length);
//
//  // Count free space the hard way
//  size_t nFree = map
//    .bySegment
//    .map!(node => node.interval)
//    .inverseIntervals
//    .map!(interval => interval.length)
//    .fold!((a, b) => a + b);
//  writeln("nFree = ", nFree);
//  assert(nFree == 0 - nUsed); // this trick only works for Interval!ulong or similar and relies on unsigned overflow.
//  
//}
