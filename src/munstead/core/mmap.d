/** Data in a virtual address space. */
module munstead.core.mmap;

import munstead.ast.base;
import munstead.core.bitflags;
import munstead.core.exception;
import munstead.core.imap;
import munstead.core.interval;
import munstead.core.iobuf;
import munstead.core.util;
import std.algorithm: filter, joiner, map, min, OpenRight, until;
import std.array: array;
import std.exception: enforce;
import std.range: choose, take, tee;
import std.range.primitives: isInputRange, ElementType;
import std.regex: Regex;
import std.string: leftJustify;
import std.traits: Unsigned;
import std.typecons: No, Nullable, Tuple, tuple;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/** Bits describing access permissions.
 *
 *  These flags describe access permissions. The permissions are only notational--the API still allows read and write
 *  access regardless of these flags.  The flags are intended to note things such as how a Linux or Windows loader
 *  sets permissions on memory segments, not to control access through the API. */
enum Access: uint {
  READABLE              = 0x00000001, /** Data can be read from such locations. */
  WRITABLE              = 0x00000002, /** New data can be written to such locations. */
  EXECUTABLE            = 0x00000004, /** Data can be executed at such locations. */
  IMMUTABLE             = 0x00000008, /** Write operations may fail. E.g., when backed by a read-only memory mapped file. */

  // Useful shortcuts
  READ_WRITE            = 0x00000003, /** Mask for readable and writable. */
  READ_WRITE_EXECUTE    = 0x00000007, /** Mask for read, write, and execute all at once. */
  READ_EXECUTE          = 0x00000005, /** Mask for read and execute. */

  // Other bits
  RESERVED_BIT_MASK     = 0x00000fff, /** These bits are given special meaning by the library. */
  USER_BIT_MASK         = 0xfffff000, /** These bits the user can define however they want. */
  USER_BIT_0            = 0x00001000  /** Least significant bit available for user to use how they want. */
}

/** Access bits as a set of flags. */
alias AccessFlags = BitFlags!Access;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/** Describes data for a contiguous region of memory. */
class Segment(A, T = ubyte) {
  alias Address = A;                  /** Type of virtual addresses, usually an unsigned type. */
  alias Value = T;                    /** Type of values stored in the map, usually unsigned bytes. */
  alias Size = Unsigned!Address;      /** Type used to represent sizes of things. */

public:
  AccessFlags access;                 /** Accessibility bit flags. */
  string name;                        /** Name for debugging and diagnostics. Escape this when printing. */

private:
  IoBuffer!(Address, Value) buffer_;
  Size offset_;

  invariant {
    assert(buffer_ !is null);
    assert(buffer_.available(offset_) > 0);
  }

public:
  /** Construct a new segment.
   *
   *  Params:
   *    buffer    = The buffer providing the data.
   *    offset    = Offset into the buffer for the first datum.
   *    access    = Notational accessibility bits. Doesn't actually affect API operations.
   *    name      = Name used for debugging. May contain special characters that should be escaped when printing. */
  this(IoBuffer!(Address, Value) buffer, Size offset, AccessFlags access, string name) {
    buffer_ = buffer;
    offset_ = offset;
    this.access = access;
    this.name = name;
  }

  /** Copy constructor.
   *
   *  The new segment points to the same buffer as the original. I.e., this is a shallow copy. */
  this(Segment other) {
    buffer_ = other.buffer_;
    offset_ = other.offset_;
    access = other.access;
    name = other.name;
  }

  /** Buffer holding the bytes for this segment. */
  pure IoBuffer!(Address, Value) buffer() @property @safe {
    return buffer_;
  }

  /** Byte offset into buffer for first byte of this segment. */
  pure Size offset() const @property @safe {
    return offset_;
  }

  /** Total length of this segment.
   *
   *  The total length of the segment is based on the length of the underlying buffer and the offset within that buffer to
   *  the beginning of this segment's data. Therefore, the return value might be larger than the address interval to which
   *  this segment is mapped in a MemoryMap. */
  pure size_t length() const @property @safe {
    return buffer_.available(offset_);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/** Operations on memory map segments. 
 *
 * See munstead.core.imap.DefaultPolicy. */
class MemoryMapPolicy(A, T) {
  alias Address = A;
  alias Value = T;
  alias Size = Unsigned!Address;
  alias Segment = .Segment!(Address, Value);

  /** Split segment into two with pivot being the low address of the second segment.
   *
   *  Both segments will still point to the same buffer, just different parts of it. */
  static Tuple!(Segment, Segment) split(Interval!Address interval, Segment segment, Address pivot) {
    assert(interval.contains(pivot));
    auto right = new Segment(segment.buffer,
                             cast(Size)(segment.offset + interval.lessThan(pivot).length),
                             segment.access,
                             segment.name);
    return tuple(segment, right);
  }

  /** Join two adjacent segments if possible.
   *
   *  Adjacent segments can be merged if they point to adjacent areas of the same underlying buffer, and they
   *  have the same name and access bits. */
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

/** Maps values into virtual address space.
 *
 *  This differs from an associative array T[A] in a number of ways:
 *
 *  $(LI The data is represented by IoBuffer objects so that data at consecutive addresses is next to each other
 *       in memory. E.g., as an array.)
 *
 *  $(LI The data is accessed with a read/write paradigm as if it were a file rather than an array.)
 *
 *  $(LI If the data is from a file, it need not be actually read into the process--it could be memory mapped.)
 *
 *  $(LI The data is organized into consecutive addresses that are given various attributes like names and
 *       accessibility.) */
class MemoryMap(A, T = ubyte): IntervalMap!(A, Segment!(A, T), MemoryMapPolicy!(A, T)) {
  alias Address = A;                                    /** Type of addresses. */
  alias Value = T;                                      /** Type of data stored at addresses. */
  alias Segment = .Segment!(Address, Value);            /** Characteristics of contiguous regions of data. */
  alias Node = Tuple!(Interval, "interval", Segment, "segment"); /** Interval, segment pair. */

  string name;                                          /** Name of mapping as a whole. */

  /** Insert new data into the mapping.
   *
   *  The new data occludes any data that might have previously been mapped at the specified location. The old data
   *  is not changed in any way.  It is permissible to map the same data at multiple locations in the address space.
   *  The interval must not be empty. The segment must be long enough to provide data at all addresses indicated in the
   *  interval. */
  override void insert(Interval interval, Segment segment) {
    enforce(!interval.empty);
    enforce(interval.length <= segment.length);
    if (name == "" && this.length == 0)
      name = segment.name;
    super.insert(interval, segment);
  }

  /** Insert a file into this mapping.
   *
   *  Maps the specified file into the address space starting at the specified address.  The file is not actually
   *  read by this operation--reads are delayed until the file is accessed. */
  void insertFile(string fileName, Address start) {
    auto buffer = new MmapBuffer!(Address,Value)(fileName);
    auto segment = new Segment(buffer, 0, AccessFlags(Access.READABLE | Access.IMMUTABLE), fileName);
    auto interval = Interval.baseSize(start, buffer.available(0));
    insert(interval, segment);
  }

  /** Change permissions for parts of the memory map.
   *
   *  All mapped addresses that fall within the specified interval (and no addresses outside that interval) have
   *  their access bits changed to the value specified. It is permissible for the interval to refer to parts of the
   *  address space that are not mapped. */
  void mprotect(Interval interval, AccessFlags access) {
    auto newNodes = segmentsWithin(interval)
      .filter!(node => node.segment.access != access)
      .tee!(node => node.segment.access = access)
      .array; // make a copy that we can use to modify the container
    foreach (node; newNodes)
      insert(node.interval, node.segment);
  }

  /** Read an object from the address space.
   *
   *  Data are read from the address space and used to initialize an object of type T, which is then returned. The
   *  object is initialized with a cast from an array of the value type to (a singleton array of) the object type. */
  bool readObjectAt(T)(Address start, ref T obj) {
    foreach (node; segmentsAt(start).byBuffer(T.sizeof).take(1)) {
      if (node.buffer.length == T.sizeof) {
        obj = (cast(T[]) node.buffer)[0];
        return true;
      }
    }
    return false;
  }

  /** Reads a string starting at the specified address.
   *
   *  Starts reading at the specified address and continues until reaching a NUL terminator or reaches the specified
   *  length limit.  The return value is a tuple whose first member is the string read (without the NUL terminator)
   *  and whose second member is a string error message or empty.  Strings are assumed to be NUL-terminated 8-bit
   *  ASCII. */
  Tuple!(string, "str", string, "error") stringAt(Address addr, size_t maxBytes = 1024*1024*1024) {
    enum chunkSize = 128 /*arbitrary*/;
    size_t maxChunks = (maxBytes + chunkSize - 1) / chunkSize;
    string err;

    auto chars = segmentsAt(addr).contiguous
      .byBuffer(chunkSize)
      .map!"a.buffer"
      .take(maxChunks)
      .joiner
      .until!"a == 0"(No.openRight)
      .array;

    if (0 == chars.length) {
      err = "string starting address " ~ addr.hexStr ~ " is not mapped";
    } else if (chars[$-1] != 0) {
      err = "string starting at " ~ addr.hexStr ~ " is not NUL-terminated";
    }

    string s = chars.length ? (cast(char[]) chars[0..$-1]).idup : "";
    return Tuple!(string, "str", string, "error")(s, err);
  }

  /** Reads a string or appends an error to an AST node.
   *
   *  This method is somewhat easier to use than the two-argument version because it alleviates the caller from
   *  having to handle errors. */
  string stringAt(Address addr, Ast node, ParseLocation loc, size_t maxBytes = 1024*1024*1024) {
    assert(node !is null);
    auto found = stringAt(addr, maxBytes);
    if (found.error != "")
      node.appendError(loc, found.error);
    return found.str;
  }

  /** Range of map entries by segment.
   *
   *  Returns a range that iterates over the segments in the mapping. The element type of the returned range is a Node
   *  tuple which has "interval" and "segment" members. */
  auto bySegment() {
    return segmentsWithin(Interval.whole);
  }

  /** Returns a trimmed range of nodes.
   *
   *  This is similar to bySegment, except the range is trimmed so no interval or segment falls outside the specified
   *  interval.  In other words, the specified interval is a superset of the union of all the returned nodes' intervals. */
  auto segmentsWithin(Interval interval) {
    return intersect(interval)
      .map!(imapNode => Node(imapNode.interval, imapNode.value));
  }

  /** Returns nodes from an address onward.
   *
   *  This is similar to segmentsWithin, except the range continues to the end of the mapping. */
  auto segmentsAt(Address va) {
    auto range1 = segmentsWithin(Interval.whole.greaterThanEqual(va));
    auto range2 = segmentsWithin(Interval()); // empty range of segments
    return choose(!range1.empty && range1.front.interval.least == va, range1, range2);
  }

  /** Print meta information about the map for debugging. */
  void dumpMeta(string prefix = "") {
    import std.stdio;

    if (length > 0) {
      size_t addrWidth = Address.min.hexStr.length;
      size_t sizeWidth = size_t.min.hexStr.length;
      writeln(prefix, leftJustify("First", addrWidth),
              "   ", leftJustify("Size", sizeWidth),
              "   ", leftJustify("Last+1", addrWidth),
              " Perms",
              " ", leftJustify("Buf and offset", 4+addrWidth),
              " Name or comment");
      writeln(prefix, leftJustify("", addrWidth, '-'),
              "   ", leftJustify("", sizeWidth, '-'),
              "   ", leftJustify("", addrWidth, '-'),
              " -----",
              " ", leftJustify("", 4+addrWidth, '-'),
              " ---------------");
    }
              
    foreach (node; bySegment) {
      AccessFlags flags = node.segment.access;
      string perms;
      perms ~= flags.isSet(Access.READABLE) ? "r" : "-";
      perms ~= flags.isSet(Access.WRITABLE) ? "w" : "-";
      perms ~= flags.isSet(Access.EXECUTABLE) ? "x" : "-";
      perms ~= flags.isSet(Access.IMMUTABLE) ? "i" : "-";
      flags = flags.excluding(Access.READ_WRITE_EXECUTE, Access.IMMUTABLE);
      perms ~= flags.empty ? "-" : "?";

      writeln(prefix, node.interval.least.hexStr, " + ", node.interval.length.hexStr, " = ", (node.interval.greatest+1).hexStr,
              " ", perms, " ", node.segment.buffer.name, " ", node.segment.offset.hexStr, " ", node.segment.name);
    }
  }
}

/** Convert a range of nodes to a range of buffers.
 *
 *  Given a range of Node containing intervals and segments, copy the data into a range whose elements are pairs
 *  of intervals and buffers. The buffers are arrays of the value type each containing up to maxElemts elements.
 *  The elements of any buffer are located at consecutive addresses in the map's address space, although they may 
 *  come from different segments. 
 *
 *  The lower bound of each buffer interval will be a multiple of `alignment` if possible.  For instance, if the input
 *  range contains segments at [16,25] and [27,30], the buffer size is 5, and the alignment is one (no alignment) the
 *  output buffers will be five bytes at [16,20], five bytes at [21,25], and four bytes at [27,30].  However, if the
 *  alignment is changed to 10, the output will be four bytes [16,19] (non-aligned), five bytes [20,24] (aligned), one
 *  byte at [25] (not aligned), three bytes at [27,29] (not aligned), and one byte at [30] (aligned).
 */
auto byBuffer(Range)(Range range, size_t maxElmts, size_t alignment = 1)
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
    private size_t maxElmts;    // max number of elements to return at a time
    private size_t alignment;   // goal for aligning output buffers
    private Range range;        // the incoming range
    Interval!Address taken;     // what we've already taken from range.front
    IntervalBuffer current;     // what's returned by font()

    this(Range range, size_t maxElmts, size_t alignment) {
      assert(maxElmts > 0);
      this.range = range;
      this.maxElmts = maxElmts;
      this.alignment = alignment;
      fillBuffer();
    }

    bool empty() @property {
      return current.interval.empty && range.empty;
    }

    private void fillBuffer() {
      current.interval = Interval!Address();
      current.buffer.length = 0;

      auto tmpMaxElmts = maxElmts;
      if (!range.empty) {
        auto begin = range.front.interval.least;
        if (!taken.empty())
          begin += taken.length;
        auto end = alignDown(begin + maxElmts, alignment);
        if (end > begin)
          tmpMaxElmts = end - begin;
      }

      while (current.interval.length < tmpMaxElmts && !range.empty &&
             (current.interval.empty || current.interval.leftAdjacent(range.front.interval))) {

        // The next chunk of data to grab, without regard for alignment
        Interval!Address chunkInterval = taken.empty ?
          range.front.interval.limit(tmpMaxElmts-current.interval.length) :
          range.front.interval.greaterThan(taken.greatest).limit(tmpMaxElmts);
        assert(current.buffer.length + chunkInterval.length <= tmpMaxElmts);
        
        // Offset into the segment's buffer
        Size bufOffset = range.front.segment.offset + (chunkInterval.least - range.front.interval.least);
        assert(range.front.segment.buffer.available(bufOffset) >= chunkInterval.length);

        // Read data from the segment's buffer and append it to our front buffer
        Value[] values = range.front.segment.buffer.read(bufOffset, chunkInterval.length);
        assert(values.length == chunkInterval.length);
        current.buffer ~= values;
        current.interval = Interval!Address.hull(current.interval, chunkInterval);

        // Remember how much we've taken from the input segment at front. If everything, pop front.
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

  return ByBuffer!Range(range, maxElmts, alignment);
}

/** Return range of segments until encountering one that's not contiguous with the previons one. */
auto contiguous(Range)(Range range)
if (isInputRange!Range && is(typeof(ElementType!Range.interval))) {
  alias Interval = typeof(ElementType!Range.interval);
  Interval previous;

  bool foundGap(Node)(Node node) {
    if (previous.empty) {
      previous = node.interval;
      return false;
    } else if (!previous.leftAdjacent(node.interval)) {
      return true;
    } else {
      previous = node.interval;
      return false;
    }
  }
    
  return range.until!(node => foundGap(node));
}
    
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
unittest {} // to prevent the following tests from being documented

unittest {
  import std.conv;
  import std.range;
  import std.stdio;
  writeln("unit tests: ", __FILE__);

  ubyte[5] a1 = [1,2,3,4,5];
  ubyte[3] a2 = [6,7,8];

  auto b1 = new ArrayBuffer!(size_t,ubyte)(a1);
  auto b2 = new ArrayBuffer!(size_t,ubyte)(a2);

  alias Map = MemoryMap!size_t;
  auto map = new Map;

  map.insert(Map.Interval.baseSize(10,5), new Map.Segment(b1, 0, AccessFlags(Access.READABLE), "segment 1"));
  map.insert(Map.Interval.baseSize(15,3), new Map.Segment(b2, 0, AccessFlags(Access.WRITABLE), "segment 2"));
  // leave a hole of two bytes, then map part of b1 again
  map.insert(Map.Interval.baseSize(20, 3), new Map.Segment(b1, 1, AccessFlags(Access.EXECUTABLE), "segment 3"));

  // Iterate over all data in the map
  foreach (i, node; map.bySegment.enumerate) {
    switch (i) {
    case 0:
      assert(node.interval.least == 10, node.interval.to!string);
      assert(node.interval.greatest == 14, node.interval.to!string);
      assert(node.segment.name == "segment 1");
      break;
    case 1:
      assert(node.interval.least == 15, node.interval.to!string);
      assert(node.interval.greatest == 17, node.interval.to!string);
      assert(node.segment.name == "segment 2");
      break;
    case 2:
      assert(node.interval.least == 20, node.interval.to!string);
      assert(node.interval.greatest == 22, node.interval.to!string);
      assert(node.segment.name == "segment 3");
      break;
    default:
      assert(0, "too many iterations");
      break;
    }
  }

  // Iterate over just a little data in the map
  foreach (i, node; map.segmentsWithin(Map.Interval.hull(14,20)).enumerate) {
    switch (i) {
    case 0:
      assert(node.interval.least == 14, node.interval.to!string);
      assert(node.interval.greatest == 14, node.interval.to!string);
      assert(node.segment.name == "segment 1");
      break;
    case 1:
      assert(node.interval.least == 15, node.interval.to!string);
      assert(node.interval.greatest == 17, node.interval.to!string);
      assert(node.segment.name == "segment 2");
      break;
    case 2:
      assert(node.interval.least == 20, node.interval.to!string);
      assert(node.interval.greatest == 20, node.interval.to!string);
      assert(node.segment.name == "segment 3");
      break;
    default:
      assert(0, "too many iterations");
      break;
    }
  }

  // Read data from the map
  foreach (i, node; map.segmentsWithin(Map.Interval.hull(14,20)).byBuffer(3).enumerate) {
    switch (i) {
    case 0:
      assert(node.interval.least == 14, node.interval.to!string);
      assert(node.interval.greatest == 16, node.interval.to!string);
      assert(node.buffer.length == 3);
      break;
    case 1:
      assert(node.interval.least == 17, node.interval.to!string);
      assert(node.interval.greatest == 17, node.interval.to!string);
      assert(node.buffer.length == 1);
      break;
    case 2:
      assert(node.interval.least == 20, node.interval.to!string);
      assert(node.interval.greatest == 20, node.interval.to!string);
      assert(node.buffer.length == 1);
      break;
    default:
      assert(0, "too many iterations");
      break;
    }
  }

  // Iterate over contiguous intervals
  foreach (i, node; map.bySegment.contiguous.enumerate) {
    switch(i) {
    case 0:
      assert(node.interval.least == 10, node.interval.to!string);
      assert(node.interval.greatest == 14, node.interval.to!string);
      assert(node.segment.name == "segment 1");
      break;
    case 1:
      assert(node.interval.least == 15, node.interval.to!string);
      assert(node.interval.greatest == 17, node.interval.to!string);
      assert(node.segment.name == "segment 2");
      break;
    default:
      assert(0, "too many iterations");
      break;
    }
  }
}

// Reading a string
unittest {
  import std.conv;
  import std.range;

  ubyte[5] a1 = [ 0x41, 0x42, 0x43, 0x44, 0x45 ]; // "ABCDE"
  ubyte[5] a2 = [ 0x46, 0x47, 0x48, 0x00, 0x01 ]; // "FGH\0\1"

  auto b1 = new ArrayBuffer!(size_t, ubyte)(a1);
  auto b2 = new ArrayBuffer!(size_t, ubyte)(a2);

  alias Map = MemoryMap!size_t;
  auto map = new Map;
  map.insert(Map.Interval.baseSize(10,5), new Map.Segment(b1, 0, AccessFlags(Access.READABLE), "segment 1"));
  map.insert(Map.Interval.baseSize(15,5), new Map.Segment(b2, 0, AccessFlags(Access.READABLE), "segment 2"));

  foreach (i, node; map.bySegment.contiguous.byBuffer(30).enumerate) {
    switch (i) {
    case 0:
      assert(node.interval.least == 10, node.interval.least.to!string);
      assert(node.interval.length == 10, node.interval.length.to!string);
      break;
    default:
      assert(0, "too many iterations");
      break;
    }
  }
}

// Aligned buffers
unittest {
  import std.conv;
  import std.range;

  ubyte[32] a1 = [0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
                  0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f ];
  auto b1 = new ArrayBuffer!(size_t, ubyte)(a1);

  alias Map = MemoryMap!size_t;
  auto map = new Map;
  map.insert(Map.Interval.baseSize(0, a1.length), new Map.Segment(b1, 0, AccessFlags(Access.READABLE), "segment 1"));

  auto where = Map.Interval.hull(8, a1.length-1);
  foreach (i, node; map.segmentsWithin(where).byBuffer(16, 16).enumerate) {
    switch (i) {
    case 0:
      assert(node.interval.least == 8, node.interval.least.to!string);
      assert(node.interval.greatest == 15, node.interval.greatest.to!string);
      break;
    case 1:
      assert(node.interval.least == 16, node.interval.least.to!string);
      assert(node.interval.greatest == 31, node.interval.greatest.to!string);
      break;
    default:
      assert(0, "too many iterations");
      break;
    }
  }
}
