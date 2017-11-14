/** Read/write operations on buffers. */
module munstead.core.iobuf;

import munstead.core.interval;
import std.algorithm: min;
import std.array: array;
import std.mmfile;
import std.range: retro;

size_t nextBufferId() {
  static size_t nextId;
  synchronized return nextId++;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/** Base class for buffers with I/O operations. */
class IoBuffer(A, T) {
  alias Address = A;            /** Data type for address of items in the buffer. */
  alias Value = T;              /** Data type for the items in the buffer. */

private:
  size_t id_;
  Interval!A space_;

public:
  /** Construct a buffer with a specified size.
   *
   * The size of a buffer cannot be changed after it is constructed. If this were allowed, then changing the size of a
   * buffer while it's being used by a memory map could have bad consequences. */
  this(Interval!A space) {
    id_ = nextBufferId();
    space_ = space;
  }
  
  /** Read up to N items starting at a particular address.
   *
   *  The returned array is a copy of the data in the buffer, not a slice. */
  abstract Value[] read(Address start, size_t n) const;

  /** Writes as many items as possible. */
  abstract size_t write(Address start, const(Value)[] values);

  /** Number of available values starting at the specified address. */
  pure size_t available(Address start) const @safe {
    return space_.overlaps(start) ? space_.greaterThanEqual(start).length : 0;
  }

  /** Identifying name (read-only).
   *
   *  Buffers are given unique identifying names so they can appear in diagnostic messages. */
  string name() const {
    string letters;
    if (id_ == 0)
      return "a";
    for (size_t n = id_; n > 0 || letters.length < 3; n /= 26)
      letters = [ cast(immutable(char))('a' + n % 26) ] ~ letters;

    return letters;
  }
    
private:
  size_t offset(Address start) const {
    return space_.overlaps(start) ? space_.lessThan(start).length : 0;
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/** I/O operations with no backing store.
 *
 *  Writes are ignored; reads always return zeros. */
class NullBuffer(A, T) : IoBuffer!(A, T) {
  /** Construct a null buffer with the specified addresses. */
  this(Interval!A space) {
    super(space);
  }

  /** Construct a null buffer of specified size.
   *
   *  The addresses of this buffer start at zero. */
  this(size_t n) {
    super(Interval!A.baseSize(cast(A)0, n));
  }

  override Value[] read(Address start, size_t n) const {
    Value[] retval;
    retval.length = min(n, available(start));
    return retval;
  }
    
  override size_t write(Address start, const(Value)[] values) {
    return min(values.length, available(start));
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/** I/O on buffers that are arrays.
 *
 *  Writing to this buffer modifies elements of the array.  The array is a fixed length for the life of the buffer. */
class ArrayBuffer(A, T) : IoBuffer!(A, T) {
private:
  T[] data_;

 public:
  /** Construct a buffer with the specified addresses. */
  this(Interval!A space) {
    super(space);
    data_.length = space.length;
  }

  /** Construct a buffer of the specified size. */
  this(size_t n) {
    auto space = Interval!A.baseSize(cast(A)0, n);
    super(space);
    data_.length = n;
  }

  /** Construct a buffer pointing to data.
   *
   *  The size of the array should not change while the buffer is pointing to it.  The best practice is to
   *  give ownership of the array to the buffer. */
  this(T[] data) {
    auto space = Interval!A.baseSize(cast(A)0, data.length);
    super(space);
    data_ = data;
  }

  override Value[] read(Address start, size_t n) const {
    n = min(n, available(start));
    auto nonConstData = cast(T[]) data_;
    return n > 0 ? nonConstData[offset(start) .. offset(start)+n].dup : nonConstData[0..0].dup;
  }

  override size_t write(Address start, const(Value)[] data) {
    size_t n = min(data.length, available(start));
    data_[offset(start) .. offset(start)+n] = data[0 .. n];
    return n;
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/** I/O buffer on memory-mapped files.
 *
 *  I/O occurs in the file only when it occurs in this buffer; the file is not read into memory explicitly. */
class MmapBuffer(A, T) : IoBuffer!(A, T) {
private:
  MmFile file_;

 public:
  /** Construct a buffer from a file.
   *
   *  The file is opened and mapped to memory. */
  this(string fileName) {
    this(new MmFile(fileName));
  }

  /** Construct a buffer from an existing memory map.
   *
   *  This gives you more control over mapping permissions than with the constructor that takes only a file name. */
  this(MmFile file) {
    assert(file !is null);
    file_ = file;
    size_t nBytes = file_.length;
    size_t nElmts = nBytes / Value.sizeof;
    super(Interval!A.baseSize(0, nElmts));
  }

  override Value[] read(Address start, size_t n) const {
    n = min(n, available(start));
    size_t byteOffset = offset(start) * Value.sizeof;
    size_t nBytes = n * Value.sizeof;

    auto nonConstFile = cast(MmFile) file_;
    auto ret = nonConstFile[byteOffset .. byteOffset + nBytes].dup;
    return cast(Value[]) ret;
  }

  override size_t write(Address start, const(Value)[] data) {
    size_t nElmts = min(data.length, available(start));
    size_t byteOffset = offset(start) * Value.sizeof;
    size_t nBytes = nElmts * Value.sizeof;
    static if (0) { // this doesn't work
      file_[byteOffset .. byteOffset + nBytes] = cast(void[])(data[0..nElmts]);
    } else {
      ubyte[] tmp = cast(ubyte[])(data[0 .. nElmts]);
      assert(tmp.length == nBytes);
      for (size_t i = 0; i < nBytes; ++i)
        file_[byteOffset + i] = tmp[i];
    }
    return nElmts;
  }

  pure MmFile mmfile() @property {
    return file_;
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
unittest {} // to prevent the following tests from being documented

unittest {
  import std.algorithm;
  import std.array;
  import std.conv;
  import std.file;
  import std.range;
  import std.stdio;
  writeln("unit tests: ", __FILE__);

  //----- Null buffers -----

  auto buf1nc = new NullBuffer!(size_t, ubyte)(100);
  const buf1 = buf1nc;
  size_t buf1s1 = buf1.available(25);
  assert(buf1s1 == 75, buf1s1.to!string);

  const(ubyte)[] buf1d1 = buf1.read(0, 5);
  assert(buf1d1.equal([0,0,0,0,0]), buf1d1.to!string);

  const(ubyte[]) buf1d2 = buf1.read(95, 10);
  assert(buf1d2.length == 5, buf1d2.length.to!string);
  assert(buf1d2.equal([0,0,0,0,0]), buf1d1.to!string);

  size_t buf1s2 = buf1nc.write(0, buf1d1);
  assert(buf1s2 == 5, buf1s2.to!string);

  //----- Array buffers -----

  auto buf2data = new int[10];
  auto buf2nc = new ArrayBuffer!(size_t, int)(buf2data);
  const buf2 = buf2nc;
  size_t buf2s1 = buf2.available(2);
  assert(buf2s1 == 8, buf2s1.to!string);

  const(int[3]) buf2d1 = [1, 2, 3];
  size_t buf2s2 = buf2nc.write(5, buf2d1);
  assert(buf2data.equal([0,0,0,0,0,1,2,3,0,0]), buf2data.to!string);

  auto buf2d2 = buf2.read(4, 10);
  assert(buf2d2.length == 6, buf2d2.length.to!string);
  assert(buf2d2.equal([0,1,2,3,0,0]), buf2d2.to!string);

  //----- Mmap buffers -----

  string fileName = "/etc/passwd"; // something that should exist but not be writable
  assert(exists(fileName));
  auto buf3d1 = (File("/etc/passwd").byLineCopy.take(1).array)[0];
  auto buf3 = new MmapBuffer!(size_t, ubyte)("/etc/passwd");
  auto buf3d2 = buf3.read(0, buf3d1.length);
  assert(buf3d1 == buf3d2, "buf3d1=" ~ buf3d1.to!string ~ " buf3d2=" ~ buf3d2.to!string);
}
