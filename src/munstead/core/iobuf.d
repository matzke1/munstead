module munstead.core.iobuf;

import munstead.core.interval;
import std.algorithm: min;
import std.mmfile;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class IoBuffer(A, T) {
  alias Address = A;
  alias Value = T;

private:
  Interval!A space_;

public:
  // The size of a buffer cannot be changed after it is constructed. If it were allowed, then changing the size of a
  // buffer while it's being used by a memory map could have bad consequences.
  this(Interval!A space) {
    space_ = space;
  }
  
  // Read up to N items starting at a particular address.
  abstract Value[] read(Address start, size_t n);

  // Writes as many items as possible.
  abstract size_t write(Address start, Value[] values);

  // How many values are available starting at the specified address.
  pure size_t available(Address start) const @safe {
    return space_.overlaps(start) ? space_.greaterThanEqual(start).length : 0;
  }

private:
  size_t offset(Address start) {
    return space_.overlaps(start) ? space_.lessThan(start).length : 0;
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class NullBuffer(A, T) : IoBuffer!(A, T) {
  this(Interval!A space) {
    super(space);
  }

  // N values starting at zero.
  this(size_t n) {
    super(Interval!A.baseSize(cast(A)0, n));
  }

  override Value[] read(Address start, size_t n) {
    Value[] retval;
    retval.length = min(n, available(start));
    return retval;
  }
    
  override size_t write(Address start, Value[] values) {
    return min(values.length, available(start));
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class ArrayBuffer(A, T) : IoBuffer!(A, T) {
private:
  T[] data_;

public:
  this(Interval!A space) {
    super(space);
    data_.length = space.length;
  }

  this(size_t n) {
    auto space = Interval!A.baseSize(cast(A)0, n);
    super(space);
    data_.length = n;
  }

  this(T[] data) {
    auto space = Interval!A.baseSize(cast(A)0, data.length);
    super(space);
    data_ = data;
  }

  override Value[] read(Address start, size_t n) {
    n = min(n, available(start));
    return n > 0 ? data_[offset(start) .. offset(start)+n] : data_[0..0];
  }

  override size_t write(Address start, Value[] data) {
    size_t n = min(data.length, available(start));
    data_[offset(start) .. offset(start)+n] = data[0 .. n];
    return n;
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class MmapBuffer(A, T) : IoBuffer!(A, T) {
private:
  MmFile file_;

public:
  this(string fileName) {
    this(new MmFile(fileName));
  }

  this(MmFile file) {
    assert(file !is null);
    file_ = file;
    size_t nBytes = file_.length;
    size_t nElmts = nBytes / Value.sizeof;
    super(Interval!A.baseSize(0, nElmts));
  }

  override Value[] read(Address start, size_t n) {
    n = min(n, available(start));
    size_t byteOffset = offset(start) * Value.sizeof;
    size_t nBytes = n * Value.sizeof;
    return cast(Value[])(file_[byteOffset .. byteOffset + nBytes]);
  }

  override size_t write(Address start, Value[] data) {
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
//unittest {
//  import std.stdio;
//
//  auto buffer = new MmapBuffer!(size_t, char)("/etc/passwd");
//
//  auto x = buffer.read(0, 100);
//  writeln("got ", x.length, " = ", x);
//}
