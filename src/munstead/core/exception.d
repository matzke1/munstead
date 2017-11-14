module munstead.core.exception;

import munstead.ast.base;
import munstead.core.interval;
import munstead.core.mmap;
import munstead.core.util;
import std.algorithm: each, filter, map;
import std.conv: to;
import std.exception;
import std.format;
import std.typecons: isIntegral;

/** Base class for tracking parse locations.
 *
 *  Every error message or exception points to a list of parse locations in order to print detailed, nested error messages.
 *  Most of the time, this chain is created by calling methods that construct these objects rather than the user instantiating
 *  them directly.
 *
 *  Locations should not change once they're created, because they might be referenced by error messages saved in objects. */
class ParseLocation {
  protected ParseLocation parent_;
  protected string what_;   // name of major thing being parsed

  /** Pointer to parent location. */
  ParseLocation parent() @property { return parent_; } // read-only

  /** Entity being parsed. */
  string what() @property { return what_; }

  /** Convert the object to a string suitable for use in error messages.
   *
   *  The returned string should not have indentation line termination. It should assume that it will be printed
   *  as part of a sentence after the word "in". */
  abstract string message();

  // used by std.conv.to!string
  abstract void toString(scope void delegate(const(char)[]) sink, FormatSpec!char fmt) const;
}

/** An error location which is a name and an index.
 *
 *  These are used when the location refers to an element of an array, entry in a table, etc. */
class IndexParseLocation: ParseLocation {
  protected size_t index_;

  /** Consructor.
   *
   *  Param:
   *    what   = String describing entity wherein the error occurs, such as "ELF string table".
   *    index  = The index to appear in the error location message.
   *    parent = The optional enclosing error location. */
  this(string what, size_t index, ParseLocation parent) {
    parent_ = parent;
    what_ = what;
    index_ = index;
  }

  /** Index describing error location. */
  size_t index() const @property { return index_; }

  override void toString(scope void delegate(const(char)[]) sink, FormatSpec!char fmt) const {
    sink((cast(IndexParseLocation)this).message()); // FIXME
  }

  override string message() {
    return what_ ~ " #" ~ index_.to!string;
  }
}

/** An error location associated with a map.
 *
 *  These are used for describing locations of parse errors within conten stored in a map. The map typically represents
 *  (part of) a file or virtual memory. */
class MapParseLocation(Map): ParseLocation {
  protected AddressSpace space_;             // whether various fields pertain to file or virtual memory address space
  protected Map bytes_;                      // file or memory bytes being parsed, or null
  protected Map.Address local_;              // address of the thing in the bytes_ content
  protected string comment_;
  protected Interval!(Map.Address) global_;  // where bytes_[0] maps in the global (file or memory) space, or empty

  /** Constructor.
   *
   *  Param:
   *    what    = String describing entity wherein the error occurs, such as "ELF string table".
   *    space   = Whether the locations are file offsets or memory addresses.
   *    bytes   = The local content wherein the error occurs.
   *    global  = Optional interval describing where the local content would appear in a global address space.
   *    local   = Offset or address within the local content.
   *    comment = Additional commentary, such as the name of the segment in which the error occurs.
   *    parent  = The optional enclosing error location. */
  this(string what, AddressSpace space, Map bytes, Interval!(Map.Address) global, Map.Address local, string comment,
       ParseLocation parent) {
    what_ = what;
    space_ = space;
    bytes_ = bytes;
    global_ = global;
    local_ = local;
    comment_ = comment;
    parent_ = parent;
  }

  override void toString(scope void delegate(const(char)[]) sink, FormatSpec!char fmt) const {
    sink((cast(MapParseLocation)this).message()); // FIXME
  }

  override string message() {
    string ret = what_;

    final switch (space_) {
    case AddressSpace.FILE:
      if (what_ == "")
        ret ~= "file";
      if (!global_.empty) {
        auto gaddr = cast(Map.Address)(global_.least + local_);
        if (bytes_ !is null && bytes_.name != "") {
          ret ~= " at offset " ~ gaddr.hexStr ~ " in " ~ bytes_.name;
        } else {
          ret ~= " at file offset " ~ (cast(Map.Address) global_.least + local_).hexStr;
        }
      } else {
        if (bytes_ !is null && bytes_.name != "") {
          ret ~= " at local offset " ~ local_.hexStr ~ " in " ~ bytes_.name;
        } else {
          ret ~= " at file local offset " ~ local_.hexStr;
        }
      }
      break;

    case AddressSpace.MEMORY:
      if (what_ == "")
        ret ~= "memory";
      if (!global_.empty) {
        auto va = cast(Map.Address)(global_.least + local_);
        ret ~= " at va " ~ va.hexStr;
      } else {
        ret ~= " offset " ~ local_.hexStr;
      }
      if (comment_ != "") {
        ret ~= " (" ~ comment_ ~ ")";
      } else if (bytes_ !is null) {
        auto where = Interval!(Map.Address)(local_);
        bytes_.segmentsWithin(where)
          .map!"a.segment"
          .filter!(segment => segment.name != "")
          .each!(segment => ret ~= " (within " ~ segment.name ~ ")");
      }
      break;
    }
    return ret;
  }
}

/** Construct an index parse location. */
IndexParseLocation indexParseLocation(string what, size_t index, ParseLocation parent) {
  return new IndexParseLocation(what, index, parent);
}

/** Base class for Munstead exceptions. */
class MunsteadException: object.Exception {
  this(string mesg, string sourceFile, size_t lineNumber) {
    super(mesg, sourceFile, lineNumber);
  }
}

/** Base class representing all types of parsing errors. */
class ParseError: MunsteadException {
  ParseLocation loc;
  size_t sequenceNumber;

  this(ParseLocation loc, string mesg, string sourceFile = __FILE__, size_t lineNumber = __LINE__) {
    super(mesg, sourceFile, lineNumber);
    this.loc = loc;
  }

  void toString(scope void delegate(const(char)[]) sink, FormatSpec!char fmt) const {
    if (sequenceNumber > 0) {
      sink("error #" ~ sequenceNumber.to!string ~ " parsing specimen: " ~ msg ~ "\n");
    } else {
      sink("error parsing specimen: " ~ msg ~ "\n");
    }
    for (ParseLocation l = cast(ParseLocation)loc; l !is null; l = l.parent) { // FIXME: cast
      sink("    in ");
      l.toString(sink, fmt);
      sink("\n");
    }
    //super.toString(sink);
  }
}
