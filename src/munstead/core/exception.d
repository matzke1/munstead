module munstead.core.exception;

import std.exception;

// Base class for Munstead errors. Errors are either accumulated in some object (such as errors when parsing import
// table entries as part of parsing a PE import table), or immediately thrown.
//
// The location of the error consists generally of a name and specifically an offset.  For example, if a syntax error is
// encountered when parsing a text configuration file, the general location is the file name and the specific location
// is a line number. On the other hand, if an error occurs when parsing an import table entry in a virtual memory image
// loaded from some PE file, the location might be the name of the file and/or memory segment and the specific location
// might the the virtual address where the error occured.  In yet another case, the general location might be a the name
// of a Munstead source file and the specific location is the line number (this kind of error is less useful to users
// who care most about their input files,  not the Munstead source code which they might not even have).
class MunsteadException: object.Exception {
  this(string mesg, string general, size_t specific) {
    super(mesg, general, specific);
  }
}

class SyntaxError: MunsteadException {
  this(string mesg, string general, size_t specific) {
    super(mesg, general, specific);
  }
}
