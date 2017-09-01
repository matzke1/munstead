// Functions for pretty-printing parts of AST nodes.
module munstead.ast.tabulate;

import munstead.ast.base;
import std.algorithm;
import std.conv;
import std.range;
import std.stdio;
import std.traits;

struct TabulateFormat {
  string prefix; // what to print to the left of each thing
  size_t nameWidth = 80; // width of the prefix + name column
  bool skipChildren; // if true, show one-line info per child; if false, show nothing

  TabulateFormat indent(string name) {
    TabulateFormat ret;
    if (this.prefix.length == 0 || name.length == 0) {
      ret.prefix = this.prefix ~ name; // one or both are empty
    } else {
      ret.prefix = this.prefix ~ "." ~ name;
    }
    ret.nameWidth = this.nameWidth;
    ret.skipChildren = this.skipChildren;
    return ret;
  }

  TabulateFormat indent(size_t idx) {
    TabulateFormat ret;
    ret.prefix = this.prefix ~ "[" ~ to!string(idx) ~ "]";
    ret.nameWidth = this.nameWidth;
    ret.skipChildren = this.skipChildren;
    return ret;
  }
};

// Basic types
void tabulate(T)(T value, TabulateFormat fmt)
if (isBasicType!T) {
  writefln("%-*s %s", fmt.nameWidth, fmt.prefix, value);
}

// Arrays
void tabulate(T)(T value, TabulateFormat fmt)
if (isArray!T) {
  static if (is(typeof(value) == string)) {
    // FIXME: string should be escaped
    writefln("%-*s \"%s\"", fmt.nameWidth, fmt.prefix, value);
  } else {
    if (value.length <= 10 && isBasicType!(typeof(value[0]))) {
      // Small arrays of basic types can be printed in-line
      writef("%-*s [", fmt.nameWidth, fmt.prefix);
      iota(value.length).each!(i => write(i>0 ? ", " : "", value[i]));
      writeln("]");

    } else {
      // Large arrays (or small arrays of non-basic type) are printed vertically
      for (size_t i = 0; i < value.length; ++i) {
	auto subfmt = fmt.indent(i);
	tabulate(value[i], subfmt);
      }
    }
  }
}

// Aggregates such as structs and classes
void tabulate(T)(T value, TabulateFormat fmt)
if (isAggregateType!T) {

  void oneLine(Node)(Node node, TabulateFormat subfmt) {
    if (node is null) {
      writefln("%-*s %s(null)", subfmt.nameWidth, subfmt.prefix, Node.stringof);
    } else {
      import munstead.ast.sections: AsmSection;
      static if (is(Node nBits : AsmSection!nBits)) {
	writefln("%-*s %s(\"%s\")", subfmt.nameWidth, subfmt.prefix, typeid(node), node.name);
      } else {
	writefln("%-*s %s", subfmt.nameWidth, subfmt.prefix, Node.stringof);
      }
    }
  }

  static if (is(typeof(value is null))) {
    if (value is null) {
      writefln("%-*s <null>", fmt.nameWidth, fmt.prefix);
      return;
    }
  }
  
  foreach (member; __traits(allMembers, typeof(value))) {
    static if (!is(typeof(__traits(getMember, value, member)))) {
      // not a member with a type (e.g., it might be an alias for a type)
    } else static if (member.length > 10 && member[0..10] == "_AST_IMPL_") {
      // don't print implementation details
    } else static if (__traits(getProtection, __traits(getMember, value, member)) != "public") {
      // only print public things
    } else static if (hasUDA!(__traits(getMember, value, member), _AstChildAccessor)) {
      // show one-line info for child pointers?
      if (!fmt.skipChildren) {
	auto child = __traits(getMember, value, member);
	auto subfmt = fmt.indent(member);
	oneLine(child, subfmt);
      }
    } else static if (hasUDA!(__traits(getMember, value, member), _AstChildMutator)) {
      // don't call child mutators
    } else static if (is(typeof(__traits(getMember, value, member)) : Ast) && member != "parent") {
      // don't follow non-child AST pointers, but print some basic info
      auto subfmt = fmt.indent(member);
      oneLine(__traits(getMember, value, member), subfmt);
    } else static if (!is(typeof(__traits(getMember, value, member)))) {
      // member is not accessible (needs to be before some std.traits such as isFunction)
    } else static if (isFunction!(__traits(getMember, value, member))) {
      // member is a function
    } else {
      auto subfmt = fmt.indent(member);
      static if (is(typeof(__traits(getMember, value, member)) == class)) {
	if (__traits(getMember, value, member) is null) {
	  // class data member null pointer
	  writefln("%-*s <null>", subfmt.nameWidth, subfmt.prefix);
	  return;
	}
      }

      static if (is(typeof(__traits(getMember, value, member).tabulate(subfmt)))) {
	// data member with "tabulate" method
	__traits(getMember, value, member).tabulate(subfmt);
      } else static if (is(typeof(tabulate(__traits(getMember, value, member), subfmt)))) {
	// "tabulate" function invoked with data member as argument
	tabulate(__traits(getMember, value, member), subfmt);
      } else static if (is(typeof(to!string(__traits(getMember, value, member))))) {
	writefln("%-*s %s", subfmt.nameWidth, subfmt.prefix, to!string(__traits(getMember, value, member)));
      }
    }
  }
}

// catch-all
void tabulate(T)(T value, TabulateFormat fmt)
if (!isBasicType!T && !isArray!T && !isAggregateType!T) {
  writefln("%-*s <not printable>", fmt.nameWidth, fmt.prefix);
}
