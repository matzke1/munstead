/** Abstract syntax trees.
 *
 *  An abstract syntax tree is a tree representation of the syntactic structure of the input language, in this case
 *  a binary specimen. For instance, an executable in the Executable and Linkable Format (ELF) common in Unix-like
 *  operating systems, consists of a file header that points to a section table and/or program header table, which
 *  in turn point to sections or segments, which in turn often contain things like symbols, relocations, etc. An ELF
 *  file can be represented in the AST by having a root node for the file header, whose children are the section
 *  table and/or program header table, etc.
 *
 *  Within Munsead, the tree and a node of the tree are interchangable: a tree is represented by the node which
 *  serves as that tree's root. Therefore, any node of one tree can serve as the root of some other (sub)tree. However,
 *  since every node which is a child of some other node has a non-null "parent" pointer, it's possible to escape from a
 *  subtree by following the parent pointer out of the subtree root node. This is considered an important feature.
 *
 *  As mentioned, each node that is a child of some other node has pointer to its parent node.  The user never needs to
 *  update these pointers manually -- they are kept consistent by the library. The parent pointer is always named "parent",
 *  but the children can have any name and type (as long as the type derives from `munstead.ast.base.Ast`). For example,
 *  assuming the child is named "mykiddo":
 *
 *  ---
 *  NodeType1 foo = ...;
 *  NodeType2 bar = ...;
 *
 *  assert(bar.parent is null);
 *  foo.mykiddo = bar;
 *  assert(bar.parent is foo);
 *  baz.mykiddo = bar; // runtime error: bar cannot have two parents
 *  ---
 *
 *  Discussion:
 *
 *  Anyone can declare their own AST node types, and the parsers even have a mechanism by which you can substitute
 *  you own derived node for one of its super classes.  In order to declare an AST node type, the following three
 *  things must be done:
 *
 *  $(LI The class must be derived ultimately from `munstead.ast.base.Ast`.)
 *
 *  $(LI The class must have a `mixin munstead.ast.base.AstNodeFeatures;`.)
 *
 *  $(LI The class must declare a local struct named `AstNodeChildren` (possibly empty) whose member types all
 *  derive from `munstead.ast.base.Ast`.  These data members will be made available (via `@property` functions)
 *  in the new node class. This is how the library enforces consistent parent/child relationships.) */
module munstead.ast.base;

import munstead.ast.tabulate;
import munstead.core.byteorder;
import munstead.core.exception;
import munstead.core.util;

import std.algorithm: each, filter, map;
import std.array: array;
import std.conv: to;
import std.exception: enforce;
import std.range: iota, take;
import std.range.primitives: isForwardRange, isInputRange, ElementType;
import std.traits: isIntegral;
import std.typecons;

/** Top-level AST node mixin.
 *
 *  This is one of the few requirements in order to define a new AST node. The others are listed in the documentation
 *  for this module. */
mixin template AstNodeFeatures() {
  mixin AstConstructorFeatures;
  mixin AstChildrenFeatures;
  mixin AstTabulateFeatures;
}

// @Attributes used internally to decorate certain generated code
struct _AstChildAccessor {} // marks generated functions that access child nodes
struct _AstChildMutator {}  // marks generated functions that modify child nodes

/** Provides constructor features to an AST node class.
 *
 *  Users will normally want to use `AstNodeFeatures` instead, which mixes in these constructor features as well.
 *
 *  The parsers in this library always construct new AST nodes by saying `T node = T.instance()` rather than the
 *  more usual `T node = new T`.  Among other things, this mixin defines the `instance` static data member which
 *  is a function pointer initialized to return `new T`. However, users can reassign this to be functions that return
 *  their own node types. For example, if the user has a `MySymbol` that inherits from `ElfSymbol!32`, they can cause 
 *  the Munstead ELF parser to use their type by saying `ElfSymbol!32.instance = MySymbol.instance` (without parentheses). */
mixin template AstConstructorFeatures() {
  import std.traits: isAbstractClass;

  // Trying to instantiate an abstract type will result in a run-time error. It would be better if "instance" were not
  // even defined for such a type, but I couldn't get isAbstractClass to work outside function scope.
  static typeof(this) function() instance = function typeof(this)() {
    /** Instantiate an object of this type, or derived type. */
    static if (!isAbstractClass!(typeof(this))) {
      return new typeof(this);
    } else {
      assert(0, typeof(this).stringof ~ " cannot be instantiated because it is abstract");
    }
  };
}

/** Provides mechanisms dealing with child nodes.
 *
 *  Most users will want to mix in `AstNodeFeatures` instead, which mixes in these child features as well. */
mixin template AstChildrenFeatures() {

  // If you get this error, you most likely forgot that all AST nodes you declare must also have an "AstChildren"
  // struct that lists the types and names of all children.  All the child types must ultimately derive from
  // munstead.ast.base.Ast.
  static if (!is(AstChildren == struct))
    pragma(msg, __FILE__, "(", __LINE__, "): Error: AST node type ", typeof(this).stringof,
           " lacks a struct AstChildren declaration");
           
  import std.traits;

  // DON'T ACCESS THIS MEMBER IN YOUR OWN CODE!!!
  //
  // Data member that should be accessed only by these functions. We give it an unwieldy name to
  // discourage users from using it directly.
  private AstChildren _AST_IMPL_children_;

  mixin AstChildrenDeclarations!(FieldNameTuple!AstChildren);

  /** Array of all children.
   *
   *  Returns an array of pointers to all the children in arbitrary order (although consistent with the names
   *  returned by the `childNames` method. The array members are of type `Ast` because this is the base class for all
   *  AST nodes. You'll need to dynamic-cast them back to the types you're expecting.  By the way, you do realize you
   *  can access the children directly by name, right? This method exists mostly for people writing custom traversals. */
  override Ast[] children() {
    import std.traits: hasUDA;
    Ast[] ret;
    // The extra static if checking is because hasUDA reports errors instead of returning false
    foreach (member; __traits(allMembers, typeof(this))) {
      static if (is(typeof(__traits(getMember, this, member)))) {
        static if (is(typeof(hasUDA!(__traits(getMember, this, member), _AstChildAccessor)))) {
          static if (hasUDA!(__traits(getMember, this, member), _AstChildAccessor))
            ret ~= __traits(getMember, this, member);
        }
      }
    }
    return ret;
  }

  /** Names of all children.
   *
   *  Returns the string names of all the children. The return value lists the names in the same order that the `children`
   *  method would return their pointers.  By the way, you do realize you can access the children directly by name, right?
   *  This method exists mostly for people writing custom traversals. */
  override string[] childNames() {
    string[] ret;
    // The extra static if checking is because hasUDA reports errors instead of returning false
    foreach (member; __traits(allMembers, typeof(this))) {
      static if (is(typeof(__traits(getMember, this, member)))) {
        static if (is(typeof(hasUDA!(__traits(getMember, this, member), _AstChildAccessor)))) {
          static if (hasUDA!(__traits(getMember, this, member), _AstChildAccessor))
            ret ~= [member];
        }
      }
    }
    return ret;
  }
}

/** Mixin child access methods.
 *
 *  Most users will want to mix in `AstNodeFeatures` instead, which mixes in these child access methods as well. */
mixin template AstChildrenDeclarations(children...) if (children.length == 0) {}

/** ditto */
mixin template AstChildrenDeclarations(children...) if (children.length > 0) {
  mixin AstChildAccessors!(children[0]);
  mixin AstChildrenDeclarations!(children[1..$]);
}

// Declare access functions for one child node given by name
mixin template AstChildAccessors(string childName) {

  // Debugging to show what accessors are being generated
  pragma(msg, "ROSETTA: generating accessors for ", typeof(this).stringof, ".", childName);

  // Does a super class already have a member with this same name?
  static if (is(typeof(__traits(getMember, super, childName))))
    pragma(msg, __FILE__, "(", __LINE__, "): Error: derived AST class ", typeof(this).stringof,
           " should not override ", childName, " from base class ", typeof(super).stringof);
    
  // Getter:  @property @_AstChildAccessor inout(T) foo() inout { ... }
  // If you get errors here about "cannot implicitly override" they you probably have a subclass
  // that's declare a child node with a name it also inherits from a base class. Another way this
  // might happen is if you forgot to add a struct AstChildren (even if it should be empty).
  mixin("@property @_AstChildAccessor inout(" ~ typeof(__traits(getMember, _AST_IMPL_children_, childName)).stringof ~ ") " ~
        childName ~ "() inout " ~
        "{ return _AST_IMPL_children_." ~ childName ~ "; }");

  // Setter: @property @_AstChildMutator T foo(T newNode) { ...; return newNode; }
  // Also enforces that newNode is not already a child in an AST (but exception is made for no-op assignments).
  // Note: the crazy \n and ~ are because emacs d-mode syntax highlighting gets confused otherwise.
  mixin("@property @_AstChildMutator " ~ typeof(__traits(getMember, _AST_IMPL_children_, childName)).stringof ~ " " ~
        childName ~ "(" ~ typeof(__traits(getMember, _AST_IMPL_children_, childName)).stringof ~ " newNode)\n" ~
        "out(ret) {\n" ~
        "    assert(ret is null || ret._AST_IMPL_parent_ is this);\n" ~
        "}\n" ~
        "body {\n" ~
        "    alias Node = " ~ typeof(__traits(getMember, _AST_IMPL_children_, childName)).stringof ~ ";\n" ~
        "    Node oldNode = _AST_IMPL_children_." ~ childName ~ ";\n" ~
        "    if (oldNode != newNode) {\n" ~
        "        if (newNode !is null) {" ~
        "            import std.exception: enforce;\n" ~
        "            enforce(newNode._AST_IMPL_parent_ is null, \"rhs node is already attached to an AST\");\n" ~
        "            newNode._AST_IMPL_parent_ = this;\n" ~
        "        }\n" ~
        "        if (oldNode !is null)\n" ~
        "            oldNode._AST_IMPL_parent_ = null;\n" ~
        "        _AST_IMPL_children_." ~ childName ~ " = newNode;\n" ~
        "    }\n" ~
        "    return newNode;\n" ~
        "}\n");
}

mixin template AstTabulateFeatures() {
  static import munstead.ast.tabulate;

  // Print node contents in a tabulare format
  override void tabulate(munstead.ast.tabulate.TabulateFormat fmt) {
    munstead.ast.tabulate.tabulate(this, fmt);
  }
}



////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Ast
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/** Base class for all AST nodes.
 *
 *  When declaring a new AST node type, inheriting directly or indirectly from `Ast` is only one of the requirements.
 *  The others are listed in the documentation for this module. */
class Ast {
  Ast _AST_IMPL_parent_;         // Do not use directly except by mixins! Use "parent" property instead.

  static size_t totalErrorCount; /** Maximum number of errors to accumulate before throwing an exception. */
  static size_t totalErrorLimit; /** Total number of errors accumulated since this was last reset. */
  static bool printErrors;       /** Print error messages regardless of whether they cause exceptions or are caught. */
  Exception[] errors;            /** Errors associated with this particular node. */

  // List of all child nodes defined directly in this and inherited through subclassing.
  abstract Ast[] children();

  // List of all child node names defined directly in this and inherited through subclassing.
  abstract string[] childNames();

  /** Nearest parent of specified type.
   *
   *  Returns `this` or its closest ancestor by following `parent` links until an AST node is found which derives
   *  from type `T`.  Returns null if the root of the tree is reached without finding such a node. */
  T ancestor(T)() {
    for (Ast ast = this; ast !is null; ast = ast.parent) {
      if (T ret = cast(T) ast)
        return ret;
    }
    return null;
  }

  // Prints this node in a useful format.
  final void tabulate(string name = "ast") {
    tabulate(TabulateFormat(name));
  }
  abstract void tabulate(TabulateFormat fmt);

  /** Parent node.
   *
   *  Since the "T" in "AST" stands for "tree", every node of an AST which is a child of some other node in the AST has
   *  a non-null `parent` which is that other node.  Parent pointers are updated automatically by the library whenever
   *  a node is assigned as the child of some other node. */
  Ast parent() @property {
    return _AST_IMPL_parent_;
  }

  /** Add an error to the list of errors for this node.
   *
   *  Throws an exception if there are too many errors.  The number of errors and the total limit can be accessed by
   *  the `totalErrorCount` and `totalErrorLimit` static data members of `munstead.ast.base.Ast`.  The errors for
   *  a particular node can be accessed with its `errors` data member.
   * 
   *  Errors are usually associated with some chain of parsing events which is provided by the `loc` argument. For instance,
   *  if an error is found while parsing a PE executable's import lookup table entry name, the chain will point back to the
   *  import lookup table entry, then to the import lookup table directory, then to the import lookup table section, then
   *  to the PE file header, and finally to the file being parsed.  Each entry in this chain contains details about the
   *  location within either the file or the virtual memory at which part of the file was loaded.
   *
   *  The `sourceFile` and `lineNumber` arguments are for the sake of throwing an exception (if any is thrown) and are
   *  the locations within the library source code, probably not too useful to users but required nonetheless by the D
   *  runtime. */
  void appendError(ParseLocation loc, string mesg, string sourceFile = __FILE__, size_t lineNumber = __LINE__) {
    if (printErrors) {
      import std.stdio;
      stderr.writeln("error: ", mesg);
      for (ParseLocation l = loc; l !is null; l = l.parent)
        stderr.writeln("    in ", loc);
    }

    auto error = new ParseError(loc, mesg, sourceFile, lineNumber);
    error.sequenceNumber = totalErrorCount + 1;
    errors ~= error;

    if (++totalErrorCount >= totalErrorLimit)
      throw errors[$-1];
  }
}



////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// AstList
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/** A list of other AST nodes.
 *
 *  Because of the specific way that we want AST traversals to operate, and so that the library can control consistency
 *  of parent/child links, any time you have a list of AST children that would normally be an array, you must instead
 *  encapsulate them in an `AstList`.  For instance, a list of symbols in a symbol table might look something like this:
 *
 *  ---
 *  class SymbolTable: Ast {
 *      mixin AstNodeFeatures;
 *      struct AstChildren {
 *          AstList!Symbol symbols;
 *      }
 *  }
 *  ---
 *
 *  Note: We may change the API to give it more of an array-like feel. */
class AstList(T): Ast
if (is(T : Ast)) {
  private T[] children_;

  override Ast[] children() {
    return children_.map!(t => cast(Ast) t).array;
  }
  
  override string[] childNames() {
    return iota(children_.length).map!(a => to!string(a)).array;
  }

  override void tabulate(munstead.ast.tabulate.TabulateFormat fmt) {
    auto subfmt = fmt.indent("nChildren");
    munstead.ast.tabulate.tabulate(children_.length, subfmt);
  }

  /** List of all children. */
  T[] opIndex() {
    return children_;
  }

  /** Single child. */
  T opIndex(size_t idx) {
    assert(idx < children_.length);
    return children_[idx];
  }

  /** Last child, provided list is not empty. */
  pure T back() @property @safe {
    assert(children_.length > 0);
    return children_[$-1];
  }

  /** Adds a new node to the front of the list. */
  void pushFront(T node) {
    enforce(node._AST_IMPL_parent_ is null, "an AST node cannot have two parents");
    node._AST_IMPL_parent_ = this;
    children_ = [node] ~ children_;
  }

  /** Adds a new node to the end of the list. */
  void pushBack(T node) {
    enforce(node._AST_IMPL_parent_ is null, "an AST node cannot have two parents");
    node._AST_IMPL_parent_ = this;
    children_ ~= [node];
  }

  /** Removes the last node. */
  void popBack() {
    assert(!empty);
    Ast child = children_[$-1];
    assert(child !is null);
    assert(child._AST_IMPL_parent_ is this);
    children_.length = children_.length - 1;
    child._AST_IMPL_parent_ = null;
  }

  /** Remove all nodes. */
  void clear() {
    foreach (child; children_) {
      assert(child._AST_IMPL_parent_ is this); // Oops. Something went majorly wrong in the past.
      child._AST_IMPL_parent_ = null;
    }
    children_ = [];
  }

  /** True if the list is empty. */
  pure bool empty() const @property @safe {
    return children_.length == 0;
  }

  /** Number of nodes in list. */
  pure size_t length() const @property @safe {
    return children_.length;
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Traversals
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/** Order that a node is visited with respect to its children. */
enum VisitOrder {
  PRE,                          /** Visit the parent before visiting its the children. */
  POST                          /** Visit the parent after visiting its children. */
};

/** Perform a depth first search of a tree, visiting each node twice.
 *
 *  Starting at the specified node, visit that node in `PRE` mode, then recursively visit each of its children using
 *  this same algorithm, then visit the specified node in `POST` mode. In essence, each node is visited once on the
 *  way down from parents to children, then again on the way back out.
 *
 *  The return value is a range whose elements are tuples with three named members:
 *  $(LI `node` is the node being visited.)
 *  $(LI `order` is a `VisitOrder`, either `PRE` or `POST`.)
 *  $(LI `depth` is the depth of this node w.r.t. the starting node.) */
auto prePostOrder(Ast ast) {
  static struct PrePostOrder {
    alias VisitInfo = Tuple!(Ast, "node", VisitOrder, "order", size_t, "depth");

    // Each path element is a list of children yet-to-visit (the left-most is the one currently being visited). The path
    // consists of path elements from the root of the tree down to the currently visited level.  The array consisting of
    // all the left-most nodes of the path elements represents the path in the tree from the root to the current node.
    alias PathElmt = Ast[];
    private PathElmt[] path;
    private VisitOrder mode; // whether front is a pre- or post- visitation of the node.

    invariant {
      assert(path.length == 0 || path[$-1].length > 0);
    }

    private this(Ast ast) {
      if (ast !is null)
        path = [[ ast ]];
      mode = VisitOrder.PRE;
    }

    pure bool empty() const @property {
      return 0 == path.length;
    }

    VisitInfo front() {
      assert(!empty);
      return VisitInfo(path[$-1][0], mode, path.length - 1);
    }

    void popFront() {
      assert(!empty);
      if (VisitOrder.PRE == mode) {
        auto children = path[$-1][0].children().filter!(node => node !is null).array;
        if (children.length > 0) {
          path ~= [ children ]; // enter the first child at the next level
        } else {
          mode = VisitOrder.POST; // leave the current node since it has no children
        }
      } else {
        path[$-1] = path[$-1][1..$]; // done with the post visit for this node
        if (path[$-1].length > 0) {
          mode = VisitOrder.PRE; // enter the sibling
        } else {
          path = path[0..$-1]; // leave the parent
        }
      }
    }

    PrePostOrder save() {
      PrePostOrder ret;
      ret.path = path.dup;
      ret.mode = mode;
      for (size_t i = 0; i < ret.path.length; ++i)
        ret.path[i] = ret.path[i].dup;
      return ret;
    }
  }

  static assert(isForwardRange!PrePostOrder);
  return PrePostOrder(ast);
}

/** Visit the nodes of an AST in DFS pre-order.
 *
 *  Performs a depth-first traversal of the AST starting at the specified node and visiting each parent before its
 *  children. The return value is a range whose elements are AST nodes. */
auto preOrder(Ast ast) {
  return prePostOrder(ast)
    .filter!(visit => visit.order == VisitOrder.PRE)
    .map!(visit => visit.node);
}

/** Visit the nodes of an AST in DFS post-order.
 *
 *  Performs a depth-first traversal of the AST starting at the specified node and visiting all children before
 *  visiting the parent.  The return value is a range whose elements are AST nodes. */
auto postOrder(Ast ast) {
  return prePostOrder(ast)
    .filter!(visit => visit.order == VisitOrder.POST)
    .map!(visit => visit.node);
}

/** Filter a range by type.
 *
 *  Given a range of AST nodes, return only those nodes of type T or derived from T.  The return value is another range. */
auto nodeType(T, Range)(Range range)
if (isInputRange!Range && is(ElementType!Range : Ast)) {
  return range.map!(node => cast(T) node).filter!"a !is null";
}

/** Iterate up through the tree following parent pointers.
 *
 *  The starting node is excluded since the common definition of "parent" is that a node is not its own parent.  The return
 *  value is a range whose elements are AST nodes. */
auto parents(Ast ast) {
  static struct Parents {
    private Ast current;

    private this(Ast ast) {
      current = ast;
    }

    pure bool empty() const @safe @property {
      return current is null;
    }

    Ast front() {
      assert(!empty);
      return current;
    }

    void popFront() {
      assert(!empty);
      current = current.parent;
    }

    Parents save() {
      Parents ret;
      ret.current = this.current;
      return ret;
    }
  }
  return Parents(ast is null ? null : ast.parent);
}

/** Print an AST for debugging purposes. */
void dumpAst(Ast ast) {
  import std.stdio;
  import std.string;
  enum indentWidth = 2;

  foreach (visit; ast.prePostOrder) {
    if (visit.order == VisitOrder.PRE) {
      string prefix = rightJustify("", indentWidth * visit.depth);
      writefln("%s%s", prefix, visit.node);
      auto fmt = TabulateFormat(prefix ~ rightJustify("", indentWidth));
      visit.node.tabulate(fmt);
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
unittest {} // to prevet next unit test from being documented

unittest {
  import std.algorithm;
  import std.conv;
  import std.exception;
  import std.range;
  import std.stdio;
  writeln("unit tests: ", __FILE__);

  class N1: Ast {
    mixin AstChildrenFeatures;
    mixin AstTabulateFeatures;
    struct AstChildren {
      N1 n1child;
    }
    string name;
    this() {}
    this(string name) {
      this.name = name;
    }
  }

  class N2: N1 {
    mixin AstChildrenFeatures;
    mixin AstTabulateFeatures;
    struct AstChildren {
      N2 n2child;
    }
    this() {}
    this(string name) {
      super(name);
    }
  }

  // Construct some objects
  auto n1a = new N1("n1a");
  auto n1b = new N1("n1b");
  auto n2a = new N2("n2a");
  auto n2b = new N2("n2b");

  // Check that parent and child pointers are all null
  assert(n1a.n1child is null);
  assert(n1a.parent is null);
  assert(n1b.n1child is null);
  assert(n1b.parent is null);
  assert(n2a.n1child is null);
  assert(n2a.n2child is null);
  assert(n2a.parent is null);
  assert(n2b.n1child is null);
  assert(n2b.n2child is null);
  assert(n2b.parent is null);

  // Add a couple children in most-derived class and it's super class.
  // The parent pointers should adjust automatically.
  n2a.n2child = n2b;
  assert(n2a.n2child is n2b);
  assert(n2b.parent is n2a);
  n2a.n1child = n1b;
  assert(n2a.n1child is n1b);
  assert(n1b.parent is n2a);

  // Child names
  assert(n2a.childNames.sort.equal(["n1child","n2child"]));
  assert(n2a.children.length == 2);
  assert(n2a.children.filter!(a => a is n2b).count == 1);
  assert(n2a.children.filter!(a => a is n1b).count == 1);
  
  // Traversal
  foreach (i, visit; n2a.prePostOrder.enumerate) {
    switch (i) {
    case 0:
      assert(visit.node is n2a, visit.node.to!string);
      assert(visit.order == VisitOrder.PRE);
      assert(visit.depth == 0);
      break;

    case 1:
      assert(visit.node is n2b, visit.to!string);
      assert(visit.order == VisitOrder.PRE);
      assert(visit.depth == 1);
      break;

    case 2:
      assert(visit.node is n2b, visit.node.to!string);
      assert(visit.order == VisitOrder.POST);
      assert(visit.depth == 1);
      break;

    case 3:
      assert(visit.node is n1b, visit.node.to!string);
      assert(visit.order == VisitOrder.PRE);
      assert(visit.depth == 1);
      break;

    case 4:
      assert(visit.node is n1b, visit.node.to!string);
      assert(visit.order == VisitOrder.POST);
      assert(visit.depth == 1);
      break;
       
    case 5:
      assert(visit.node is n2a, visit.node.to!string);
      assert(visit.order == VisitOrder.POST);
      assert(visit.depth == 0);
      break;

    default:
      assert(0, "too many iterations");
    }
  }
  assert(n2a.prePostOrder.count == 6);

  // Change the child, with parent pointers adjusting
  n2a.n1child = n1a;
  assert(n2a.n1child is n1a);
  assert(n1a.parent is n2a);
  assert(n1b.parent is null);

  // Assign the same child again
  n2a.n1child = n1a;
  assert(n2a.n1child is n1a);
  assert(n1a.parent is n2a);

  // Assign same child to a different point in the tree
  assertThrown!Exception(n2a.n1child = n2b);
  assert(n2a.n1child is n1a);
  assert(n2a.n2child is n2b);
  assert(n2b.parent is n2a);

  // Remove a child, parent pointers adjusting
  n2a.n1child = null;
  assert(n2a.n1child is null);
  assert(n1a.parent is null);

}

