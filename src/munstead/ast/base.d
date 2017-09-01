module munstead.ast.base;

import munstead.ast.tabulate;
import munstead.core.byteorder;

import std.algorithm: filter, map;
import std.array: array;
import std.conv: to;
import std.exception: enforce;
import std.range: iota;
import std.range.primitives: isForwardRange, isInputRange, ElementType;
import std.typecons;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                 HOW TO DEFINE A NEW AST NODE TYPE
//
// 1. Your new node class MUST derive directly or indirectly from class Ast:
//
//      class MyBinaryExprNode: Ast {
//
// 2. Your new class MUST override all the methods necessary to be a proper AST node. The easiest way to do this
//    is to mix in the AST node features:
//
//          mixin AstNodeFeatures;
//
// 3. You MUST define a local "AstChidren" struct whose members are the child nodes -- don't create a data member
//    for this type.  The members of the AstChildren struct are used to build accessors:
//
//          struct AstChildren {
//
// 4. Children of your node are declared inside the Rosetta struct and must derive from class Ast:
//
//              MyBinaryExprNode lhs, rhs;
//          }
//    }
//
// 5. The Rosetta children can be accessed as if they were data members of your class, therefore you class MUST not
//    have members with conflicting names.  Attaching an AST to a node by assignment adjusts parent/child relationships
//    after validating that the new subtree isn't already part of some other AST:
//
//    auto lhs = new MyBinaryExprNode;
//    auto node = new MyBinaryExprNode;
//    node.lhs = lhs;
//    assert(lhs.parent is node);
//    assert(node.parent is null);
//
//    node.rhs = lhs; // error: lhs is already attached at node.rhs
//
// 6. The children can also all be accessed with the "children" function, which returns an array of children and is
//    useful for writing iterators that don't care about the member names.

// Top-level mixin. This is what most users will use to make their class an AST node.
mixin template AstNodeFeatures() {
  mixin AstChildrenFeatures;
  mixin AstTabulateFeatures;
}

// @Attributes used internally to decorate certain generated code
struct _AstChildAccessor {} // marks generated functions that access child nodes
struct _AstChildMutator {}  // marks generated functions that modify child nodes

// Mixes in child node accessors and various functions if the node has an AstChildren struct, otherwise does nothing.
mixin template AstChildrenFeatures() {
  static if (!is(AstChildren == struct))
    pragma(msg, __FILE__, "(", __LINE__, "): Error: AST node type ", typeof(this).stringof,
	   " lacks a struct AstChildren declaration");
	   
  import std.traits;

  // Data member that should be accessed only by these functions. We give it an unwieldy name to
  // discourage users from using it directly.
  private AstChildren _AST_IMPL_children_;

  mixin AstChildrenDeclarations!(FieldNameTuple!AstChildren);

  override Ast[] children() {
    import std.traits: hasUDA;
    Ast[] ret;
    foreach (member; __traits(allMembers, typeof(this))) {
      static if (is(typeof(__traits(getMember, this, member)))) {
	static if (hasUDA!(__traits(getMember, this, member), _AstChildAccessor)) {
	  ret ~= __traits(getMember, this, member);
	}
      }
    }
    return ret;
  }

  override string[] childNames() {
    string[] ret;
    foreach (member; __traits(allMembers, typeof(this))) {
      static if (is(typeof(__traits(getMember, this, member)))) {
	static if (hasUDA!(__traits(getMember, this, member), _AstChildAccessor))
	  ret ~= [member];
      }
    }
    return ret;
  }
}

//// Statically returns an array with all the names of the AST children.
//string[] astChildNames(T)() if (is(T : Ast)) {
//  import std.traits: hasUDA;
//  string[] ret;
//  foreach (member; __traits(allMembers, T)) {
//    static if (is(typeof(__traits(getMember, T, member)))) {
//      static if (hasUDA!(__traits(getMember, T, member), _AstChildAccessor)) {
//	ret ~= [member];
//      }
//    }
//  }
//  return ret;
//}

// Declare access functions for all child nodes
mixin template AstChildrenDeclarations(children...) if (children.length == 0) {}
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
  mixin("@property @_AstChildMutator " ~ typeof(__traits(getMember, _AST_IMPL_children_, childName)).stringof ~ " " ~
	childName ~ "(" ~ typeof(__traits(getMember, _AST_IMPL_children_, childName)).stringof ~ " newNode)
        out(ret) {
            assert(ret is null || ret._AST_IMPL_parent_ is this);
        }
        body {
            alias Node = " ~ typeof(__traits(getMember, _AST_IMPL_children_, childName)).stringof ~ ";
            Node oldNode = _AST_IMPL_children_." ~ childName ~ ";
            if (oldNode != newNode) {
                if (newNode !is null) {
	            import std.exception: enforce;
	            enforce(newNode._AST_IMPL_parent_ is null, \"rhs node is already attached to an AST\");
	            newNode._AST_IMPL_parent_ = this;
                }
                if (oldNode !is null)
	            oldNode._AST_IMPL_parent_ = null;
	        _AST_IMPL_children_." ~ childName ~ " = newNode;
            }      
            return newNode;
        }");
}

mixin template AstTabulateFeatures() {
  static import munstead.ast.tabulate;

  // Print node contents in a tabulare format
  override void tabulate(munstead.ast.tabulate.TabulateFormat fmt) {
    munstead.ast.tabulate.tabulate(this, fmt);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Base class for all AST nodes.
class Ast {
  Ast _AST_IMPL_parent_;	 // Do not use directly except by mixins! Use "parent" property instead.
  protected size_t maxErrors_;   // max number of errors to accumulate before throwing an exception
  protected Exception[] errors_; // errors encountered at this node (not cumulative across the children)

  // List of all child nodes defined directly in this and inherited through subclassing.
  abstract Ast[] children();

  // List of all child node names defined directly in this and inherited through subclassing.
  abstract string[] childNames();

  // Prints this node in a useful format.
  final void tabulate(string name = "ast") {
    tabulate(TabulateFormat(name));
  }
  abstract void tabulate(TabulateFormat fmt);

  // Every node except the root node has a parent. The root node's parent is null.  The parent property is read-only and
  // adjusted by inserting or removing this node from an AST.
  Ast parent() @property {
    return _AST_IMPL_parent_;
  }

  // Add another error to the list of errors for this node.  Throws the exception if there are too many errors.  This is
  // typically used for things like parse errors in situations where the parser tries to recover.
  void appendError(Exception ex) {
    assert(ex !is null);
    if (errors_.length >= maxErrors_)
      throw ex;
    errors_ ~= [ ex ];
  }

  // Clear errors by removing errors from this node.
  void clearErrors() {
    errors_ = [];
  }

  // Returns the errors associated with this node.
  Exception[] errors() {
    return errors_;
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// An AstList is an AST node whose only purpose is to hold an ordered list of AST children.
class AstList(T): Ast
if (is(T : Ast)) {
  private T[] children_;

  // List of all the children of this node, returning base class. See also opIndex for "[]"
  override Ast[] children() {
    return children_.map!(t => cast(Ast) t).array;
  }
  
  // Child names are just integers 0 .. N
  override string[] childNames() {
    return iota(children_.length).map!(a => to!string(a)).array;
  }

  override void tabulate(munstead.ast.tabulate.TabulateFormat fmt) {
    auto subfmt = fmt.indent("nChildren");
    munstead.ast.tabulate.tabulate(children_.length, subfmt);
  }

  // All children in the type stored, not the base type.
  T[] opIndex() {
    return children_;
  }

  T opIndex(size_t idx) {
    assert(idx < children_.length);
    return children_[idx];
  }

  // Add a new child AST to the front of the list.
  void pushFront(T node) {
    enforce(node._AST_IMPL_parent_ is null, "an AST node cannot have two parents");
    node._AST_IMPL_parent_ = this;
    children_ = [node] ~ children_;
  }

  // Add a new child AST to the end of the list.
  void pushBack(T node) {
    enforce(node._AST_IMPL_parent_ is null, "an AST node cannot have two parents");
    node._AST_IMPL_parent_ = this;
    children_ ~= [node];
  }

  // Remove all children from the list.
  void clear() {
    foreach (child; children_) {
      assert(child._AST_IMPL_parent_ is this); // Oops. Something went majorly wrong in the past.
      child._AST_IMPL_parent_ = null;
    }
    children_ = [];
  }

  // True if there are no children
  pure bool empty() const @property @safe {
    return children_.length == 0;
  }

  // Number of children
  pure size_t length() const @property @safe {
    return children_.length;
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
enum VisitOrder { PRE, POST };

// Does a depth first search of the tree and visits each node twice: once on the way down and once on the way back
// up. The range elements are node/order pairs.  The elements of the returned range are tuples with three named members:
//   + "node" is the node being visited
//   + "order" is a VisitOrder, either PRE or POST
//   + "depth" is the depth of this node w.r.t. the starting node
auto prePostOrder(Ast ast) {
  static struct PrePostOrder {
    alias Pair = Tuple!(Ast, "node", VisitOrder, "order", size_t, "depth");

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

    Pair front() {
      assert(!empty);
      return Pair(path[$-1][0], mode, path.length - 1);
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

// Visit the nodes of an AST in pre-order by visiting the parent first and then the children.  The range elements are
// AST nodes.
auto preOrder(Ast ast) {
  return prePostOrder(ast)
    .filter!(visit => visit.order == VisitOrder.PRE)
    .map!(visit => visit.node);
}

// Visit nodes of an AST in post-order by visiting the children before the parent.  The range elements are AST nodes.
auto postOrder(Ast ast) {
  return prePostOrder(ast)
    .filter!(visit => visit.order == VisitOrder.POST)
    .map!(visit => visit.node);
}
	  
// Filter an AST range to have only nodes that are type T or derived from T.
auto nodeType(T, Range)(Range range)
if (isInputRange!Range && is(ElementType!Range : Ast)) {
  return range.filter!(node => cast(T) node !is null).map!(node => cast(T) node);
}

// Iterate up through the tree following parent pointers.
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
  return Parents(ast);
}

// Return the nearest parent that's the specified type, or null if no such parent exists.
T ancestor(T)(Ast ast) {
  while (ast !is null) {
    if (T ret = cast(T) ast)
      return ret;
    ast = ast.parent;
  }
  return null;
}

// Print an AST for debugging purposes
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
