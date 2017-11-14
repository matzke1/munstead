// Demonstrates some cool D language features, etc.
import std.stdio;                                                 // like C++ "#include" but faster


// Things to know about D
//    1. C++ has ".", "->", and "::" but D uses "." for all three.
//    2. D has a garbage collector, so although you see "new" you'll seldom see "delete"


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Some interesting D capabiliites
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void demoInteresting() {
  import std.conv;

  // Convert anything to a string
  int answer = 42;
  string s1 = to!string(answer);            			  // like: boost::lexical_cast<std::string>(answer)

  // D's uniform function call syntax (UFCS) means the first arg can be on the left of the function name
  string s2 = answer.to!string;

  // UFCS is useful because it means you can read from left-to-right instead of C's right-to-left. Lets say we have
  // some function that takes an array of something and prints it in tabular format. It needs to decide how wide the
  // column should be in order to hold the array indexes.  The C++ implementation looks like this:
  //
  //    template<T> size_t indexColumnWidth(const std::vector<T> &stuff) {
  //      return boost::lexical_cast<std::string>(stuff.size()).size();
  //    }
  //
  size_t indexColumnWidth(T)(T[] stuff) {
    return stuff.length.to!string.length;
  }
}  



  
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Demo enums
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Some basic stuff about enums
void demoEnums() {
  enum Color { RED, GREEN, BLUE }

  auto c1 = Color.RED;                                            // The constants need to be qualified
  // Color c2 = 1;                                                // compile error to assign number to enum, as in C++

  // The C++ way to print enum names when given a value (don't do it this way in D!)
  write("c1 = ");
  with (Color) {                                                  // "with" is a little like C++ "using"
    final switch (c1) {                                           // "final" means error if a case is missing
      case RED:                                                   // no qualification necessary due to "with"
        writeln("RED");                                           // like C++: std::cout <<"RED" <<std::endl;
        break;
      case GREEN:
        writeln("GREEN");
        break;
      case BLUE:
        writeln("BLUE");
        break;
    }
  }

  // The correct way in D
  writeln("c1 = ", c1);                                           // prints "c1 = RED"
}



// We can use the enum features and introspection to create a nice BitFlags type.
void demoBitFlags() {
  enum Access : uint {                                            // Like C++11, enums can derive from some type
    READ    = 0x0001,
    WRITE   = 0x0002,
    EXECUTE = 0x0004,
    RW      = 0x0003,                                             // multiple flag bits
    RWX     = 0x0007                                              // ditto
  }

  with (Access) {                                                 // so I don't have to keep qualifying the constants
    import munstead.core.bitflags;                                // something I wrote that declares the BitFlags template

    auto flags1 = BitFlags!Access(READ, EXECUTE);                 // "BitFlags!Access" is like C++ "BitFlags<Access>"
    assert(flags1.isSet(READ));                                   // read bit is set
    assert(flags1.isClear(WRITE));                                // but write bit is clear
    writeln("flags1 = ", flags1);                                 // prints "flags1 = READ|EXECUTE"

    auto flags2 = flags1.including(WRITE);                        // like C++: auto flags2 = (Access)(flags1 | WRITE)
    auto flags3 = flags2.excluding(READ);                         // variable length arguments work fine
    writeln("flags3 = ", flags3);                                 // prints "flags3 = WRITE|EXECUTE"

    Access invalidFlag = cast(Access) 128;                        // like C++, can't assign integer to enum w/out cast
    auto flags4 = BitFlags!Access(READ, WRITE, invalidFlag);      // variable length arguments too
    writeln("flags4 = ", flags4);                                 // "READ|WRITE|cast(Access)128"
  }
}



////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Demo the AST declaration mechanism.
//
// This demo creates AST nodes for a simple language consisting of expressions having operators (binary and unary) and
// terms (variables and integers). The class hiearchy is:
//
//                                            Ast
//                                             |
//                       +---------------- SgExpression -----------+
//                       |                                         |
//          +------- SgOperator ----------+                 +--- SgTerm ---+
//          |            |                |                 |              |
//      SgBinaryOp   SgUnaryOp!"-"   SgUnaryOp!"+"      SgVariable     SgInteger
//
// For variety, the specific binary operation is stored in an SgBinaryOp data member, while each unary operator is
// its own class via class templates.
//
// Any user can create their own AST nodes.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

import munstead.ast.base;                                         // like C++ #include except faster and more powerful

class SgExpression: Ast {                                         // All AST nodes are ultimately derived from class Ast.
  mixin AstNodeFeatures;                                          // defines certain members in this class
  struct AstChildren {}                                           // no children for SgExpression (but see subclasses)
}

class SgOperator: SgExpression {                                  // another mostly empty class
  mixin AstNodeFeatures;
  struct AstChildren {}
}


class SgBinaryOp: SgOperator {
  mixin AstNodeFeatures;
  struct AstChildren {
    SgExpression lhs, rhs;                                        // data members for AST child nodes; added by magic!
  }

  enum Op { SUM, DIFFERENCE, PRODUCT, QUOTIENT }                  // the different types of binary operators
  Op op;                                                          // a data member, like C++
}


class SgUnaryOp(string op): SgOperator                            // class template with string argument
if (op == "-" || op == "+") {                                     // like Boost's enable_if except MUCH cleaner
  mixin AstNodeFeatures;
  struct AstChildren {
    SgExpression operand;                                         // "operand" is a child in the AST
  }

  this(SgExpression operand) {                                    // define a useful constructor
    this.operand = operand;
  }

  this() {}                                                       // parser class override mechanism needs default c'tors
}
                            

class SgTerm: SgExpression {
  mixin AstNodeFeatures;
  struct AstChildren {}
}


class SgVariable: SgTerm {
  mixin AstNodeFeatures;
  struct AstChildren {}
  string name;                                                    // name of the variable

  this(string name) {                                             // define a useful constructor
    this.name = name;
  }

  this() {}                                                       // parser class override mechanism needs default c'tors
}


class SgInteger: SgTerm {
  mixin AstNodeFeatures;
  struct AstChildren {}
  int value;                                                      // value of the integer literal

  this(int i) {                                                   // define a useful constructor
    value = i;
  }

  this() {}                                                       // parser class override mechanism needs default c'tors
}




////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Demo how to construct AST nodes now that we've created some.
//
// Create an AST for the expression:  -i * 5 + j
//
// All the AST features demoed here are possible because of:
//   1. Introspection, allowing us to find the "AstChildren" declarations
//   2. Mixins, allowing us to define class members at the right levels of the class hierarchy
//   3. Operator overloading of "=" in order to validate and adjust parent pointers
//
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

SgExpression demoAstCreation() {
  // Bear with me... this is a little long because I'm doing things one step at a time. If we had declared proper
  // constructors it could be a single statement (see the comment at the end).

  // We'll create the "-i * 5" from the bottom up...
  auto i = new SgVariable("i");                                   // "auto" is like in C++11
  assert(i.parent is null);                                       // "is" compares pointers, "==" compares objects

  auto neg_i = new SgUnaryOp!"-"(i);                              // "T!x(i)" is like C++ "T<x>(i)"
  assert(i.parent is neg_i);                                      // magic! Parent ptrs updated automatically
  //new SgUnaryOp!"*"(i);                                         // compile error since decl allows only "-" and "+"

  auto five = new SgInteger(5);

  auto product = new SgBinaryOp;                                  // this time we use the default constructor
  product.op = SgBinaryOp.Op.PRODUCT;                             // enum constants are qualified by their type
  product.lhs = neg_i;                                            // no more "set_lhs" and "get_lhs"
  assert(neg_i.parent is product);                                // parent pointers still work!
  //neg_i.parent = null;                                          // setting the parent is illegal (compile error)
  product.rhs = five;

  // Now create the sum from the top down for variety...
  auto sum = new SgBinaryOp;
  sum.op = sum.Op.SUM;                                            // qualification by object works just as well

  auto j = new SgVariable("j");
  sum.lhs = product;
  sum.rhs = j;

  // Enforces tree invariants:
  //   1. Every non-root node has one unique parent, and
  //   2. A depth-first traversal from the root visits each node exactly once
  try {
    sum.lhs = j;                                                  // "j" would be visited twice
    assert(0);                                                    // Dan's favorite, and never compiled out like in C++
  } catch (Exception e) {                                         // ...btw, other "assert" are conditionally compiled
    assert(e.msg == "rhs node is already attached to an AST");    // here, "rhs" means the "j" in "sum.lhs = j"
  }

  assert(sum !is null, "oops");                                   // assert can take a message 2nd argument
  return sum;

  // If we had defined proper constructors, we could have done it like this:
  //
  //return new SgBinaryOp(SgBinaryOp.Op.SUM,
  //                      new SgBinaryOp(SgBinaryOp.Op.PRODUCT,
  //                                     new SgUnaryOp!"-"(new SgVariable("i")),
  //                                     new SgInteger(5)),
  //                      new SgVariable("j"));
}





////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Demo some AST traversals
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Traversal in a loop like what Markus does.
void printIndentedAst(Ast ast) {
  // Do a pre-order traversal and stop at the first integer
  int firstInteger;                                               // initializes to zero
  foreach (node; ast.preOrder) {                                  // empty parens "()" are usually optional
    if (auto i = cast(SgInteger) node) {                          // like C++: dynamic_cast<SgInteger*>(t.node)
      firstInteger = i.value;
      break;
    }
  }
  debug writeln("firstInteger = ", firstInteger);                 // like "#ifndef NDEBUG"

  // Pre-post traversal returns a "visitor" which has some additional information besides the node, such as whether this
  // is the pre-order or post-order visit of the node, and the depth of the node from the starting root,
  writeln("Pre/post traversal shows tree with indentation:");
  foreach (visit; ast.prePostOrder()) {                           // "auto" is implied, "visit" is the loop index
    writefln("  %s%*s%s",                                         // compatible with printf
             visit.order == VisitOrder.PRE ? "--> " : "<-- ",     // "order" is a data member; "PRE" is an enum
             visit.depth * 4, "",                                 // some indentation based on depth from "ast"
             visit.node);                                         // AST node visited; "%s" prints its type name
  }

}

// Find the first occurrence of an SgInteger node and prints its value. This demos a functional style of programming,
// split onto multiple lines just for sake of commenting. The evaluation of the various steps is lazy (i.e., we don't
// traverse a million nodes if we hit an integer near the front).
void printFirstFewIntegers(Ast ast) {
  import std.algorithm;                                           // unlike #include, imports are scoped
  import std.range: drop, take;					  // and can even restrict what is imported
  import munstead.core.util: frontOrElse;

  writeln("variables: ",                                          // prints 'variables: ["i", "j"]'
        ast                                                       // where to start in the tree
        .preOrder                                                 // empty parens are usually optional
        .nodeType!SgVariable                                      // filter to keep only the SgVariable nodes
        .map!"a.name"                                             // string form of lambda; "a" is type SgVariable
        .take(5));                                                // take (up to) five variables for printing

  // functional and iterative approaches can be combined
  size_t count;                                                   // size_t is like in C++, but initializes to zero
  write("variables again: [");
  foreach (variable; ast.preOrder.nodeType!SgVariable) {          // iterate over just the SgVariable nodes
    writef(" \"%s\"", variable.name);                             // try a printf this time
    if (++count >= 5)
      break;
  }
  writeln(" ]");

  // Use Robb's "frontOrElse" if the range might be empty
  int firstInt  = ast.preOrder.nodeType!SgInteger.map!"a.value".frontOrElse(911);
  assert(firstInt == 5);
  int secondInt =  ast.preOrder.nodeType!SgInteger.map!"a.value".drop(1).frontOrElse(911);
  assert(secondInt == 911);
}

// Demonstrate parent traversal.
void demoParentTraversals(SgExpression expr) {
  import std.algorithm;
  import std.range;
  import munstead.core.util: frontOrNull;

  // First, find the "i" variable which is nice and deep in the AST.
  SgVariable i = expr.preOrder.nodeType!SgVariable.filter!"a.name=\"i\"".front;

  // Iteratively print all the ancestors of i, starting with i itself
  writeln("Following parent pointers:");
  foreach (ancestor; i.parents)
    writeln("  ", ancestor);

  // Here's the replacement for ROSE's getEnclosingNode<SgBinaryOp>(expr)
  SgBinaryOp binop = i.parents.nodeType!SgBinaryOp.frontOrNull;
  assert(binop.op == binop.Op.PRODUCT);

  // To skip "i" itself
  SgExpression e = i.parents.drop(1).nodeType!SgExpression.frontOrNull;
  assert(e !is i);

  // To get the great-grandparent, either of these work.
  Ast gp1 = i.parent.parent.parent;
  Ast gp2 = i.parents.drop(2).frontOrNull;			  // drop the parent and the grandparent
  assert(gp1 is gp2);
}
  
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Demo being able to override an AST class used by the parser.
//
// Sometimes users would like to replace some nodes of the AST with their own nodes. Here's how to cause the ELF parser
// to use MySymbolTable in places of ElfSymbolTable, templatized for 32- and 64-bit containers.

import munstead.elf.symbols;					  // unlike #define, imports can be anywhere

class MySymbolTable(size_t nBits): ElfSymbolTable!nBits {
  mixin AstNodeFeatures;
  struct AstChildren {};

  // [your own members here]
}

void demoAstClassOverrides() {
  // Tell the parser that whenever it requests a 32- or 64-bit ELF symbol table object, it should instantiate
  // one of your own objects.
  ElfSymbolTable!32.instance = MySymbolTable!32.instance;
  ElfSymbolTable!64.instance = MySymbolTable!64.instance;

  // It's a compile-time error if you provide an incorrect virtual constructor. In the following example,
  // MySymbolTable!32 doesn't inherit from ElfSymbol!64 and thus cannot be used as the ElfSymbolTable!64 constructor.
  //ElfSymbolTable!64.instance = MySymbolTable!32.instance; // compile-time error

  // Now parse the file like normal
  import munstead.ast.files;
  auto file = AsmFile.parseAny("/bin/cat");
}


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Replacements for Rose::StringUtility
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void demoStrings() {
  import munstead.core.util;

  //----- Replacement for StringUtility::addrToString(5) -----

  string s1 = hexStr(5);
  assert (s1 == "0x00000005");

  string s2 = hexStr(5uL);
  assert(s2 == "0x00000000_00000005");

  string s3 = 5.hexStr;
  assert(s3 == "0x00000005");

  //----- Replacement for StringUtility::cEscape(s) -----

  string badStr = "\aHello, \"world\"\n\0";
  string s4 = badStr.cEscape;
  assert(s4 == "\"\\aHello, \\\"world\\\"\\n\\000\"", s4);	  // note it also adds surrounding quotes
}

  
int main() {
  demoInteresting();
  demoEnums();
  demoBitFlags();
  SgExpression expr = demoAstCreation();
  printIndentedAst(expr);
  printFirstFewIntegers(expr);
  demoParentTraversals(expr);
  demoStrings();
  demoAstClassOverrides();
  return 0;
}
