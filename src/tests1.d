import munstead.ast.base;
import munstead.ast.elf;
import std.stdio;

class A: Ast {
  mixin AstNodeFeatures;
  struct AstChildren {
    Ast a1;
    Ast a2;
  }
}

class B: A {
  mixin AstNodeFeatures;
  struct AstChildren {
    Ast b1;
  }
}

void main() {
  auto a = new A;
  auto b = new B;
  b.b1 = a;

  auto names = b.childNames();
  auto children = b.children();
  assert(names.length == children.length);
  for (size_t i = 0; i < names.length; ++i)
    writeln(names[i], " = ", children[i]);
}
