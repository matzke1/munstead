import munstead.core.mmap;
import munstead.ast.base;
import munstead.elf.files;
import munstead.elf.sections;
import munstead.elf.segments;
import munstead.ast.tabulate;
import std.algorithm;
import std.array;
import std.exception;
import std.range;
import std.stdio;

// Given an AST, do some simple traversals and print node types
void simpleTraversals(Ast ast) {
  // preOrder returns an iterator range which visits the nodes of the AST in pre-order.
  // write(node) prints the node dynamic type name.
  writeln("Pre-order traversal node types:");
  ast.preOrder()
    .each!(node => writeln("  ", node));

  writeln("Another way to do the same thing:");
  foreach (node; ast.preOrder)
    writeln("  ", node);

  writeln("Get a list of all the nodes first:");
  Ast[] nodes = ast.preOrder.array;
  foreach (node; nodes)
    writeln("  ", node);

  // preOrder and postOrder are not bidirectional, thus "retro" needs an "array" capture first.
  writeln("Print the reverse-prefix-dfs traversal:");
  foreach (node; ast.preOrder.array.retro)
    writeln("  ", node);

  // Pre-order traversal that prints the path from the current node to the root of the entire tree.
  writeln("Path to root for each node:");
  foreach (node; ast.preOrder) {
    foreach (parent; node.parents)
      write(" ", parent);
    writeln();
  }
}

// Pre-post traversal. Each node is visited twice: once in PRE order and once in POST order, and all its children are
// visited between the pre and post.
void prePostTraversal(Ast ast) {
  writeln("Pre-post traversal:");
  foreach (visit; ast.prePostOrder) {
    writefln("  %s%*s %s",  VisitOrder.PRE==visit.order?"enter> ":"leave< ", 4*visit.depth, "", visit.node);

    // if you want the path try this, which returns the path from the root node down to the current node. The
    // result is an iterator range which you could turn into an Ast[] by appending ".array" to the end. The
    // "array" before "retro" is because "parents" is not a bidirectional range and therefore its results need
    // to be captured before they can be reversed.
    auto fromRoot = visit.node.parents.array.retro;

    // if you want the path from the start of the traversal to the current node, do this instead. The
    // No.openRight is a flag that says the interval is closed on the right, i.e., the "ast" node is included
    // in the interval.
    auto fromAst = visit.node.parents.until!(node => node is ast)(No.openRight).array.retro;

    // Get the closest parent that's the specified type or a subclass thereof, or null. I could have used
    // "auto" on the left of course, but wanted to show/check the actual return type.
    ElfFileHeader!64 p = visit.node.ancestor!(ElfFileHeader!64);
  }
}

// Nodes also have the ability to print themselves recursively in a tabular arrangement.
void printAst(Ast ast) {
  writeln("Print the entire ast:");
  ast.tabulate("  ast");

  writeln("Print the on-disk format of 64-bit section table entries:");
  foreach (node; ast.preOrder.nodeType!(ElfSectionTableEntry!64)) {
    writeln();
    auto fmt = TabulateFormat("  section");
    fmt.skipChildren = true; // because our traversal will show each one
    node.disk.tabulate(fmt);
  }
}

void printNodes(NodeType)(Ast ast, string prefix = "") {
  foreach (node; ast.preOrder.nodeType!NodeType) {
    node.tabulate(TabulateFormat(prefix));
    writeln();
  }
}

// Print all entries of the section table
void printSectionTable(Ast ast) {
  writeln("Section table:");
  printNodes!(ElfSectionTableEntry!64)(ast, "  section");
  printNodes!(ElfSectionTableEntry!32)(ast, "  section");
}

// Print all entries of the segment table
void printSegmentTable(Ast ast) {
  writeln("Segment table:");
  printNodes!(ElfSegmentTableEntry!64)(ast, "  segment");
  printNodes!(ElfSegmentTableEntry!32)(ast, "  segment");
}

// Print certain info from the segment table
void printSegmentInfo(ElfHeader)(ElfHeader fhdr) {
  writeln("Segment table:");
  foreach (phent; fhdr.segmentTable.entries[]) {
    writefln("  Segment %-3d %-16s %s", phent.index, phent.disk.p_type, phent.disk.p_flags);
    if (phent.section !is null)
      writefln("    %s", typeid(phent.section));
    writefln("    file 0x%08x + 0x%08x", phent.disk.p_offset, phent.disk.p_filesz);
    writefln("    addr 0x%08x + 0x%08x align 0x%08x", phent.disk.p_vaddr, phent.disk.p_memsz, phent.disk.p_align);
    foreach (shent; fhdr.sectionsWithinSegment(phent)) {
      writefln("    section %-32s %-16s %s", shent.name, shent.disk.sh_type, shent.disk.sh_flags);
      if (shent.section !is null)
	writefln("      %s", typeid(shent.section));
      if (!shent.fileExtent.empty)
	writefln("      file 0x%08x + 0x%08x", shent.fileExtent.least, shent.fileExtent.length);
      if (!shent.preferredExtent.empty)
	writefln("      addr 0x%08x + 0x%08x", shent.preferredExtent.least, shent.preferredExtent.length);
    }
  }
}
   
void main(string[] argv) {
  enforce(argv.length == 2);
  auto ast = ElfFile.parse(argv[1]);

  ast.tabulate(TabulateFormat("fhdr"));

  //simpleTraversals(ast);
  //prePostTraversal(ast);
  //printAst(ast);

  static if (0) {
    if (auto fhdr = cast(ElfFileHeader!32) ast) {
      printSegmentInfo(fhdr);
    } else if (auto fhdr = cast(ElfFileHeader!64) ast) {
      printSegmentInfo(fhdr);
    }
  }

  //printSectionTable(ast);
  //printSegmentTable(ast);
  dumpAst(ast);
}
  
