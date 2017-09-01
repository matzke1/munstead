// Architecture-independent AST nodes
module munstead.ast.files;

import munstead.ast.base;
import munstead.core.byteorder;
import munstead.core.interval;
import munstead.core.mmap;
import munstead.core.wordtypes;

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Top-level structure for a file, from which all other structures are reachable.
class AsmFile: Ast {
  mixin AstNodeFeatures;
  struct AstChildren {}
  
  ByteOrder byteOrder;
}
