/** Architecture-independent AST nodes for entire files. */
module munstead.ast.files;

import munstead.ast.base;
import munstead.core.byteorder;
import munstead.core.exception;
import munstead.core.wordtypes;
import munstead.elf.files;
import munstead.pe.files;

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// AsmFile
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/** Top-level, architecture-independent structure for a specimen.
 *
 *  This node type serves as the top-level node for a specimen, from which all other syntactic structures are reachable. */
class AsmFile: Ast {
  mixin AstNodeFeatures;
  struct AstChildren {}

  /** Order of multi-byte structures. */
  ByteOrder byteOrder;

  /** Check whether file is a particular type.
   *
   *  Returns true if the named file can be opened and appears to satisfy the cursory requirements for being a
   *  particular type of file.  The requirement is usually that some form of file header can be parsed without
   *  encountering an error. This could be as simple as whether the file has a valid "magic number" for the
   *  specified file type. */
  static bool isFile(Header)(string fileName) {
    return Header.testFile(fileName);
  }

  /** Parse the file as one of a choice of formats.
   *
   *  Each of the specified formats is tried in turn to see if it passes the `isFile` test, and if so that
   *  parser is used to parse the named file. If errors occur during parsing, then either a valid AST node is
   *  returned and the errors are recorded in the AST, or an exception is thrown. The specific action depends on
   *  how many errors were encountered and whether it exceeded the library-wide error limit as contained in the
   *  `munstead.ast.base.Ast.totalErrorLimit` variable.  If none of the specified parsers accept responsibility
   *  for the file (i.e., `isFile` always returned false) then this method returns null. */
  static AsmFile parseGiven(Header : AsmFile, Others...)(string fileName) {
    if (isFile!Header(fileName)) {
      auto fhdr = Header.instance();
      try {
        fhdr.parseFile(fileName);
      } catch (ParseError) {
      }
      return fhdr;
    } else static if (Others.length) {
      return parseGiven!(Others)(fileName);
    } else {
      return null;
    }
  }

  /** Parse the named file if possible.
   *
   *  This method tries to parse the named file using all parsers known to the library. It is the same as calling
   *  `parseGiven` with a list of all parser types. */
  static AsmFile parseAny(string fileName) {
    return parseGiven!(ElfFileHeader!64, ElfFileHeader!32, PeFileHeader!32, PeFileHeader!64)(fileName);
  }
}
