module munstead.dos.files;

import munstead.ast.base;
import munstead.ast.files;
import munstead.core.byteorder;
import munstead.core.exception;
import munstead.core.interval;
import munstead.core.mmap;
import munstead.core.util;
import munstead.core.wordtypes;
import std.algorithm: fold, map;
import std.conv: to;

// The nBits defines the address space for the file offsets; we must support various sizes because DOS stubs can appear
// in PE files.
class DosFileHeader(size_t nBits): AsmFile {
  mixin AstNodeFeatures;
  struct AstChildren {}

  struct Disk {
    align(1):
    ubyte[2] e_magic;           // "MZ"
    Word!8 e_last_page_size;    // bytes used on last page of file (1 page == 512 bytes); zero => last page is full
    Word!8 e_total_pages;       // number of pages (inc. last possibly partial page) in file
    Word!8 e_nrelocs;           // number of relocation entries stored after this header
    Word!8 e_header_paragraphs; // header size in paragraphs (16-byte blocks) inc. relocations
    Word!8 e_minalloc;          // number of extra paragraphs needed, similar to BSS in Unix
    Word!8 e_maxalloc;          // max paragraphs to allocate for BSS
    Word!8 e_ss;                // initial value of SS register relative to program load segment
    Word!8 e_sp;                // initial value for SP register
    Word!8 e_cksum;             // checksum; 16-bit sum of all words in file should be zero (usually not filled in)
    Word!8 e_ip;                // initial value for IP register
    Word!8 e_cs;                // initial value for CS register relative to program load segment
    Word!8 e_relocs_offset;     // file address of relocation table
    Word!8 e_overlay;           // overlay number (zero indicates main program)
    Word!8 e_res1;              // reserved (zero)
  }

  Disk disk;
  MemoryMap!(Word!nBits) fileBytes; // data from the file, starting at offset zero
  Interval!(Word!nBits) fileExtent; // part of file which is the header

  // Before parsing, you must assign a map to the "fileBytes" member. This map should contain the file contents to be
  // parsed starting at offset zero.
  void parse(ParseLocation parentLoc) {
    auto loc = fileLocation("", 0, parentLoc);
    parseHeader(loc);
  }

  // Alternative parsing method where the user supplies a file name.
  void parseFile(string fileName) {
    fileBytes = new MemoryMap!(Word!nBits);
    fileBytes.insertFile(fileName, 0);
    parse(null);
  }

  // Test whether the specified named file appears to be a DOS file.  A file is a DOS file if it contains a valid DOS
  // file header, regardless of whether it appears to also be a PE file.
  static bool testFile(string fileName) {
    auto fhdr = DosFileHeader.instance();
    fhdr.fileBytes = new MemoryMap!(Word!nBits);
    fhdr.fileBytes.insertFile(fileName, 0);
    try {
      auto loc = fhdr.fileLocation("", 0, null);
      fhdr.parseHeader(loc);
      return fhdr.preOrder.map!(node => node.errors.length).fold!"a+b"(0uL) == 0;
    } catch {
      return false;
    }
  }

  // Low-level parsing method that parses just the file header without any recursive parsing.
  void parseHeader(ParseLocation ploc) {
    assert(fileBytes !is null);
    auto loc = fileLocation("DOS file header", 0, ploc);

    fileExtent = Interval!(Word!nBits).baseSizeTrunc(0, disk.sizeof);
    if (!fileBytes.readObjectAt(0, disk))
      appendError(loc, "short read");

    if (disk.e_magic[0] != 'M' || disk.e_magic[1] != 'Z')
      appendError(loc, "invalid magic number (" ~ disk.e_magic.to!string ~ ")");

    byteOrder = ByteOrder.LITTLE_ENDIAN;
    disk = disk.fromLittleEndian;
  }

  // File location for error reporting
  ParseLocation fileLocation(string what, Word!nBits offset, ParseLocation parent) {
    auto global = Interval!(Word!nBits).whole;
    return new MapParseLocation!(MemoryMap!(Word!nBits))(what, AddressSpace.FILE, fileBytes, global, offset, "", parent);
  }
}
