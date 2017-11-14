import munstead.ast.base;
import munstead.ast.files;
import munstead.core.exception;
import munstead.core.interval;
import munstead.core.mmap;
import munstead.core.util;
import munstead.core.wordtypes;
import munstead.elf.dynamic;
import munstead.elf.files;
import munstead.elf.interp;
import munstead.elf.reloc;
import munstead.elf.sections;
import munstead.elf.symbols;
import munstead.elf.types;
import std.algorithm: filter, map;
import std.conv: to;
import std.exception;
import std.stdio;
import std.string: leftJustify, rightJustify;

void listElfFile(size_t nBits)(ElfFileHeader!nBits fhdr) {
  listFileInfo(fhdr);
  listElfHeader(fhdr);
  listSectionHeaders(fhdr);
  listSegmentHeaders(fhdr);
  listSectionSegmentMapping(fhdr);
  listRegisteredSections(fhdr);
  listMemoryMap(fhdr);

  if (fhdr.segmentTable !is null) {
    foreach (dyn; fhdr.segmentTable.entries[].map!(a => cast(ElfDynamicSection!nBits)(a.section)).filter!"a !is null")
      listDynamicSection(dyn);
  }

  foreach (rel; fhdr.sections[].map!(a => cast(ElfRelocSection!(nBits, ElfRelocHasAddend.NO)) a).filter!"a !is null")
    listRelocSection(rel);
  foreach (rela; fhdr.sections[].map!(a => cast(ElfRelocSection!(nBits, ElfRelocHasAddend.YES)) a).filter!"a !is null")
    listRelocSection(rela);

  foreach (symtab; fhdr.sections[].map!(section => cast(ElfSymbolTable!nBits) section).filter!"a !is null") {
    listSymbolTable(symtab);
  }
}

void listFileInfo(size_t nBits)(ElfFileHeader!nBits fhdr) {
  assert(fhdr !is null);
  writeln("File:");
  fhdr.fileBytes.dumpMeta("  ");
  writeln();
}

void listElfHeader(size_t nBits)(ElfFileHeader!nBits fhdr) {
  assert(fhdr !is null);
  writeln("ELF Header:");

  write("  Magic:  ");
  foreach (ubyte b; fhdr.fileBytes.segmentsWithin(Interval!(Word!nBits).baseSize(0, 16)).byBuffer(1).map!"a.buffer[0]")
    writef(" %02x", b);
  writeln();

  writeln("  Class:                             ", fhdr.disk.e_ident_file_class);
  writeln("  Data:                              ", fhdr.disk.e_ident_data_encoding);
  writeln("  Version:                           ", fhdr.disk.e_ident_file_version);
  writeln("  OS/ABI:                            ", fhdr.disk.e_ident_osabi);
  writeln("  ABI Version:                       ", fhdr.disk.e_ident_abiversion);
  writeln("  Type:                              ", fhdr.disk.e_type);
  writeln("  Machine:                           ", fhdr.disk.e_machine);
  writeln("  Version:                           ", fhdr.disk.e_version);
  writefln("  Entry pointer address:             0x%0*x", nBits/4, fhdr.disk.e_entry);
  writefln("  Start of program headers:          %d (bytes into file)", fhdr.disk.e_phoff);
  writefln("  Start of section headers:          %d (bytes into file)", fhdr.disk.e_shoff);
  writefln("  Flags:                             %s", fhdr.disk.e_flags);
  writefln("  Size of this header:               %d (bytes)", fhdr.disk.e_ehsize);
  writefln("  Size of program headers:           %d (bytes)", fhdr.disk.e_phentsize);
  writefln("  Number of program headers:         %d", fhdr.segmentTable.entries.length);
  writefln("  Size of section headers:           %d (bytes", fhdr.disk.e_shentsize);
  writefln("  Number of section headers:         %d", fhdr.sectionTable.entries.length);
  writeln();
}

void listSectionHeaders(size_t nBits)(ElfFileHeader!nBits fhdr) {
  assert(fhdr !is null);
  
  writeln("Section Headers:");
  if (fhdr.sectionTable.entries.length == 0) {
    writeln("  There are no section headers in this file.");
    return;
  }

  const size_t addrWidth = Word!nBits.min.hexStr.length;
  writeln("   Idx  ", leftJustify("Name/comment", 5+2*addrWidth),
          " Type");
  writeln("            ", leftJustify("Offset", addrWidth),
          " ", leftJustify("Address", addrWidth),
          " Size");
  writeln("            ", leftJustify("EntSize", addrWidth),
          " ", leftJustify("Alignment", addrWidth),
          " Flags Link Info");
  writeln("  ----- ----", leftJustify("", addrWidth, '-'),
          " ", leftJustify("", addrWidth, '-'),
          " ---------------");
  for (size_t i=0; i<fhdr.sectionTable.entries.length; ++i) {
    auto shent = fhdr.sectionTable.entries[i];
    writef("  [%3d]", i);
    writeln(" ", leftJustify(shent.printableName, 5+2*addrWidth), " ", shent.disk.sh_type);
    writeln("            ", shent.disk.sh_offset.hexStr, " ", shent.disk.sh_addr.hexStr, " ", shent.disk.sh_size.hexStr);
    writeln("            ", shent.disk.sh_entsize.hexStr, " ", shent.disk.sh_addralign.hexStr, " ", shent.disk.sh_flags,
            " ", shent.disk.sh_link, " ", shent.disk.sh_info);
  }
  writeln();
}

void listSegmentHeaders(size_t nBits)(ElfFileHeader!nBits fhdr) {
  assert(fhdr !is null);
  writeln("Program Headers:");
  if (fhdr.segmentTable.entries.length == 0) {
    writeln("  There are no program headers in this file.");
    return;
  }

  const size_t addrWidth = Word!nBits.min.hexStr.length;
  writeln("   Idx  ", leftJustify("Type", 15),
          " ", leftJustify("Offset", addrWidth),
          " ", leftJustify("VirtAddr", addrWidth),
          " PhysAddr");
  writeln("        ", leftJustify("", 15),
          " ", leftJustify("FileSize", addrWidth),
          " ", leftJustify("VirtSize", addrWidth),
          " Flags Alignment");
  writeln("  ----- ", leftJustify("", 15, '-'),
          " ", leftJustify("", addrWidth, '-'),
          " ", leftJustify("", addrWidth, '-'),
          " ---------------");

  for (size_t i=0; i<fhdr.segmentTable.entries.length; ++i) {
    auto phent = fhdr.segmentTable.entries[i];
    writefln("  [%3d] %-15s %s %s %s",
             i, phent.disk.p_type, phent.disk.p_offset.hexStr, phent.disk.p_vaddr.hexStr, phent.disk.p_paddr.hexStr);
    writefln("        %-15s %s %s %-9s 0x%x", 
             "", phent.disk.p_filesz.hexStr, phent.disk.p_memsz.hexStr, phent.disk.p_flags, phent.disk.p_align);

    if (auto interp = cast(ElfInterpSection!nBits) phent.section)
      writefln("           [Requesting program interpreter: %s]", interp.interpreter);
  }
  writeln();
}

void listSectionSegmentMapping(size_t nBits)(ElfFileHeader!nBits fhdr) {
  assert(fhdr !is null);
  if (fhdr.segmentTable.entries.length > 0 && fhdr.sectionTable.entries.length >0) {
    writeln("Section to Segment Mapping:");

    writeln("  ", leftJustify("Name/comment", 40),
            " ", leftJustify("Type", 19),
            " Object type");
    writeln("  ", leftJustify("", 40, '-'),
            " ", leftJustify("", 19, '-'),
            " ----------------------------------------");

    foreach (phent; fhdr.segmentTable.entries[]) {
      writefln("  %-40s %-19s %s", phent.printableName, phent.disk.p_type, phent.section);

      foreach (shent; fhdr.sectionsWithinSegment(phent))
        writefln("    %-41s %-16s %s", shent.printableName, shent.disk.sh_type, shent.section);
    }
    writeln();
  }
}

void listRegisteredSections(size_t nBits)(ElfFileHeader!nBits fhdr) {
  assert(fhdr !is null);
  enum addrWidth = Word!nBits.min.hexStr.length, sizeWidth = size_t.min.hexStr.length;

  writeln("Registered sections:");
  writeln("  ", leftJustify("Name/comment", 51), " Object type");
  writeln("    ", leftJustify("Offset", addrWidth),
          "   ", leftJustify("FileSize", sizeWidth),
          "   ", leftJustify("FileEnd+1", addrWidth));
  writeln("    ", leftJustify("Address", addrWidth),
          "   ", leftJustify("MemSize", sizeWidth),
          "   ", leftJustify("AddrEnd+1", addrWidth));
  writeln("  --", leftJustify("", addrWidth, '-'),
          "   ", leftJustify("", sizeWidth, '-'),
          "   ", leftJustify("", addrWidth, '-'));

  if (fhdr.sections.length == 0) {
    writeln("  No registered sections.");
  } else {
    foreach (section; fhdr.sections[]) {
      writeln("  ", leftJustify(section.printableName, 51), " ", section);

      if (section.fileExtent.empty) {
        writeln("    empty");
      } else {
        writeln("    ", section.fileExtent.least.hexStr,
                " + ", section.fileExtent.length.hexStr,
                " = ", (cast(Word!nBits) section.fileExtent.greatest+1).hexStr);
      }

      if (section.preferredExtent.empty) {
        writeln("    empty");
      } else {
        writeln("    ", section.preferredExtent.least.hexStr,
                " + ", section.preferredExtent.length.hexStr,
                " = ", (cast(Word!nBits) section.preferredExtent.greatest+1).hexStr);
      }
    }
  }
  writeln();
}

void listMemoryMap(size_t nBits)(ElfFileHeader!nBits fhdr) {
  assert(fhdr !is null);
  writeln("Virtual memory map:");
  if (fhdr.memBytes is null) {
    writeln("  Map does not exist.");
  } else if (fhdr.memBytes.empty) {
    writeln("  Map is empty.");
  } else {
    fhdr.memBytes.dumpMeta("  ");
  }
  writeln();
}

void listDynamicSection(size_t nBits)(ElfDynamicSection!nBits dyn) {
  assert(dyn !is null);
  writefln("Dynamic section at offset %s contains %d entries:", dyn.fileExtent.least.hexStr, dyn.entries.length);
  size_t tagWidth = Elf!nBits.Sxword.min.hexStr.length;
  size_t valWidth = Elf!nBits.Xword.min.hexStr.length;
  writeln("  Idx",
          " ", leftJustify("Tag", tagWidth),
          " ", leftJustify("Type", 24),
          " ", leftJustify("Name/Value", valWidth),
          " Detail");
  writeln("  ---",
          " ", leftJustify("", tagWidth, '-'),
          " ", leftJustify("", 24, '-'),
          " ", leftJustify("", valWidth, '-'),
          " ----------------------------------------");

  static void printSection(ElfDynamicEntry!nBits entry, ElfSection!nBits section) {
    if (section !is null && section.preferredExtent.least == entry.disk.d_val) {
      writeln(" ", section.printableName);
    } else if (section is null) {
      writeln(" no corresponding section");
    } else {
      writeln(" duplicate entry");
    }
  }

  for (size_t i = 0; i < dyn.entries.length; ++i) {
    auto entry = dyn.entries[i];

    writef("  %-3d %s %-24s", i, entry.disk.d_tag.hexStr, "(" ~ to!string(entry.disk.d_tag) ~ ")");
    if (entry.isPointer()) {
      write(" ", entry.disk.d_val.hexStr);
    } else {
      writef(" %-*d", valWidth, entry.disk.d_val);
    }

    with (entry.Tag) {
      switch (entry.disk.d_tag) {
      case DT_STRTAB:
        printSection(entry, dyn.strtab);
        break;

      case DT_SYMTAB:
        printSection(entry, dyn.symtab);
        break;

      case DT_REL:
        printSection(entry, dyn.rel);
        break;

      case DT_RELA:
        printSection(entry, dyn.rela);
        break;

      case DT_NEEDED:
        writeln(" ", entry.detail.needed.cEscape);
        break;

      case DT_RPATH:
        writeln(" ", entry.detail.rpath.cEscape);
      break;

      case DT_RUNPATH:
        writeln(" ", entry.detail.runpath.cEscape);
        break;

      case DT_SONAME:
        writeln(" ", entry.detail.soname.cEscape);
        break;

      case DT_FLAGS:
        writeln(" ", entry.detail.flags);
        break;

      case DT_FLAGS_1:
        writeln(" ", entry.detail.flags1);
        break;

      case DT_POSFLAG_1:
        writeln(" ", entry.detail.posflag1);
        break;

      case DT_INIT_ARRAY:
        if (dyn.initArray !is null) {
          for (size_t j = 0; j < dyn.initArray.values.length; ++j)
            write(0==j ? " " : ", ", dyn.initArray.values[j].hexStr);
        }
        writeln();
        break;

      case DT_FINI_ARRAY:
        if (dyn.finiArray !is null) {
          for (size_t j = 0; j < dyn.finiArray.values.length; ++j)
            write(0==j ? " " : ", ", dyn.finiArray.values[j].hexStr);
        }
        writeln();
        break;
      
      default:
        writeln();
      }
    }
  }
  writeln();
}

void listRelocSection(size_t nBits, ElfRelocHasAddend hasAddend)(ElfRelocSection!(nBits, hasAddend) reloc) {
  assert(reloc !is null);
  write("Relocation ", reloc.printableName);
  if (!reloc.fileExtent.empty)
    write(" at offset ", reloc.fileExtent.least.hexStr);
  if (!reloc.preferredExtent.empty)
    write(" at address ", reloc.preferredExtent.least.hexStr);
  writeln(" contains ", reloc.entries.length, " entries:");

  enum addrWidth = Word!nBits.min.hexStr.length;
  write("  ", leftJustify("Offset", addrWidth),
        " ", leftJustify("Info", addrWidth),
        " ", leftJustify("Type", 24),
        " ", leftJustify("Sym Value", addrWidth));
  static if (hasAddend == ElfRelocHasAddend.YES)
    write(" ", leftJustify("Addend", addrWidth));
  writeln();

  write("  ", leftJustify("", addrWidth, '-'),
        " ", leftJustify("", addrWidth, '-'),
        " ", leftJustify("", 24, '-'),
        " ", leftJustify("", addrWidth, '-'));
  static if (hasAddend == ElfRelocHasAddend.YES)
    write(" ", leftJustify("", addrWidth, '-'));
  writeln();
  
  foreach (entry; reloc.entries[]) {
    auto symbol = entry.symbol;
    Word!nBits symbolValue = symbol is null ? 0 : symbol.disk.st_value;
    
    write("  ", entry.disk.r_offset.hexStr,
          " ", entry.disk.r_info.hexStr,
          " ", leftJustify(entry.typeName, 24),
          " ", symbolValue.hexStr);
    static if (hasAddend == ElfRelocHasAddend.YES)
      write(" ", entry.disk.r_addend.hexStr);

    if (symbol is null) {
      write(" ERROR: no symbol");
    } else {
      write(" ", symbol.name);
    }

    writeln();
  }
  writeln();
}

void listSymbolTable(size_t nBits)(ElfSymbolTable!nBits symtab) {
  assert(symtab !is null);
  enum addrWidth = Word!nBits.min.hexStr.length;
  size_t indexWidth = symtab.symbols.length.to!string.length;
  writeln("Symbol table ", symtab.printableName, " contains ", symtab.symbols.length, " entries:");

  writeln("  ", leftJustify("Num", indexWidth),
          " ", leftJustify("Value", addrWidth),
          " ", leftJustify("Size", addrWidth),
          " ", leftJustify("Type", 11),
          " ", leftJustify("Binding", 10),
          " ", leftJustify("Visibility", 13),
          " ", leftJustify("Index", 24),
          " Name");
  writeln("  ", leftJustify("", indexWidth, '-'),
          " ", leftJustify("", addrWidth, '-'),
          " ", leftJustify("", addrWidth, '-'),
          " ", leftJustify("", 11, '-'),
          " ", leftJustify("", 10, '-'),
          " ", leftJustify("", 13, '-'),
          " ", leftJustify("", 24, '-'),
          " --------------------");

  for (size_t symbolIdx = 0; symbolIdx < symtab.symbols.length; ++symbolIdx) {
    auto symbol = symtab.symbols[symbolIdx];
    writef("  %*d %s %s %-11s %-10s %-13s",
           indexWidth, symbolIdx,
           symbol.disk.st_value.hexStr,
           symbol.disk.st_size.hexStr,
           symbol.type,
           symbol.binding,
           symbol.visibility);

    if (ElfSection!nBits linked = symbol.linkedSection) {
      string s = symbol.linkedSectionIndex.to!string ~ " " ~ linked.name.cEscape;
      if (s.length > 24)
        s = s[0..21] ~ "...";
      writef(" %-24s", s);
    } else {
      writef(" %-24s", cast(ElfSectionTable!nBits.SectionIndex) symbol.disk.st_shndx);
    }

    writeln(" ", symbol.name.cEscape);
  }

  writeln();
}

void main(string[] argv) {
  enforce(argv.length == 2);

  AsmFile file = AsmFile.parseGiven!(ElfFileHeader!32, ElfFileHeader!64)(argv[1]);
  enforce(file !is null, "could not parse " ~ argv[1].cEscape);
  
  if (auto fhdr32 = cast(ElfFileHeader!32) file) {
    listElfFile(fhdr32);
  } else if (auto fhdr64 = cast(ElfFileHeader!64) file) {
    listElfFile(fhdr64);
  } else {
    writeln("unrecognized file format");
  }
}
