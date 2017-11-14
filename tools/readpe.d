import munstead.ast.base;
import munstead.ast.files;
import munstead.core.exception;
import munstead.core.hexdump;
import munstead.core.util;
import munstead.core.wordtypes;
import munstead.dos.files;
import munstead.pe.exports;
import munstead.pe.imports;
import munstead.pe.files;
import munstead.pe.resources;
import std.algorithm: filter, fold, map, max;
import std.conv: to;
import std.exception;
import std.range: enumerate;
import std.stdio;
import std.string: leftJustify;

HexDumpFormat hdFormat = {prefix:"    "};

void listPeFile(size_t nBits)(PeFileHeader!nBits fhdr) {
  listFileInfo(fhdr);
  listAllErrors(fhdr);
  listDosHeader(fhdr.dosHeader);
  listCoffHeader(fhdr);
  listOptionalHeader(fhdr);
  listDataDirectory(fhdr);
  listSectionTable(fhdr);
  listMemoryMap(fhdr);
  listRegisteredSections(fhdr);
  listExportSections(fhdr);
  listImportSections(fhdr);
  listResourceSections(fhdr);
}

void listFileInfo(size_t nBits)(PeFileHeader!nBits fhdr) {
  assert(fhdr !is null);
  writeln("File:");
  fhdr.fileBytes.dumpMeta("  ");
  writeln();
}

void listAllErrors(size_t nBits)(PeFileHeader!nBits fhdr) {
  size_t nErrors = fhdr.preOrder.map!(node => node.errors.length).fold!"a+b"(0uL);
  if (nErrors > 0) {
    writeln("Errors detected during parsing (", nErrors.plural("errors"), " in total):");
    foreach (i, node; fhdr.preOrder.filter!(node => node.errors.length > 0).enumerate) {
      listErrors(node);
      writeln();
    }
  }
}

void listErrors(Node)(Node node, string prefix = "  ")
if (is(Node : Ast)) {
  if (node.errors.length > 0) {
    writeln(prefix, node.errors.length.plural("errors"), " detected when parsing this ", typeid(node), " node");
    foreach (error; node.errors) {
      string s;
      if (auto parseError = cast(ParseError) error) {
        s = parseError.to!string;
      } else {
        s = error.to!string;
      }
      import std.string: lineSplitter;
      foreach (line; s.lineSplitter)
        writeln(prefix, "  ", line);
    }
  }
}

void listDosHeader(size_t nBits)(DosFileHeader!nBits fhdr) {
  writeln("DOS file header at file offset 0:");
  listErrors(fhdr);

  enum fieldWidth = 23;
  writeln("  ", leftJustify("Field", fieldWidth),
          " Value");
  writeln("  ", leftJustify("", fieldWidth, '-'),
          " ---------------------------------");

  writefln("  %-*s %d (bytes)", fieldWidth, "last page size", fhdr.disk.e_last_page_size);
  writefln("  %-*s %d", fieldWidth, "total pages", fhdr.disk.e_total_pages);
  writefln("  %-*s %d", fieldWidth, "number of relocations", fhdr.disk.e_nrelocs);
  writefln("  %-*s %d (16-byte units)", fieldWidth, "header size", fhdr.disk.e_header_paragraphs);
  writefln("  %-*s %d (16-byte units)", fieldWidth, "minalloc", fhdr.disk.e_minalloc);
  writefln("  %-*s %d (16-byte units)", fieldWidth, "maxalloc", fhdr.disk.e_maxalloc);
  writefln("  %-*s %s", fieldWidth, "initial SS register", fhdr.disk.e_ss.hexStr);
  writefln("  %-*s %s", fieldWidth, "initial SP register", fhdr.disk.e_sp.hexStr);
  writefln("  %-*s %s", fieldWidth, "checksum", fhdr.disk.e_cksum.hexStr);
  writefln("  %-*s %s", fieldWidth, "initial IP register", fhdr.disk.e_ip.hexStr);
  writefln("  %-*s %s", fieldWidth, "initial CS register", fhdr.disk.e_cs.hexStr);
  writefln("  %-*s %s", fieldWidth, "relocation table offset", fhdr.disk.e_relocs_offset);
  writefln("  %-*s %d", fieldWidth, "overlay number", fhdr.disk.e_overlay);
  writeln("  file data:");
  hexdump(fhdr.fileBytes, fhdr.fileExtent, hdFormat);
  writeln();
}

void listCoffHeader(size_t nBits)(PeFileHeader!nBits fhdr) {
  if (fhdr.coffHeader is null) {
    writeln("COFF file header:\n  No COFF file header parsed.");
  } else {
    writeln("COFF file header at file offset ", fhdr.coffHeader.fileExtent.hexStr, ":");
    listErrors(fhdr.coffHeader);

    enum fieldWidth = 23;
    writeln("  ", leftJustify("Field", fieldWidth),
            " Value");
    writeln("  ", leftJustify("", fieldWidth, '-'),
            " ---------------------------------");

    with (fhdr.coffHeader) {
      writefln("  %-*s %s", fieldWidth, "machine", disk.machine);
      writefln("  %-*s %d", fieldWidth, "number of sections", disk.numberOfSections);
      writefln("  %-*s %d %s", fieldWidth, "time stamp", disk.timeDateStamp, timeStamp.toSimpleString);
      writefln("  %-*s %s", fieldWidth, "symbol table offset", disk.pointerToSymbolTable);
      writefln("  %-*s %d", fieldWidth, "number of symbols", disk.numberOfSymbols);
      writefln("  %-*s %d (bytes)", fieldWidth, "size of optional header", disk.sizeOfOptionalHeader);
      writefln("  %-*s %s", fieldWidth, "characteristics", disk.characteristics);
      writeln("  file data:");
      hexdump(fhdr.fileBytes, fileExtent, hdFormat);
    }
  }
  writeln();
}

void listOptionalHeader(size_t nBits)(PeFileHeader!nBits fhdr) {
  if (fhdr.optionalHeader is null) {
    writeln("Optional file header:\n  No optional file header parsed.");
  } else {
    writeln("Optional file header at file offset ", fhdr.optionalHeader.fileExtent.hexStr, ":");
    listErrors(fhdr.optionalHeader);

    enum fieldWidth = 23;
    writeln("  ", leftJustify("Field", fieldWidth),
            " Value");
    writeln("  ", leftJustify("", fieldWidth, '-'),
            " ---------------------------------");

    with (fhdr.optionalHeader.disk) {
      writefln("  %-*s %s", fieldWidth, "magic", magic.hexStr);
      writefln("  %-*s %d.%d", fieldWidth, "linker version", majorLinkerVersion, minorLinkerVersion);
      writefln("  %-*s %s", fieldWidth, "code size", sizeOfCode.hexStr);
      writefln("  %-*s %s", fieldWidth, "initialized data size", sizeOfInitializedData.hexStr);
      writefln("  %-*s %s", fieldWidth, "uninitialized data size", sizeOfUninitializeData.hexStr);
      writefln("  %-*s %s", fieldWidth, "entry point RVA", entryPointRva.hexStr);
      writefln("  %-*s %s", fieldWidth, "code RVA", baseOfCode.hexStr);
      static if (32 == nBits)
        writefln("  %-*s %s", fieldWidth, "data RVA", baseOfData.hexStr);
      writefln("  %-*s %s", fieldWidth, "image base", imageBase.hexStr);
      writefln("  %-*s %s", fieldWidth, "section alignment", sectionAlignment.hexStr);
      writefln("  %-*s %s", fieldWidth, "file alignment", fileAlignment.hexStr);
      writefln("  %-*s %d.%d", fieldWidth, "OS version", majorOsVersion, minorOsVersion);
      writefln("  %-*s %d.%d", fieldWidth, "image version", majorImageVersion, minorImageVersion);
      writefln("  %-*s %d.%d", fieldWidth, "subsystem version", majorSubsystemVersion, minorSubsystemVersion);
      writefln("  %-*s %d", fieldWidth, "win32 version", win32VersionNumber);
      writefln("  %-*s %s", fieldWidth, "size of image", sizeOfImage.hexStr);
      writefln("  %-*s %s", fieldWidth, "size of headers", sizeOfHeaders.hexStr);
      writefln("  %-*s %s", fieldWidth, "checksum", checkSum.hexStr);
      writefln("  %-*s %s", fieldWidth, "subsystem", subsystem);
      writefln("  %-*s %s", fieldWidth, "DLL characteristics", dllCharacteristics);
      writefln("  %-*s %s", fieldWidth, "size of stack, reserve", sizeOfStackReserve.hexStr);
      writefln("  %-*s %s", fieldWidth, "size of stack, commit", sizeOfStackCommit.hexStr);
      writefln("  %-*s %s", fieldWidth, "size of heap, reserve", sizeOfHeapReserve.hexStr);
      writefln("  %-*s %s", fieldWidth, "size of heap, commit", sizeOfHeapCommit.hexStr);
      writefln("  %-*s %s", fieldWidth, "loader flags", loaderFlags);
      writefln("  %-*s %d", fieldWidth, "data dir entries", numberOfRvaAndSizes);
    }
    writeln("  file data:");
    hexdump(fhdr.fileBytes, fhdr.optionalHeader.fileExtent, hdFormat);
  }
  writeln();
}

void listDataDirectory(size_t nBits)(PeFileHeader!nBits fhdr) {
  if (fhdr.dataDirectory is null) {
    writeln("Optional header data directory:\n  No optional header data directory parsed.");
  } else {
    writeln("Optional header data directory at file offset ", fhdr.dataDirectory.fileExtent.hexStr, ":");
    listErrors(fhdr.dataDirectory);

    enum addrWidth = Word!nBits.min.hexStr.length;
    writeln("  Index",
            " ", leftJustify("Type", 23),
            " ", leftJustify("RVA", addrWidth),
            "   ", leftJustify("Size", addrWidth),
            "   ", leftJustify("RvaEnd", addrWidth));
    writeln("       ",
            " ", leftJustify("", 23),
            " ", leftJustify("VA", addrWidth),
            "   ", leftJustify("", addrWidth),
            "   ", leftJustify("VaEnd", addrWidth));
    writeln("   --- ",
            " ", leftJustify("", 23, '-'),
            " ", leftJustify("", addrWidth, '-'),
            "   ", leftJustify("", addrWidth, '-'),
            "   ", leftJustify("", addrWidth, '-'));

    foreach (ddent; fhdr.dataDirectory.entries[]) {
      writefln("  [%3d] %-23s %-*s + %-*s = %s",
               ddent.index,
	       cast(PeDataDirectory!nBits.DataDirType)ddent.index,
               addrWidth, ddent.disk.rva.hexStr,
	       addrWidth, ddent.disk.nBytes.hexStr,
               (ddent.disk.rva + ddent.disk.nBytes).hexStr);
      writefln("   %3s  %-23s %s   %*s   %s", "", "",
               fhdr.va(ddent.disk.rva).hexStr,
               addrWidth, "",
               fhdr.va(ddent.disk.rva + ddent.disk.nBytes).hexStr);
      listErrors(ddent, "    ");
    }
    writeln("  file data:");
    hexdump(fhdr.fileBytes, fhdr.dataDirectory.fileExtent, hdFormat);
  }
  writeln();
}

void listSectionTable(size_t nBits)(PeFileHeader!nBits fhdr) {
  if (fhdr.sectionTable is null || fhdr.sectionTable.entries.length == 0) {
    writeln("Section table:");
    writeln("  Not present.");
    return;
  }

  writeln("Section table at file offset ", fhdr.sectionTable.fileExtent.hexStr, ":");
  listErrors(fhdr.sectionTable);

  enum addrWidth = Word!32.min.hexStr.length;
  writeln("  Index",
          " ", leftJustify("Name", 8),
          " Characteristics");
  writeln("            ", leftJustify("Offset", addrWidth),
          " ", leftJustify("RVA", addrWidth),
          " ", leftJustify("RelocOff", addrWidth),
          " ", leftJustify("LnumOff", addrWidth));
  writeln("            ", leftJustify("FileSize", addrWidth),
          " ", leftJustify("MemSize", addrWidth),
          " ", leftJustify("RelocCnt", addrWidth),
          " ", leftJustify("LnumCnt", addrWidth));
  writeln("  ----- ----", leftJustify("", addrWidth, '-'),
          " ", leftJustify("", addrWidth, '-'),
          " ", leftJustify("", addrWidth, '-'),
          " ", leftJustify("", addrWidth, '-'));

  foreach (shent; fhdr.sectionTable.entries[]) {
    writefln("  [%3d] %s %s", shent.index, shent.name.cEscape, shent.disk.characteristics);
    writefln("            %-*s %-*s %-*s %-*s",
             addrWidth, shent.disk.pointerToRawData.hexStr,
             addrWidth, shent.disk.rva.hexStr,
             addrWidth, shent.disk.pointerToRelocations.hexStr,
             addrWidth, shent.disk.pointerToLineNumbers.hexStr);
    writefln("            %-*s %-*s %-*d %-*d",
             addrWidth, shent.disk.sizeOfRawData.hexStr,
             addrWidth, shent.disk.virtualSize.hexStr,
             addrWidth, shent.disk.numberOfRelocations,
             addrWidth, shent.disk.numberOfLineNumbers);
    listErrors(shent, "            ");
  }
  writeln("  file data:");
  hexdump(fhdr.fileBytes, fhdr.sectionTable.fileExtent, hdFormat);
  writeln();
}

void listRegisteredSections(size_t nBits)(PeFileHeader!nBits fhdr) {
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

      listErrors(section, "    ");
    }
  }
  writeln();
}

void listMemoryMap(size_t nBits)(PeFileHeader!nBits fhdr) {
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

void listExportSections(size_t nBits)(PeFileHeader!nBits fhdr) {
  enum addrWidth = Word!nBits.min.hexStr.length;
  size_t nExportSections;
  foreach (section; fhdr.sections[].map!(a => cast(PeExportSection!nBits) a).filter!"a !is null") {
    ++nExportSections;
    writeln("Export section at va ", section.preferredExtent.hexStr, ":");
    listErrors(section);

    if (section.directory is null) {
      write("  No export directory parsed.\n\n");
      continue;
    }

    with (section.directory) {
      writeln("  ", fhdr.va(disk.nameRva).hexStr, " ", dllName.cEscape);

      writeln("  ", timeStamp.toSimpleString,
              ", version ", disk.minorVersion, ".", disk.majorVersion);
      writeln("  ", disk.nAddressTableEntries.plural("address table entries"));
      writeln("  ", disk.nNamePointers.plural("name/ordinal table entries"),
              " beginning with ordinal #", disk.ordinalBase);
      listErrors(section.directory);

      // List the export address table
      writeln();
      if (addressTable !is null) {
        writeln("  Export address table at va ", addressTable.preferredExtent.hexStr,
                " with ", addressTable.entries.length.plural("entries"), ":");
        listErrors(addressTable, "    ");

        foreach (entry; addressTable.entries[]) {
          writef("    [%4d] at va %s", entry.index, entry.preferredExtent.hexStr);
          if (entry.isForwarder) {
            writeln(" forwards to ", entry.forwarderRva.hexStr, " ", entry.forwarder.cEscape);
          } else {
            writeln(" export rva ", entry.exportRva.hexStr, " (va ", fhdr.va(entry.exportRva).hexStr, ")");
          }
          listErrors(entry, "      ");
        }
      } else {
        writeln("  Export address table:\n    No export address table parsed.");
      }

      // List the export name table and ordinal table
      writeln();
      if (nameTable !is null || ordinalTable !is null) {
        size_t nEntries;
        if (nameTable !is null) {
          writeln("  Export name table at va ", nameTable.preferredExtent.hexStr,
                  " with ", nameTable.entries.length.plural("entries"));
          nEntries = nameTable.entries.length;
          listErrors(nameTable, "    ");
        } else {
          writeln("  Export name table (absent)");
        }
        if (ordinalTable !is null) {
          writeln("  parallels export ordinal table at va ", ordinalTable.preferredExtent.hexStr,
                  " with ", ordinalTable.entries.length.plural("entries"));
          nEntries = max(nEntries, ordinalTable.entries.length);
          listErrors(nameTable, "    ");
        } else {
          writeln("  but ordinal table is missing");
        }

        foreach (i; 0 .. nEntries) {
          auto nameEntry = nameTable !is null && i < nameTable.entries.length ? nameTable.entries[i] : null;
          auto ordinalEntry = ordinalTable !is null && i < ordinalTable.entries.length ? ordinalTable.entries[i] : null;

          writef("    [%4d] ", i);
          if (nameEntry !is null) {
            writeln("at va ", nameEntry.preferredExtent.hexStr, " name = ", nameEntry.disk.nameRva.hexStr,
                    " ", nameEntry.name.cEscape);
            listErrors(nameEntry, "           ");
          } else {
            writeln("name entry is absent");
          }

          write("           ");
          if (ordinalEntry !is null) {
            writeln("at va ", ordinalEntry.preferredExtent.hexStr, " ordinal = ", ordinalEntry.disk.ordinal);
            listErrors(ordinalEntry, "           ");
          } else {
            writeln("ordinal entry is absent");
          }
        }
      } else {
        writeln("  Export name table:\n    No export name table parsed.");
      }
    }

    writeln();
  }

  if (0 == nExportSections)
    write("Export section:\n  No export sections parsed.\n\n");
}
  
void listImportSections(size_t nBits)(PeFileHeader!nBits fhdr) {
  enum addrWidth = Word!nBits.min.hexStr.length;
  enum rvaWidth = Word!32.min.hexStr.length;
  size_t nImportSections;
  foreach (section; fhdr.sections[].map!(a => cast(PeImportSection!nBits) a).filter!"a !is null") {
    ++nImportSections;
    writeln("Import section at va ", section.preferredExtent.hexStr, ":");
    listErrors(section);
    if (section.directory.entries.length == 0) {
      writeln("  No import directories parsed.\n");
      continue;
    }

    writeln("  ", leftJustify("Index", 5),
            " ", leftJustify("NameRva", rvaWidth),
            " Name");
    writeln("        ", leftJustify("ILT-RVA", rvaWidth),
            " ", leftJustify("IAT-RVA", rvaWidth),
            " ", leftJustify("FwrdChainIdx", 12),
            " ", leftJustify("TimeStamp", 20));
    writeln("        ", leftJustify("VA", addrWidth),
            "   ", leftJustify("Raw bytes", 16*3+1),
            "  ASCII");
    writeln("        ILT/IAT details");
    writeln("  -----",
            " ", leftJustify("", rvaWidth, '-'),
            " ", leftJustify("", rvaWidth, '-'),
            " ", leftJustify("", 12, '-'),
            " ", leftJustify("", 20, '-'),
            " ---------------- -----------------");

    foreach (directory; section.directory.entries[]) {
      writefln("  [%3d] %s %s", directory.index, directory.disk.nameRva.hexStr, directory.dllName);
      writefln("        %s %s %-12d %s",
               directory.disk.importLookupTableRva.hexStr, directory.disk.importAddressTableRva.hexStr,
               directory.disk.forwarderChainIdx, directory.timeStamp.toSimpleString);
      listErrors(directory, "        ");
      auto fmt = hdFormat;
      fmt.prefix = "        ";
      hexdump(fhdr.memBytes, directory.preferredExtent, fmt);

      // List import lookup table and import address table in parallel
      if (directory.importLookupTable !is null || directory.importAddressTable !is null) {
        size_t nEntries;
        if (directory.importLookupTable !is null) {
          writeln("        Import lookup table at va ", directory.importLookupTable.preferredExtent.hexStr,
                  " with ", directory.importLookupTable.entries.length.plural("entries"));
          nEntries = directory.importLookupTable.entries.length;
          listErrors(directory.importLookupTable, "        ");
        } else {
          writeln("        Import lookup table (absent)");
        }
        if (directory.importAddressTable !is null) {
          writeln("        parallels import address table at va ", directory.importAddressTable.preferredExtent.hexStr,
                  " with ", directory.importAddressTable.entries.length.plural("entries"));
          nEntries = max(nEntries, directory.importAddressTable.entries.length);
          listErrors(directory.importAddressTable, "        ");
        } else {
          writeln("        but import address table is missing");
        }

        foreach (i; 0 .. nEntries) {
          auto ilt = directory.importLookupTable !is null && i < directory.importLookupTable.entries.length ?
            directory.importLookupTable.entries[i] : null;
          auto iat = directory.importAddressTable !is null && i < directory.importAddressTable.entries.length ?
            directory.importAddressTable.entries[i] : null;

          writef("        [%4d] ", i);
          if (ilt !is null) {
            write("ILT entry at va ", ilt.preferredExtent.hexStr);
            if (ilt.isImportByOrdinal) {
              writefln(" ordinal:   %-5d", ilt.ordinal);
            } else {
              writefln(" hint/name: %-5d %s %s", ilt.hint, fhdr.va(ilt.hintNameRva).hexStr, ilt.name.cEscape);
            }
            listErrors(ilt, "               ");
          } else {
            write("ILT entry is absent");
          }

          write("               ");
          if (iat !is null) {
            writeln("IAT entry at va ", iat.preferredExtent.hexStr, " address ", iat.disk.rawValue.hexStr);
            listErrors(iat, "               ");
          }
        }
      }
      writeln();
    }
  }

  if (0 == nImportSections)
    writeln("Import section:\n  No import sections parsed.\n");
}

void listResourceSections(size_t nBits)(PeFileHeader!nBits fhdr) {
  size_t nResourceSections;
  foreach (section; fhdr.sections[].map!(a => cast(PeResourceSection!nBits) a).filter!"a !is null") {
    ++nResourceSections;
    writeln("Resource section at va ", section.preferredExtent.hexStr, ":");
    listErrors(section);
    if (section.root is null) {
      writeln("  No root directory parsed.\n");
      continue;
    }

    size_t depth; // we don't use visit.depth because we only count certain node types
    foreach (visit; section.prePostOrder) {
      if (auto dir = cast(PeResourceDirectoryTable!nBits) visit.node) {
	if (visit.order == VisitOrder.PRE) {
	  auto indent = leftJustify("", 2*++depth, ' ');
	  writeln(indent, "Directory at va ", dir.preferredExtent.hexStr, " ",
		  dir.timeStamp.toSimpleString, " (", dir.disk.timeStamp, ")" ~
		  " version ", dir.disk.majorVersion, ".", dir.disk.minorVersion,
		  ", ", dir.disk.nNameEntries.plural("named entries"), " and ", dir.disk.nIdEntries.plural("id entries"));
	  listErrors(dir, indent);
	} else {
	  assert(depth > 0);
	  --depth;
	}
      } else if (auto dentry = cast(PeResourceDirectoryEntry!nBits) visit.node) {
	if (visit.order == VisitOrder.PRE) {
	  auto indent = leftJustify("", 2*++depth, ' ');
	  writeln(indent, "Directory entry at va ", dentry.preferredExtent.hexStr, " index=", dentry.index,
		  " ", dentry.absoluteName.cEscape);
	  listErrors(dentry, indent);
	} else {
	  assert(depth > 0);
	  --depth;
	}
      } else if (auto leaf = cast(PeResourceDataEntry!nBits) visit.node) {
	if (visit.order == VisitOrder.PRE) {
	  auto indent = leftJustify("", 2*++depth, ' ');
	  writeln(indent, "Resource at va ", leaf.preferredExtent.hexStr, " code page ", leaf.disk.codePage,
		  ", data at va ", leaf.dataPreferredExtent.hexStr, ", ", leaf.disk.size.plural("bytes"));
	  listErrors(leaf, indent);
	  auto fmt = hdFormat;
	  fmt.prefix = indent ~ "  ";
	  leaf.data.hexdump(fmt);
	} else {
	  assert(depth > 0);
	  --depth;
	}
      }
    }
  }

  if (0 == nResourceSections)
    writeln("Resource section:\n  No resource sections parsed.\n");
}
  
void main(string[] argv) {
  enforce(argv.length == 2);

  static if (true) {
    try {
      Ast.totalErrorLimit = 500;
      auto file = AsmFile.parseGiven!(PeFileHeader!32, PeFileHeader!64)(argv[1]);
      enforce(file !is null, "could not parse " ~ argv[1].cEscape);

      if (auto fhdr32 = cast(PeFileHeader!32) file) {
        listPeFile(fhdr32);
      } else if (auto fhdr64 = cast(PeFileHeader!64) file) {
        listPeFile(fhdr64);
      } else {
        writeln("unrecognized file format");
      }
    } catch (ParseError e) {
      writeln(e);
    }
  } else {
    auto fhdr = PeFileHeader!64.instance();
    try {
      fhdr.parseFile(argv[1]);
    } catch (ParseError e) {
      writeln(e);
    }
  }
}
