module munstead.elf.dynamic;

import munstead.ast.base;
import munstead.core.byteorder;
import munstead.core.mmap;
import munstead.core.wordtypes;
import munstead.elf.files;
import munstead.elf.sections;
import munstead.elf.segments;
import munstead.elf.types;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class ElfDynamicSection(size_t nBits): ElfSection!nBits {
  mixin AstNodeFeatures;
  struct AstChildren {
    AstList!(ElfDynamicEntry!nBits) entries;
  }

  this() {
    entries = new AstList!(ElfDynamicEntry!nBits);
  }

  static ElfDynamicSection
  parse(MemoryMap)(MemoryMap file, ElfFileHeader!nBits fhdr, ElfSectionTableEntry!nBits shent) {
    assert(file !is null);
    assert(fhdr !is null);
    assert(shent !is null);
    auto ret = new ElfDynamicSection;
    ret.initFromSectionTableEntry(file, shent);
    ret.parse(fhdr);
    return ret;
  }

  static ElfDynamicSection
  parse(MemoryMap)(MemoryMap file, ElfFileHeader!nBits fhdr, ElfSegmentTableEntry!nBits phent) {
    assert(file !is null);
    assert(fhdr !is null);
    assert(phent !is null);
    auto ret = new ElfDynamicSection;
    ret.initFromSegmentTableEntry(file, phent);
    ret.parse(fhdr);
    return ret;
  }

  void parse(ElfFileHeader!nBits fhdr) {
    enum bytesPerEntry = ElfDynamicEntry!nBits.Disk.sizeof;
    size_t nEntries = mmap.length / bytesPerEntry;
    for (size_t index = 0; index < nEntries; ++index) {
      auto offset = cast(Word!nBits)(index * bytesPerEntry);
      auto entry = ElfDynamicEntry!nBits.parse(this, offset, fhdr.byteOrder);
      entry.index = index;
      entries.pushBack(entry);
      if (entry.Tag.DT_NULL == entry.disk.d_tag)
	break;
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class ElfDynamicEntry(size_t nBits): Ast {
  mixin AstNodeFeatures;
  struct AstChildren {}

  enum Tag : Elf!nBits.Sxword {
    //                                  Union       Executable  Library     Level-2
    //                                  ----------- ----------- ----------- ---------
    DT_NULL        	   =0, 	//      ignored     mandatory   mandatory   no
    DT_NEEDED      	   =1, 	//      d_val       optional    optional    no
    DT_PLTRELSZ    	   =2, 	//      d_val       optional    optional    no
    DT_PLTGOT      	   =3, 	//      d_ptr       optional    optional    no
    DT_HASH        	   =4, 	//      d_ptr       mandatory   mandatory   no
    DT_STRTAB      	   =5, 	//      d_ptr       mandatory   mandatory   no
    DT_SYMTAB      	   =6, 	//      d_ptr       mandatory   mandatory   no
    DT_RELA        	   =7, 	//      d_ptr       mandatory   optional    no
    DT_RELASZ      	   =8, 	//      d_val       mandatory   optional    no
    DT_RELAENT     	   =9, 	//      d_val       mandatory   optional    no
    DT_STRSZ       	   =10, //      d_val       mandatory   mandatory   no
    DT_SYMENT      	   =11, //      d_val       mandatory   mandatory   no
    DT_INIT        	   =12, //      d_ptr       optional    optional    no
    DT_FINI        	   =13, //      d_ptr       optional    optional    no
    DT_SONAME      	   =14, //      d_val       ignored     optional    no
    DT_RPATH      	   =15, //      d_val       optional    ignored     yes
    DT_SYMBOLIC   	   =16, //      ignored     ignored     optional    yes
    DT_REL         	   =17, //      d_ptr       mandatory   optional    no
    DT_RELSZ       	   =18, //      d_val       mandatory   optional    no
    DT_RELENT      	   =19, //      d_val       mandatory   optional    no
    DT_PLTREL      	   =20, //      d_val       optional    optional    no
    DT_DEBUG       	   =21, //      d_ptr       optional    ignored     no
    DT_TEXTREL    	   =22, //      ignored     optional    optional    yes
    DT_JMPREL      	   =23, //      d_ptr       optional    optional    no
    DT_BIND_NOW   	   =24, //      ignored     optional    optional    yes
    DT_INIT_ARRAY  	   =25, //      d_ptr       optional    optional    no
    DT_FINI_ARRAY  	   =26, //      d_ptr       optional    optional    no
    DT_INIT_ARRAYSZ	   =27, //      d_val       optional    optional    no
    DT_FINI_ARRAYSZ	   =28, //      d_val       optional    optional    no
    DT_RUNPATH     	   =29, //      d_val       optional    optional    no
    DT_FLAGS       	   =30, //      d_val       optional    optional    no
    DT_ENCODING    	   =32, //      unspecified unspecified unspecified no
    DT_PREINIT_ARRAY       =32, //      d_ptr       optional    ignored     no
    DT_PREINIT_ARRAYSZ     =33, //      d_val       optional    ignored     no
    DT_SYMTAB_SHNDX        =34, //      d_ptr       optional    optional    no
    DT_LOOS        =0x6000000D, //      unspecified unspecified unspecified no
    DT_HIOS        =0x6ffff000, //      unspecified unspecified unspecified no
    DT_LOPROC      =0x70000000, //      unspecified unspecified unspecified no
    DT_HIPROC      =0x7fffffff  //      unspecified unspecified unspecified no
  }

  // Bit flags for the DT_FLAGS entry
  enum Flags {
    DF_ORIGIN     = 0x01,
    DF_SYMBOLIC   = 0x02,
    DF_TEXTREL 	  = 0x04,
    DF_BIND_NOW   = 0x08,
    DF_STATIC_TLS = 0x10
  }

  struct Disk {
    align(1):
    Tag d_tag;
    Elf!nBits.Xword d_val; // or file memory address d_addr which is the same size
  }
  
  size_t index; // index of this entry in the dynamic section
  Disk disk;

  static ElfDynamicEntry
  parse(ElfDynamicSection!nBits section, Word!nBits offset, ByteOrder byteOrder) {
    auto ret = new ElfDynamicEntry;
    section.mmap.readObjectAt(offset, ret.disk);
    ret.disk = toNative(ret.disk, byteOrder);
    return ret;
  }

  bool isPointer() const pure {
    switch (disk.d_tag) {
      case Tag.DT_PLTGOT:
      case Tag.DT_HASH:
      case Tag.DT_STRTAB:
      case Tag.DT_SYMTAB:
      case Tag.DT_RELA:
      case Tag.DT_INIT:
      case Tag.DT_FINI:
      case Tag.DT_REL:
      case Tag.DT_DEBUG:
      case Tag.DT_JMPREL:
      case Tag.DT_INIT_ARRAY:
      case Tag.DT_FINI_ARRAY:
      case Tag.DT_PREINIT_ARRAY:
      case Tag.DT_SYMTAB_SHNDX:
	return true;
      default:
	if (disk.d_tag >= Tag.DT_ENCODING && (disk.d_tag < Tag.DT_LOPROC || disk.d_tag > Tag.DT_HIPROC))
	  return disk.d_tag % 2 == 0;
	return false;
    }
  }
}				     
