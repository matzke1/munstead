module munstead.elf.loader;

import munstead.ast.files;
import munstead.core.interval;
import munstead.core.mmap;
import munstead.core.util;
import munstead.core.wordtypes;
import munstead.elf.dynamic;
import munstead.elf.files;
import munstead.elf.sections;
import munstead.elf.segments;
import munstead.loader;
import std.algorithm: filter, map, minElement;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Loads program headers (segments). This is intended to be as close as possible to the behavior of the native Linux
// loader.
class ElfSegmentLoader(size_t nBits): AsmLoader!nBits {

  this() {
    pageSize = 4096;
  }

  // Load all loadable segments in the file
  override void incrementalLoad(MemoryMap!(Word!nBits) mmap, AsmFile file) {
    assert(mmap !is null);
    assert(file !is null);
    
    auto fhdr = cast(ElfFileHeader!nBits) file;
    assert(fhdr !is null, "wrong file type for loader");
    loadSegments(mmap, fhdr);
  }
    
  // Lowest memory address of any PT_LOAD segment, or zero.
  Word!nBits baseAddress(ElfFileHeader!nBits fhdr) {
    assert(fhdr !is null);
    if (fhdr.segmentTable is null)
      return 0;
    return fhdr.segmentTable.entries[]
      .filter!(phent => phent.disk.p_type == phent.SegmentType.PT_LOAD)
      .map!(phent => phent.disk.p_vaddr)
      .minElement;
  }

  // Memory access permissions for a program header
  AccessFlags memoryAccess(ElfSegmentTableEntry!nBits phent) {
    assert(phent !is null);
    AccessFlags ret;
    if (phent.disk.p_flags.isSet(phent.SegmentFlags.PF_R))
      ret = ret.including(Access.READABLE);
    if (phent.disk.p_flags.isSet(phent.SegmentFlags.PF_W))
      ret = ret.including(Access.WRITABLE);
    if (phent.disk.p_flags.isSet(phent.SegmentFlags.PF_X))
      ret = ret.including(Access.EXECUTABLE);
    return ret;
  }

  // Load each segment at its preferred address, padding the segment to a multiple of the page size and filling
  // holes-due-to-padding with data from the file if possible, zeros otherwise.
  void loadSegments(MemoryMap!(Word!nBits) mmap, ElfFileHeader!nBits fhdr) {
    assert(mmap !is null);
    assert(fhdr !is null);

    if (fhdr.segmentTable !is null) {
      foreach (phent; fhdr.segmentTable.entries[]) {
        if (phent.section !is null && phent.disk.p_type == phent.SegmentType.PT_LOAD) {
          insertSection(mmap, phent.section, memoryAccess(phent), joinNames(fhdr.fileBytes.name, phent.section.printableName),
                        fhdr.fileBytes, AddressSpace.FILE, FillType.ZEROS);
        }
      }
    }
  }

  // Change PT_GNU_RELRO permissions. The PT_GNU_RELRO segment's purpose is to change the permissions of parts of memory
  // from read-write (which was needed during relocations) to read-only (the intended post-relocation permission).
  void updatePermissions(MemoryMap!(Word!nBits) mmap, ElfFileHeader!nBits fhdr) {
    assert(mmap !is null);
    assert(fhdr !is null);

    if (fhdr.segmentTable !is null) {
      foreach (phent; fhdr.segmentTable.entries[]) {
        if (phent.section !is null && phent.disk.p_type == phent.SegmentType.PT_GNU_RELRO)
          mmap.mprotect(alignInterval(phent.preferredExtent), memoryAccess(phent));
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Loads sections. This loader is intended to refine a map that already contains ELF program headers.
class ElfSectionLoader(size_t nBits): AsmLoader!nBits {

  this() {
    pageSize = 1;
  }

  override void incrementalLoad(MemoryMap!(Word!nBits) mmap, AsmFile file) {
    assert(mmap !is null);
    assert(file !is null);
    auto fhdr = cast(ElfFileHeader!nBits) file;
    assert(fhdr !is null, "wrong file type for loader");
    loadSections(mmap, fhdr);
  }

  // Memory access permissions for a section
  AccessFlags memoryAccess(ElfSectionTableEntry!nBits shent) {
    assert(shent !is null);
    auto ret = AccessFlags(Access.READABLE);
    if (shent.disk.sh_flags.isSet(shent.SectionFlags.SHF_WRITE))
      ret = ret.including(Access.WRITABLE);
    if (shent.disk.sh_flags.isSet(shent.SectionFlags.SHF_EXECINSTR))
      ret = ret.including(Access.EXECUTABLE);
    return ret;
  }

  void loadSections(MemoryMap!(Word!nBits) mmap, ElfFileHeader!nBits fhdr) {
    assert(mmap !is null);
    assert(fhdr !is null);

    if (fhdr.sectionTable !is null) {
      foreach (shent; fhdr.sectionTable.entries[]) {
        if (shent.section !is null && !shent.section.preferredExtent.empty && shent.section.preferredExtent.least > 0)
          insertSection(mmap, shent.section, memoryAccess(shent), joinNames(fhdr.fileBytes.name, shent.section.printableName),
                        fhdr.fileBytes, AddressSpace.FILE, FillType.ZEROS);
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Top-level loader for ELF files.
class ElfLoader(size_t nBits): AsmLoader!nBits {

  override void incrementalLoad(MemoryMap!(Word!nBits) mmap, AsmFile file) {
    auto fhdr = cast(ElfFileHeader!nBits) file;
    assert(fhdr !is null);

    auto segloader = new ElfSegmentLoader!nBits;
    segloader.incrementalLoad(mmap, file);

    auto secloader = new ElfSectionLoader!nBits;
    secloader.incrementalLoad(mmap, file);
  }
}
