module munstead.pe.loader;

import munstead.ast.files;
import munstead.core.mmap;
import munstead.core.util;
import munstead.core.wordtypes;
import munstead.loader;
import munstead.pe.files;
import munstead.pe.sections;

// Loads the entire file as one big chunk into memory using the imageBase field in the file's optional header
class PeFileLoader(size_t nBits): AsmLoader!nBits {
  this() {
    pageSize = 4096;
  }

  override void incrementalLoad(MemoryMap!(Word!nBits) mmap, AsmFile file) {
    assert(mmap !is null);
    assert(file !is null);
    auto fhdr = cast(PeFileHeader!nBits) file;
    assert(fhdr !is null, "wrong file type for loader");

    Word!nBits baseAddress = fhdr.optionalHeader.disk.imageBase;
    auto access = AccessFlags(Access.READ_WRITE_EXECUTE);

    assert(fhdr.fileBytes !is null);
    assert(fhdr.fileBytes !is mmap); // we'd have to arrange the following loop a bit differently
    foreach (mapNode; fhdr.fileBytes.bySegment) {
      auto newInterval = mapNode.interval.shiftRightClip(baseAddress);
      if (!newInterval.empty)
	mmap.insert(newInterval,
		    dupSegment(mapNode.segment, access, joinNames(fhdr.fileBytes.name, "file load at image base")));
    }
  }
}

// Loads each section of the section table
class PeSectionLoader(size_t nBits): AsmLoader!nBits {
  this() {
    pageSize = 4096;
  }

  AccessFlags memoryAccess(PeSectionTableEntry!nBits shent) {
    assert(shent !is null);
    AccessFlags ret;
    with (shent.Characteristics) {
      if (shent.disk.characteristics.isSet(MEM_READ))
        ret = ret.including(Access.READABLE);
      if (shent.disk.characteristics.isSet(MEM_WRITE))
        ret = ret.including(Access.WRITABLE);
      if (shent.disk.characteristics.isSet(MEM_EXECUTE))
        ret = ret.including(Access.EXECUTABLE);
    }
    return ret;
  }

  override void incrementalLoad(MemoryMap!(Word!nBits) mmap, AsmFile file) {
    assert(mmap !is null);
    assert(file !is null);
    auto fhdr = cast(PeFileHeader!nBits) file;
    assert(fhdr !is null, "wrong file type for loader");

    if (fhdr.sectionTable is null)
      return;
    loadSectionTableEntries(mmap, fhdr);
  }

  // Loads section table entries into memory without having created their AsmSection objects yet
  void loadSectionTableEntries(MemoryMap!(Word!nBits) mmap, PeFileHeader!nBits fhdr) {
    assert(mmap !is null);
    assert(fhdr !is null);
    
    Word!nBits baseAddress = fhdr.optionalHeader.disk.imageBase;
    foreach (shent; fhdr.sectionTable.entries[]) {
      assert(shent.section is null);
      if (!shent.sectionPreferredExtent.empty) {

        // In order to call createContent, the temporary section needs to be linked into the AST. But since we don't
        // really know the section's ultimate type, we'll just unlink it afterward.
        auto tempSection = PeSection!nBits.instance();
        fhdr.sections.pushBack(tempSection);
        tempSection.createContent(shent);
        insertSection(mmap, tempSection, memoryAccess(shent), joinNames(fhdr.fileBytes.name, tempSection.printableName),
                      fhdr.fileBytes, AddressSpace.FILE, FillType.ZEROS);
        fhdr.sections.popBack();
      }
    }
  }
}
