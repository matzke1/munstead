module munstead.loader;

import munstead.ast.files;
import munstead.ast.sections;
import munstead.core.interval;
import munstead.core.iobuf;
import munstead.core.mmap;
import munstead.core.util;
import munstead.core.wordtypes;
import std.algorithm: filter, map;
import std.array: array;
import std.conv: to;

class AsmLoader(size_t nBits) {
  Word!nBits pageSize = 4096;   // page size in bytes

  enum FillType { EMPTY, ZEROS };

  // Load the file into virtual memory, whatever that means for this kinds of loader.
  MemoryMap!(Word!nBits) load(AsmFile file) {
    auto mmap = new MemoryMap!(Word!nBits);
    incrementalLoad(mmap, file);
    return mmap;
  }

  abstract void incrementalLoad(MemoryMap!(Word!nBits) mmap, AsmFile file);

  // Method of aligning the interval to the page size.
  Interval!(Word!nBits) alignInterval(Interval!(Word!nBits) interval) {
    if (pageSize <= 1 || interval.empty)
      return interval;
    auto base = alignDown(interval.least, pageSize);
    auto size = alignUp(interval.greatest + 1 - base, pageSize);
    return interval.baseSize(base, size);
  }

  // Create a new segment from an existing segment by copying and then adjusting the access permissions and name. The underlying
  // data buffers are not duplicated.
  Segment!(Word!nBits) dupSegment(Segment!(Word!nBits) oldSegment, AccessFlags access, string name) {
    auto segment = new Segment!(Word!nBits)(oldSegment);
    segment.access = access;
    segment.name = name;
    return segment;
  }

  // Create a new section name by joining non-empty names together with spaces.
  string joinNames(string[] names...) {
    string ret;
    foreach (name; names.filter!(s => s!=""))
      ret ~= " " ~ name;
    return ret == "" ? ret : ret[1..$];
  }

  // Put zeros in part of the memory map
  void zeroFill(MemoryMap!(Word!nBits) mmap, Interval!(Word!nBits) where, AccessFlags access, string name) {
    assert(mmap !is null);
    if (!where.empty) {
      auto zeroBuffer = new ArrayBuffer!(mmap.Address, mmap.Value)(where.length);
      auto zeroSegment = new mmap.Segment(zeroBuffer, 0, access, joinNames(name, "zero padding"));
      mmap.insert(where, zeroSegment);
    }
  }

  // Set part of a map to zero or empty
  void emptyOrZero(MemoryMap!(Word!nBits) mmap, Interval!(Word!nBits) where, FillType fillType, AccessFlags access,
                   string name) {
    final switch (fillType) {
      case FillType.EMPTY:
        mmap.remove(where);
        break;
      case FillType.ZEROS:
        zeroFill(mmap, where, access, name);
        break;
    }
  }

  // Insert a section into the specified memory map.  The section is aligned according to the alignInterval method
  // (which usually just aligns according to the page size). Holes that exist in the section's data or due to the
  // alignment process are filled according to the 'backfill' argument as follows: If backfill is null, then holes are
  // zero-filled by allocating buffers initialized to zero; otherwise, if mmap and backfill are the same object nothing
  // is adjusted for holes; otherwise the hole is cleared by removing it from 'mmap' and then segments are copied from
  // the backfill into 'mmap' (without copying the backfill data).  The address space for backfill is either file offsets
  // or virtual memory addresses as determined by backfillSpace.
  void insertSection(MemoryMap!(Word!nBits) mmap, AsmSection!nBits section, AccessFlags access, string name,
                     MemoryMap!(Word!nBits) backfill, AddressSpace backfillSpace, FillType fillType) {
    assert(mmap !is null);
    assert(section !is null);
    if (!section.preferredExtent.empty) {
      auto base = section.preferredExtent.least;

      // Insert the section's data exactly
      foreach (mapNode; section.bytes.bySegment)
        mmap.insert(mapNode.interval.shiftRight(base), dupSegment(mapNode.segment, access, name));

      // Get a list of extents representing the holes around and within the section data due to alignment and holes
      // already present within the section data.
      auto holes = section.bytes
        .bySegment
        .map!(node => node.interval.shiftRight(base))
        .inverseIntervals
        .intersect(alignInterval(section.preferredExtent));

      // Do something to fill in the holes.
      foreach (hole; holes) {
        if (backfill is null) {
          emptyOrZero(mmap, hole, fillType, access, name);

        } else if (backfill is mmap) {
          // don't touch the hole

        } else if (backfillSpace == AddressSpace.MEMORY) {
          // fill with data from another memory map
          mmap.remove(hole);
          foreach (fillNode; backfill.segmentsWithin(hole))
            mmap.insert(fillNode.interval, dupSegment(fillNode.segment, access, fillNode.segment.name));
          foreach (interval; backfill.segmentsWithin(hole).map!"a.interval".inverseIntervals.intersect(hole))
            emptyOrZero(mmap, interval, fillType, access, name);
          
        } else if (!section.fileExtent.empty) {
          // fill with file data
          assert(backfillSpace == AddressSpace.FILE);
          assert(!section.fileExtent.empty);
          mmap.remove(hole);

          if (section.preferredExtent.least > section.fileExtent.least) {
            auto shift = section.preferredExtent.least - section.fileExtent.least;
            auto fileHole = hole.shiftLeftClip(shift);
            foreach (fillNode; backfill.segmentsWithin(fileHole)) {
              string s = joinNames(name, "padding from offset 0x" ~ to!string(fillNode.interval.least, 16));
              mmap.insert(fillNode.interval.shiftRightClip(shift), dupSegment(fillNode.segment, access, s));
            }
            foreach (fileInterval; backfill.segmentsWithin(fileHole).map!"a.interval".inverseIntervals.intersect(fileHole))
              emptyOrZero(mmap, fileInterval.shiftRightClip(shift), fillType, access, name);
            
          } else {
            auto shift = section.fileExtent.least - section.preferredExtent.least;
            auto fileHole = hole.shiftRightClip(shift);
            foreach (fillNode; backfill.segmentsWithin(fileHole)) {
              string s = joinNames(fillNode.segment.name, "padding from offset 0x" ~ to!string(fillNode.interval.least, 16));
              mmap.insert(fillNode.interval.shiftLeftClip(shift), dupSegment(fillNode.segment, access, s));
            }
            foreach (fileInterval; backfill.segmentsWithin(fileHole).map!"a.interval".inverseIntervals.intersect(fileHole))
              emptyOrZero(mmap, fileInterval.shiftLeftClip(shift), fillType, access, name);
          }

        } else {
          // no file data
          emptyOrZero(mmap, hole, fillType, access, name);
        }
      }
    }
  }
}    
