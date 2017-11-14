module munstead.core.hexdump;

import core.stdc.ctype: isprint;
import munstead.core.interval;
import munstead.core.mmap;
import munstead.core.util;
import std.stdio;

struct HexDumpFormat {
  string prefix;
}

void
hexdump(Address)(MemoryMap!(Address,ubyte) map, Interval!Address where, HexDumpFormat fmt) {
  foreach (region; map.segmentsWithin(where).byBuffer(16,16)) {
    auto firstAddr = alignDown(region.interval.least, 16);
    auto leftPadding = region.interval.least - firstAddr;
    auto rightPadding = 16 - (region.interval.length + leftPadding);

    // Address
    write(fmt.prefix);
    write(firstAddr.hexStr, ": ");

    // Bytes
    auto va = firstAddr;
    string ascii;
    for (size_t i = 0; i < 16; ++i, ++va) {
      if (8 == i)
        write(" ");

      if (!region.interval.contains(va)) {
        write("   "); // no data
        ascii ~= " ";
      } else {
        ubyte b = region.buffer[va - region.interval.least];
        if (0 == b) {
          write(" --"); // so zeros stand out a bit more
          ascii ~= ".";
        } else if (0xff == b) {
          write(" ##"); // so 0xff stands out a bit more
          ascii ~= ".";
        } else {
          writef(" %02x", b);
          if (isprint(b)) {
            ascii ~= cast(char) b;
          } else {
            ascii ~= ".";
          }
        }
      }
    }

    // ASCII
    writeln("  |", ascii[0..8], " ", ascii[8..$], "|");
  }
}

void
hexdump(Map)(Map map, HexDumpFormat fmt) {
  hexdump(map, map.hull, fmt);
}

void
hexdump(Map)(Map map, Interval!(Map.Address) where) {
  HexDumpFormat fmt;
  hexdump(map, where, fmt);
}

void
hexdump(Map)(Map map) {
  HexDumpFormat fmt;
  hexdump(map, fmt);
}


