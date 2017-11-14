module munstead.core.registers;
import std.algorithm: min;
import std.conv: to;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Register descriptor
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/** Describes a machine register. */
struct Register {
private:
  enum: uint { EMPTY_PATTERN = 0x000005ff, OFFSET_WIDTH_MASK = 0x0003ffff }
  uint data_ = EMPTY_PATTERN;

public:
  /** Not-a-register. */
  static Register opCall() {
    return Register.init;
  }

  /** Register with specified properties. */
  static Register opCall(uint majr, uint minr, size_t offset, size_t width) {
    Register retval;
    retval.majorNumber(majr);
    retval.minorNumber(minr);
    retval.setOffsetWidth(offset, width);
    return retval;
  }

  /** Major number.
   *
   *  Each family of registers is identified by a major number. Major numbers are in the range 0 through 15, inclusive. */
  uint majorNumber() const pure @property {
    return data_ >> 28; 				// bits 28-31 (4 bits)
  }

  /** ditto */
  void majorNumber(uint majr) pure @property {
    assert(majr < 16);
    data_ &= 0x0fffffff;
    data_ |= majr << 28;
  }

  /** Minor number.
   *
   *  Within a family of register related by their majorNumber, the minorNumber identifies individuals. Minor numbers
   *  are in the range 0 through 1023, inclusive. */
  uint minorNumber() const pure @property {
    return (data_ >> 18) & 0x3ff;
  }

  /** ditto */
  void minorNumber(uint minr) pure @property {
    assert(minr < 1024);
    data_ &= 0xf003ffff;
    data_ |= minr << 18;
  }

  /** Offset to least-significant bit.
   *	
   *  Each member of a register family (member identified by major and minor number) is conceptually 512 bits wide. A
   *  RegisterDescriptor has an offset and width (nBits property) to describe a contiguous region of that space. The sum
   *  of the offset and width cannot exceed 512. Decreasing the offset preserves the width; increasing the offset may
   *  truncate the width. The offset can be set and queried even for descriptors that are empty (width==0).
   *
   *  See also, setOffsetWidth which sets both offset and size at once. */
  size_t offset() const pure @property {
    if ((data_ & OFFSET_WIDTH_MASK) == EMPTY_PATTERN)
      return 0;
    return (data_ >> 9) & 0x1ff; // bits 9-17 (9 bits)
  }

  /** ditto */
  void offset(size_t offset) pure @property {
    assert(offset < 512);
    size_t width = min(nBits(),512-offset);
    setOffsetWidth(offset, width);
  }
  
  /** Size in bits.
   *
   *  Each member of a register family (member identified by major and minor number) is conceptually 512 bits wide. A
   *  RegisterDescriptor has an @ref offset and this width-in-bits property to describe a contiguous region of that
   *  space. The sum of the offset and width cannot exceed 512 (the setOffsetWidth function sets both at once). */
  size_t nBits() const pure @property {
    if ((data_ & OFFSET_WIDTH_MASK) == EMPTY_PATTERN)
      return 0;
    uint offsetField = (data_ >> 9) & 0x1ff; // bits 9-17 (9 bits)
    uint widthField = data_ & 0x1ff;     // bits 0-9 (9 bits)
    if (offsetField + widthField == 512)
      return 0;
    return widthField + 1;
  }

  /** ditto */
  void nBits(size_t width) pure @property {
    assert(width <= 512);
    setOffsetWidth(offset(), width);
  }

  /** Set offset and size at the same time.
   *
   *  Adjusting the offset and width individually with the offset and nBits properties can be tricky because you need to
   *  always satisfy the invariant that offset + size <= 512.  Setting the offset individually may change the width, and
   *  setting the width individually might cause an invariant to be violated. Therefore, the recommended way to adjust
   *  both is to use this setOffsetWidth method.
   *
   *  The offset and width are set as indicated. Their sum must not exceed 512. */
  void setOffsetWidth(size_t offset, size_t width) pure {
    assert(offset < 512);
    assert(offset + width <= 512);

    data_ &= ~OFFSET_WIDTH_MASK;
    if (0 == offset && 0 == width) {
      data_ |= EMPTY_PATTERN;
    } else if (0 == width) {
      uint widthField = cast(uint)(512 - offset);
      data_ |= (offset << 9) | widthField;
    } else {
      data_ |= (offset << 9) | (width - 1);
    }

    assert(this.offset() == offset);
    assert(this.nBits() == width);
  }

  /** Predicate returns true if the width is zero.
   *
   *  Default-constructed registers have an initial width of zero. */
  bool empty() const pure @property {
    return 0 == nBits();
  }

  /** Compare two descriptors.
   *
   *  Descriptors are sorted by major and minor numbers. If two descriptors have the same major and minor numbers then this
   *  less-than operation satisfies the requirements of a strict weak ordering although the exact details are not specified
   *  (it is not a simple comparison of offset and width). */
  int opCmp(ref const Register other) const {
    if (data_ < other.data_)
      return -1;
    if (data_ > other.data_)
      return 1;
    return 0;
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Register dictionary
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/** Associates register descriptors and names.
 *
 *  A dictionary maintains a 1:1 mapping between register descriptors and names. */
class RegisterDictionary {
private:
  string architectureName_;
  string[Register] names_;
  Register[string] registers_;

public:
  /** Create a new, empty dictionary. */
  this(string name) {
    architectureName_ = name;
  }

  /** Name of dictionary.
   *
   *  This is used mostly for debugging. */
  string architecture() const pure @property {
    return architectureName_;
  }

  /** ditto */
  void architecture(string name) pure @property {
    architectureName_ = name;
  }

  /** Insert a register/name pair.
   *
   *  Any pairs already having the specified register descriptor or name are first removed. */
  void insert(Register register, string name) {
    if (name in registers_)
      names_.remove(registers_[name]);
    if (register in names_)
      registers_.remove(names_[register]);
    names_[register] = name;
    registers_[name] = register;
  }

  /** Insert all other registers/names into this dictionary.
   *
   *  Any pairs already having any of the specified register descriptors or names are first removed. */
  void insert(RegisterDictionary other) {
    foreach (name, register; other.registers_)
      insert(register, name);
  }

  /** Name for a register.
   *
   *  Returns the name associated with the given register descriptor, or the empty string if there is no such register
   *  descriptor. */
  string name(Register register) const {
    return names_.get(register, "");
  }
  
  /** Register descriptor for a name.
   *
   *  Returns the register descriptor associated with the given name, or the default-constructed descriptor if no such
   *  name exists. */
  Register register(string name) const {
    return registers_.get(name, Register());
  }

  /** Find the largest register with the same major and minor number.
   *
   *  Scans the hash to find the largest register having the same major and minor numbers as the specified register.
   *  If multiple registers tie for largest, the one with the lowest offset is returned.  If the specified register
   *  is default constructed, or if no existing registers match, then a default constructed register descriptor is
   *  returned.
   *
   *  This is not an efficient operation, being O(N) where N is the number of register/name pairs in this dictionary.
   *  It's intended mainly for the exceptional case of trying to create a reasonable name for a register that doesn't
   *  exist. */
  Register largestOverlapping(Register needle) const {
    Register retval;
    if (!needle.empty) {
      foreach (register; registers_.byValue) {
	if (register.majorNumber() == needle.majorNumber() && register.minorNumber() == needle.minorNumber()) {
	  if (retval.empty || retval.nBits > retval.nBits) {
	    retval = register;
	  } else if (register.nBits == retval.nBits && register.offset < retval.offset) {
	    retval = register;
	  }
	}
      }
    }
    return retval;
  }

  /** Convert a register descriptor to a string.
   *
   *  This is similar to the "name" method, but generates a sane name rather than returning empty strings for registers
   *  that don't exist. If the specified register descriptor exists in the map, then the corresponding name is returned.
   *  Otherwise, if the dictionary has some register(s) that encapsulate the specified descriptor, the largest one is
   *  chosen with largestOverlapping and then an offset and size indicator like "{@o+n}" is appended, where "o" is the
   *  bit offset and "n" is the number of bits. If no similar register can be found then a string like "REG{a,b @o+n}"
   *  is returned where "a" and "b" are the major and minor numbers and "o" and "n" are the offset and number of bits. */
  string toString(Register register) {
    string retval;
    if (register.empty) {
      retval = "REG{none}";
    } else {
      retval = name(register);
      if (retval == "") {
	Register overlap = largestOverlapping(register);
	if (!overlap.empty()) {
	  retval = name(overlap) ~ "{@" ~ (register.offset - overlap.offset).to!string ~ "+" ~ register.nBits.to!string ~ "}";
	} else {
	  retval = "REG{" ~ register.majorNumber.to!string ~ "," ~ register.minorNumber.to!string ~
	    " @" ~ register.offset.to!string ~ "+" ~ register.nBits.to!string ~ "}";
	}
      }
    }
    assert(retval != "");
    return retval;
  }
}
