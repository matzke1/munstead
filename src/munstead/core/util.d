// miscellaneous things
module munstead.core.util;


bool isEnumMember(E, I)(I value) {
  import std.traits: EnumMembers;
  foreach (member; EnumMembers!E) {
    if (member == value)
      return true;
  }
  return false;
}
    
T alignUp(T)(T value, T alignment) {
  return alignment <= 1 ? value : alignment * ((value + alignment - 1) / alignment);
}

T alignDown(T)(T value, T alignment) {
  return alignment <= 1 ? value : alignment * (value / alignment);
}
