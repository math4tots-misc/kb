
* address potential issue of converting between usize/u32 in many places
* assign limits to some entities that can overflow (e.g. number of labels
    number of opcodes, etc)
    u32::MAX or even i32::MAX seems like a pretty generous limit for most things
* List/Set/Map comprehensions
