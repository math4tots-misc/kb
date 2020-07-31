use std::borrow;
use std::cell::Ref;
use std::cell::RefCell;
use std::cmp;
use std::fmt;
use std::hash;
use std::ops;
use std::rc::Rc;

/// str smarat pointer that also stashes chars as
/// needed so that char access can be constant time
#[derive(Clone)]
pub struct RcStr(Rc<Str>);

pub struct Str {
    string: String,
    chars: RefCell<Option<Chars>>,
}

enum Chars {
    ASCII,
    Chars(Vec<char>),
}

impl RcStr {
    fn new(s: String) -> Self {
        s.into()
    }
    unsafe fn new_ascii(string: String) -> Self {
        Self(Rc::new(Str {
            string,
            chars: RefCell::new(Some(Chars::ASCII)),
        }))
    }
    pub fn unwrap_or_clone(self) -> String {
        match Rc::try_unwrap(self.0) {
            Ok(string) => string.string,
            Err(rc) => rc.string.clone(),
        }
    }
    pub fn len(&self) -> usize {
        self.0.string.len()
    }
    fn chars(&self) -> Ref<Chars> {
        if self.0.chars.borrow().is_none() {
            let mut opt_chars = self.0.chars.borrow_mut();
            *opt_chars = Some(if self.0.string.is_ascii() {
                Chars::ASCII
            } else {
                Chars::Chars(self.0.string.chars().collect())
            });
        }
        Ref::map(self.0.chars.borrow(), |chars| chars.as_ref().unwrap())
    }
    pub fn charlen(&self) -> usize {
        let chars = self.chars();
        let chars: &Chars = &chars;
        match chars {
            Chars::ASCII => self.len(),
            Chars::Chars(chars) => chars.len(),
        }
    }
    pub fn charslice(&self, start: usize, end: usize) -> RcStr {
        let chars = self.chars();
        let chars: &Chars = &chars;
        match chars {
            Chars::ASCII => unsafe { Self::new_ascii(self.0.string[start..end].to_owned()) },
            Chars::Chars(chars) => Self::new(chars[start..end].iter().collect()),
        }
    }
    pub fn getchar(&self, index: usize) -> Option<char> {
        let chars = self.chars();
        let chars: &Chars = &chars;
        match chars {
            Chars::ASCII => self.0.string.as_bytes().get(index).map(|c| *c as char),
            Chars::Chars(chars) => chars.get(index).cloned(),
        }
    }
}

impl fmt::Debug for RcStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0.string)
    }
}

impl fmt::Display for RcStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.string)
    }
}

impl borrow::Borrow<str> for RcStr {
    fn borrow(&self) -> &str {
        self.0.string.borrow()
    }
}

impl ops::Deref for RcStr {
    type Target = str;

    fn deref(&self) -> &str {
        self.0.string.deref()
    }
}

impl AsRef<str> for RcStr {
    fn as_ref(&self) -> &str {
        self.0.string.as_ref()
    }
}

impl cmp::PartialOrd for RcStr {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        self.0.string.partial_cmp(&other.0.string)
    }
}

impl cmp::Ord for RcStr {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.0.string.cmp(&other.0.string)
    }
}

impl cmp::PartialEq for RcStr {
    fn eq(&self, other: &Self) -> bool {
        self.0.string.eq(&other.0.string)
    }
}

impl cmp::Eq for RcStr {}

impl hash::Hash for RcStr {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.0.string.hash(state)
    }
}

impl ops::Index<ops::Range<usize>> for RcStr {
    type Output = str;
    fn index(&self, index: ops::Range<usize>) -> &str {
        self.0.string.index(index)
    }
}

impl ops::Index<ops::RangeTo<usize>> for RcStr {
    type Output = str;
    fn index(&self, index: ops::RangeTo<usize>) -> &str {
        self.0.string.index(index)
    }
}

impl ops::Index<ops::RangeFrom<usize>> for RcStr {
    type Output = str;
    fn index(&self, index: ops::RangeFrom<usize>) -> &str {
        self.0.string.index(index)
    }
}

impl ops::Index<ops::RangeFull> for RcStr {
    type Output = str;
    fn index(&self, index: ops::RangeFull) -> &str {
        self.0.string.index(index)
    }
}

impl From<String> for RcStr {
    fn from(string: String) -> Self {
        Self(Rc::new(Str {
            string,
            chars: RefCell::new(None),
        }))
    }
}

impl From<&String> for RcStr {
    fn from(string: &String) -> Self {
        string.clone().into()
    }
}

impl From<&str> for RcStr {
    fn from(s: &str) -> Self {
        s.to_owned().into()
    }
}
