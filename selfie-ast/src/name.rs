use std::fmt;

use crate::Ustr;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(Ustr);

impl Name {
    pub fn new(s: &str) -> Self {
        Self::interned(Ustr::from(s))
    }

    pub fn interned(u: Ustr) -> Self {
        Self(u)
    }

    pub fn is_camel_case(&self) -> bool {
        self.0.chars().next().is_some_and(|c| c.is_uppercase())
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl fmt::Debug for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}
