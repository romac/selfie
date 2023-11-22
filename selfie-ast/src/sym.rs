use core::hash::Hash;
use std::fmt;

use crate::Name;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Sym {
    pub name: Name,
    pub id: u32,
}

impl Sym {
    pub fn new(name: Name) -> Self {
        Self { name, id: 0 }
    }

    pub fn from(name: &str) -> Self {
        Self::new(Name::new(name))
    }

    pub fn as_str(&self) -> &str {
        self.name.as_str()
    }
}

impl fmt::Debug for Sym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}${}", self.name.as_str(), self.id)
    }
}

impl fmt::Display for Sym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name.as_str())
    }
}
