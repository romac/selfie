use core::hash::{Hash, Hasher};
use std::fmt;

use crate::Name;

#[derive(Copy, Clone, Eq)]
pub struct Sym {
    pub global_id: u64,
    pub id: u64,
    pub name: Name,
}

impl Sym {
    pub fn new(name: Name) -> Self {
        Self {
            global_id: 0,
            id: 0,
            name,
        }
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
        write!(f, "{}#{}", self.name.as_str(), self.id)
    }
}

impl fmt::Display for Sym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name.as_str())
    }
}

impl PartialEq for Sym {
    fn eq(&self, other: &Self) -> bool {
        self.global_id == other.global_id
    }
}

impl PartialOrd for Sym {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Sym {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.global_id.cmp(&other.global_id)
    }
}

impl Hash for Sym {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.global_id.hash(state);
    }
}
