use core::fmt;
use std::collections::BTreeSet;
use std::path::PathBuf;
use std::str::FromStr;

use clap::Parser;

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
pub struct Cli {
    pub file: PathBuf,

    #[arg(short, long)]
    pub debug: DebugSections,
}

#[derive(Clone, Debug, Default)]
pub struct DebugSections {
    sections: BTreeSet<DebugSection>,
}

impl DebugSections {
    pub const fn new() -> Self {
        Self {
            sections: BTreeSet::new(),
        }
    }

    pub fn contains(&self, section: DebugSection) -> bool {
        self.sections.contains(&section)
    }
}

impl FromStr for DebugSections {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let sections = s
            .split(',')
            .map(|s| s.parse())
            .collect::<Result<BTreeSet<_>, _>>()?;

        Ok(Self { sections })
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum DebugSection {
    Lex,
    Parse,
    Name,
    Type,
    CallGraph,
}

const DEBUG_SECTIONS: &[DebugSection] = &[
    DebugSection::Lex,
    DebugSection::Parse,
    DebugSection::Name,
    DebugSection::Type,
    DebugSection::CallGraph,
];

impl fmt::Display for DebugSection {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Lex => write!(f, "lex"),
            Self::Parse => write!(f, "parse"),
            Self::Name => write!(f, "name"),
            Self::Type => write!(f, "type"),
            Self::CallGraph => write!(f, "call-graph"),
        }
    }
}

impl FromStr for DebugSection {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "lex" => Ok(Self::Lex),
            "parse" => Ok(Self::Parse),
            "name" => Ok(Self::Name),
            "type" => Ok(Self::Type),
            "call-graph" => Ok(Self::CallGraph),

            _ => {
                let available = DEBUG_SECTIONS
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");

                Err(format!(
                    "unknown debug section '{s}', available: {available}"
                ))
            }
        }
    }
}
