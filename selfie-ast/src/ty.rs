use crate::Name;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Int64,
    Float64,
    String,
    Bool,
    Unit,
    Tuple(Vec<Type>),
    Named(Name),
    Fn { args: Vec<Type>, ret: Box<Type> },
}