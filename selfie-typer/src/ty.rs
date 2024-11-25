use core::fmt;

use indexmap::IndexMap;

use selfie_ast::Sym;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Int64,
    Float64,
    Bool,
    Char,
    Unit,
    String,
    Tuple(Vec<Type>),
    Named(Sym),
    Fn(FnType),
    Unknown,
}

impl From<&selfie_ast::Type> for Type {
    fn from(value: &selfie_ast::Type) -> Self {
        match value {
            selfie_ast::Type::Int64(_) => Type::Int64,
            selfie_ast::Type::Float64(_) => Type::Float64,
            selfie_ast::Type::Bool(_) => Type::Bool,
            selfie_ast::Type::Char(_) => Type::Char,
            selfie_ast::Type::Unit(_) => Type::Unit,
            selfie_ast::Type::String(_) => Type::String,
            selfie_ast::Type::Tuple(_, tys) => Type::Tuple(tys.iter().map(Type::from).collect()),
            selfie_ast::Type::Named(_, sym) => Type::Named(*sym),
            selfie_ast::Type::Fn { args, ret, .. } => Type::Fn(FnType {
                args: args.iter().map(Type::from).collect(),
                ret: Box::new(Type::from(ret.as_ref())),
            }),
        }
    }
}

impl From<selfie_ast::Type> for Type {
    fn from(value: selfie_ast::Type) -> Self {
        Self::from(&value)
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Unknown => write!(f, "{{unknown}}"),
            Type::Int64 => write!(f, "Int64"),
            Type::Float64 => write!(f, "Float64"),
            Type::Bool => write!(f, "Bool"),
            Type::Char => write!(f, "Char"),
            Type::Unit => write!(f, "Unit"),
            Type::String => write!(f, "String"),
            Type::Tuple(tys) => {
                write!(f, "(")?;
                for (i, ty) in tys.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{ty}")?;
                }
                write!(f, ")")
            }
            Type::Named(sym) => write!(f, "{sym}"),
            Type::Fn(FnType { args, ret }) => {
                write!(f, "(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg}")?;
                }
                write!(f, ") -> {ret}")
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FnType {
    pub args: Vec<Type>,
    pub ret: Box<Type>,
}

impl FnType {
    pub fn new(args: Vec<Type>, ret: Box<Type>) -> Self {
        Self { args, ret }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructType {
    pub fields: IndexMap<Sym, Type>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EnumType {
    pub variants: IndexMap<Sym, Option<Type>>,
}
