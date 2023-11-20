use std::fmt;

pub use ordered_float::OrderedFloat;
pub use ustr::Ustr;

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
        write!(f, "{}", self.0.as_str())
    }
}

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

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Param {
    pub name: Name,
    pub ty: Type,
    pub kind: ParamKind,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ParamKind {
    Anon,
    Normal,
    Alias(Name),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Arg {
    Named(NamedArg),
    Anon(Expr),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NamedArg {
    pub name: Name,
    pub value: Expr,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Module {
    pub decls: Vec<Decl>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FnDecl {
    pub name: Name,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub body: Expr,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct StructDecl {
    pub name: Name,
    pub fields: Vec<Field>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Field {
    pub name: Name,
    pub ty: Type,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct EnumDecl {
    pub name: Name,
    pub variants: Vec<Variant>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Variant {
    pub name: Name,
    pub ty: Option<Type>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Decl {
    Fn(FnDecl),
    Struct(StructDecl),
    Enum(EnumDecl),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Op2 {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq,
    And,
    Or,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Op1 {
    Not,
    Neg,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FnCall {
    pub name: Name,
    pub args: Vec<Arg>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct MethodCall {
    pub expr: Box<Expr>,
    pub name: Name,
    pub args: Vec<Arg>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FieldAccess {
    pub expr: Box<Expr>,
    pub name: Name,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Let {
    pub name: Name,
    pub value: Box<Expr>,
    pub body: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct If {
    pub cnd: Box<Expr>,
    pub thn: Box<Expr>,
    pub els: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct BinaryOp {
    pub op: Op2,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct UnaryOp {
    pub op: Op1,
    pub expr: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct StructInit {
    pub id: Name,
    pub args: Vec<NamedArg>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct EnumInit {
    pub ty: Option<Name>,
    pub variant: Name,
    pub arg: Option<Box<Expr>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Tuple {
    pub items: Vec<Expr>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Literal {
    Int64(i64),
    Float64(OrderedFloat<f64>),
    Bool(bool),
    String(Ustr),
    Unit,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expr {
    Lit(Literal),
    Var(Name),
    FnCall(FnCall),
    MethodCall(MethodCall),
    FieldAccess(FieldAccess),
    Tuple(Tuple),
    Let(Let),
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
    If(If),
    StructInit(StructInit),
    EnumInit(EnumInit),
    Return(Box<Expr>),
}
