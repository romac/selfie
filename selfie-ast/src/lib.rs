pub use ordered_float::OrderedFloat;
pub use ustr::Ustr;

mod name;
pub use name::Name;

mod ty;
pub use ty::Type;

mod decl;
pub use decl::{Decl, EnumDecl, Field, FnDecl, Param, ParamKind, StructDecl, Variant};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Module {
    pub decls: Vec<Decl>,
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct BinaryOp {
    pub op: Op2,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Op1 {
    Not,
    Neg,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct UnaryOp {
    pub op: Op1,
    pub expr: Box<Expr>,
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
