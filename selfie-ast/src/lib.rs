pub use ordered_float::OrderedFloat;
pub use ustr::Ustr;

pub type Span = chumsky::span::SimpleSpan<usize, Ustr>;

mod call_graph;
pub use call_graph::CallGraph;

mod decl;
pub use decl::{Decl, EnumDecl, Field, FnDecl, Param, ParamKind, StructDecl, Variant};

mod name;
pub use name::Name;

mod program;
pub use program::Program;

mod sym;
pub use sym::Sym;

mod ty;
pub use ty::Type;

pub mod utils;
pub mod visitor;

mod macros;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Module {
    pub span: Span,
    pub sym: Sym,
    pub decls: Vec<Decl>,
}

impl Module {
    pub fn fns(&self) -> impl Iterator<Item = &FnDecl> + '_ {
        self.decls.iter().filter_map(|decl| match decl {
            Decl::Fn(fn_decl) => Some(fn_decl),
            _ => None,
        })
    }

    pub fn fns_mut(&mut self) -> impl Iterator<Item = &mut FnDecl> + '_ {
        self.decls.iter_mut().filter_map(|decl| match decl {
            Decl::Fn(fn_decl) => Some(fn_decl),
            _ => None,
        })
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Literal {
    Int64(Span, i64),
    Float64(Span, OrderedFloat<f64>),
    Bool(Span, bool),
    String(Span, Ustr),
    Unit(Span),
}

impl Literal {
    pub fn span(&self) -> Span {
        match self {
            Self::Int64(span, _) => *span,
            Self::Float64(span, _) => *span,
            Self::Bool(span, _) => *span,
            Self::String(span, _) => *span,
            Self::Unit(span) => *span,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Var {
    pub span: Span,
    pub sym: Sym,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expr {
    Lit(Literal),
    Var(Var),
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

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Self::Lit(lit) => lit.span(),
            Self::Var(var) => var.span(),
            Self::FnCall(call) => call.span(),
            Self::MethodCall(call) => call.span(),
            Self::FieldAccess(field) => field.span(),
            Self::Tuple(tuple) => tuple.span(),
            Self::Let(let_) => let_.span(),
            Self::UnaryOp(op) => op.span(),
            Self::BinaryOp(op) => op.span(),
            Self::If(if_) => if_.span(),
            Self::StructInit(init) => init.span(),
            Self::EnumInit(init) => init.span(),
            Self::Return(expr) => expr.span(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Arg {
    Named(NamedArg),
    Anon(Expr),
}

impl Arg {
    pub fn span(&self) -> Span {
        match self {
            Self::Named(arg) => arg.span(),
            Self::Anon(expr) => expr.span(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NamedArg {
    pub span: Span,
    pub sym: Sym,
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
    pub span: Span,
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
    pub span: Span,
    pub op: Op1,
    pub expr: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FnCall {
    pub span: Span,
    pub sym: Sym,
    pub args: Vec<Arg>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct MethodCall {
    pub span: Span,
    pub expr: Box<Expr>,
    pub sym: Sym,
    pub args: Vec<Arg>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FieldAccess {
    pub span: Span,
    pub expr: Box<Expr>,
    pub sym: Sym,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Let {
    pub span: Span,
    pub sym: Sym,
    pub value: Box<Expr>,
    pub body: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct If {
    pub span: Span,
    pub cnd: Box<Expr>,
    pub thn: Box<Expr>,
    pub els: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct StructInit {
    pub span: Span,
    pub sym: Sym,
    pub args: Vec<NamedArg>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct EnumInit {
    pub span: Span,
    pub sym: Option<Sym>,
    pub variant: Sym,
    pub arg: Option<Box<Expr>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Tuple {
    pub span: Span,
    pub items: Vec<Expr>,
}

impl_span!(
    Module,
    Var,
    NamedArg,
    BinaryOp,
    UnaryOp,
    FnCall,
    MethodCall,
    FieldAccess,
    Let,
    If,
    StructInit,
    EnumInit,
    Tuple
);

impl_sym!(Module, Var, NamedArg, FnCall, MethodCall, FieldAccess, Let);
