#[derive(Debug)]
pub struct Name(pub(crate) String);

#[derive(Debug)]
pub enum Type {
    Int64,
    Float64,
    String,
    Bool,
    Unit,
    Named(Name),
    Fn { args: Vec<Type>, ret: Box<Type> },
}

#[derive(Debug)]
pub struct Param {
    pub name: Name,
    pub ty: Type,
    pub anon: bool,
}

#[derive(Debug)]
pub enum Arg {
    Named(NamedArg),
    Anon(Expr),
}

#[derive(Debug)]
pub struct NamedArg {
    pub name: Name,
    pub value: Expr,
}

#[derive(Debug)]
pub struct Module {
    pub decls: Vec<Decl>,
}

#[derive(Debug)]
pub struct FnDecl {
    pub name: Name,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub body: Vec<Expr>,
}

#[derive(Debug)]
pub struct StructDecl {
    pub name: Name,
    pub fields: Vec<Param>,
}

#[derive(Debug)]
pub struct EnumDecl {
    pub name: Name,
    pub variants: Vec<Variant>,
}

#[derive(Debug)]
pub struct Variant {
    pub name: Name,
    pub ty: Type,
}

#[derive(Debug)]
pub enum Decl {
    Fn(FnDecl),
    Struct(StructDecl),
    Enum(EnumDecl),
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct FnCall {
    pub name: Name,
    pub args: Vec<Arg>,
}

#[derive(Debug)]
pub struct Let {
    pub name: Name,
    pub value: Box<Expr>,
    pub body: Box<Expr>,
}

#[derive(Debug)]
pub struct If {
    pub cnd: Box<Expr>,
    pub thn: Vec<Expr>,
    pub els: Vec<Expr>,
}

#[derive(Debug)]
pub struct BinOp {
    pub op: Op2,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub enum Expr {
    Int64(i64),
    Float64(f64),
    String(String),
    Var(Name),
    FnCall(FnCall),
    Let(Let),
    BinOp(BinOp),
    If(If),
    Return(Box<Expr>),
}
