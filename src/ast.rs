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
pub enum Decl {
    Fn {
        name: Name,
        params: Vec<Param>,
        return_type: Type,
        body: Vec<Expr>,
    },
}

#[derive(Debug)]
pub enum BinOp {
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
pub enum Expr {
    Int64(i64),
    Float64(f64),
    String(String),
    Var(Name),
    FnCall {
        name: Name,
        args: Vec<Arg>,
    },
    Let {
        name: Name,
        value: Box<Expr>,
        body: Box<Expr>,
    },
    BinOp {
        op: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    If {
        cnd: Box<Expr>,
        thn: Vec<Expr>,
        els: Vec<Expr>,
    },
    Return(Box<Expr>),
}
