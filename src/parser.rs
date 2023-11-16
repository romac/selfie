use thiserror::Error;

use crate::ast::*;
use crate::lexer::{lex, Token};

#[derive(Clone, Debug, Error)]
pub enum ParseError {
    #[error("Lex error: {0}")]
    Lex(String),

    #[error("Unexpected end of file")]
    UnexpectedEof,

    #[error("Unexpected token {0:?}")]
    UnexpectedToken(Token),

    #[error("Expected identifier")]
    ExpectedIdentifier,

    #[error("Expected {0:?}, instead got {1:?}")]
    ExpectedToken(Token, Token),
}

type TokenStream = std::iter::Peekable<std::vec::IntoIter<Token>>;

pub fn parse(contents: &str) -> Result<Module, ParseError> {
    let tokens = lex(contents).map_err(|e| ParseError::Lex(e.to_string()))?;
    let decls = parse_decls(tokens)?;
    Ok(Module { decls })
}

macro_rules! eat_match {
    ($tokens:ident, $($token:pat => $expr:expr),+ $(,)*) => {
        match $tokens.next() {
            $(Some($token) => $expr,)+
            Some(token) => return Err(ParseError::UnexpectedToken(token)),
            None => return Err(ParseError::UnexpectedEof),
        }
    };
}

fn parse_decls(tokens: Vec<Token>) -> Result<Vec<Decl>, ParseError> {
    let mut tokens = tokens.into_iter().peekable();
    let mut decls = Vec::new();
    while let Some(decl) = parse_decl(&mut tokens)? {
        decls.push(decl);
    }
    Ok(decls)
}

fn parse_decl(tokens: &mut TokenStream) -> Result<Option<Decl>, ParseError> {
    match tokens.peek() {
        Some(Token::Fn) => parse_fn_decl(tokens).map(Some),
        Some(Token::Struct) => parse_struct_decl(tokens).map(Some),
        Some(Token::Enum) => parse_enum_decl(tokens).map(Some),
        Some(other) => Err(ParseError::UnexpectedToken(other.clone())),
        _ => Ok(None),
    }
}

fn parse_fn_decl(tokens: &mut TokenStream) -> Result<Decl, ParseError> {
    tokens.eat(Token::Fn)?;

    let name = parse_name(tokens)?;
    let params = parse_params(tokens)?;
    let return_type = if let Some(Token::Arrow) = tokens.peek() {
        tokens.next();
        parse_type(tokens)?
    } else {
        Type::Unit
    };

    let body = parse_block(tokens)?;

    Ok(Decl::Fn(FnDecl {
        name,
        params,
        return_type,
        body,
    }))
}

fn parse_struct_decl(tokens: &mut TokenStream) -> Result<Decl, ParseError> {
    tokens.eat(Token::Struct)?;
    let name = parse_name(tokens)?;
    tokens.eat(Token::BraceOpen)?;
    let fields = parse_fields(tokens)?;
    tokens.eat(Token::BraceClose)?;
    Ok(Decl::Struct(StructDecl { name, fields }))
}

fn parse_enum_decl(tokens: &mut TokenStream) -> Result<Decl, ParseError> {
    tokens.eat(Token::Enum)?;
    let name = parse_name(tokens)?;
    tokens.eat(Token::BraceOpen)?;
    let variants = parse_variants(tokens)?;
    tokens.eat(Token::BraceClose)?;
    Ok(Decl::Enum(EnumDecl { name, variants }))
}

fn parse_name(tokens: &mut TokenStream) -> Result<Name, ParseError> {
    eat_match!(tokens, Token::Identifier(name) => Ok(Name(name)))
}

fn parse_type(tokens: &mut TokenStream) -> Result<Type, ParseError> {
    let name = eat_match!(tokens, Token::Identifier(name) => name);
    match name.as_str() {
        "Int64" => Ok(Type::Int64),
        other => Ok(Type::Named(Name(other.to_string()))),
    }
}

fn parse_block(tokens: &mut TokenStream) -> Result<Vec<Expr>, ParseError> {
    tokens.eat(Token::BraceOpen)?;

    let mut exprs = Vec::new();
    while let Some(expr) = parse_expr_in_block(tokens)? {
        exprs.push(expr);
    }

    tokens.eat(Token::BraceClose)?;

    Ok(exprs)
}

fn parse_expr_in_block(tokens: &mut TokenStream) -> Result<Option<Expr>, ParseError> {
    match tokens.peek() {
        Some(Token::BraceClose) => Ok(None),
        _ => parse_expr(tokens).map(Some),
    }
}

fn parse_expr(tokens: &mut TokenStream) -> Result<Expr, ParseError> {
    let lhs = parse_term(tokens)?;
    parse_expr0(tokens, lhs)
}

fn parse_expr0(tokens: &mut TokenStream, lhs: Expr) -> Result<Expr, ParseError> {
    match tokens.peek() {
        Some(Token::Plus | Token::Dash) => parse_binop_term(tokens, lhs),
        _ => Ok(lhs),
    }
}

fn parse_term(tokens: &mut TokenStream) -> Result<Expr, ParseError> {
    let lhs = parse_factor(tokens)?;
    parse_term0(tokens, lhs)
}

fn parse_term0(tokens: &mut TokenStream, lhs: Expr) -> Result<Expr, ParseError> {
    match tokens.peek() {
        Some(Token::Star | Token::Slash) => parse_binop_factor(tokens, lhs),
        _ => Ok(lhs),
    }
}

fn parse_binop_term(tokens: &mut TokenStream, lhs: Expr) -> Result<Expr, ParseError> {
    if tokens.peek().is_none() {
        return Ok(lhs);
    }

    let op = eat_match!(
        tokens,
        Token::Plus => Op2::Add,
        Token::Dash => Op2::Sub,
    );

    let rhs = parse_factor(tokens)?;
    let expr = Expr::BinOp(BinOp {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    });

    parse_expr0(tokens, expr)
}

fn parse_factor(tokens: &mut TokenStream) -> Result<Expr, ParseError> {
    let lhs = parse_subfactor(tokens)?;
    parse_factor0(tokens, lhs)
}

fn parse_factor0(tokens: &mut TokenStream, lhs: Expr) -> Result<Expr, ParseError> {
    match tokens.peek() {
        Some(Token::Star | Token::Slash) => parse_binop_factor(tokens, lhs),
        _ => Ok(lhs),
    }
}

fn parse_binop_factor(tokens: &mut TokenStream, lhs: Expr) -> Result<Expr, ParseError> {
    if tokens.peek().is_none() {
        return Ok(lhs);
    }

    let op = eat_match!(
        tokens,
        Token::Star => Op2::Mul,
        Token::Slash => Op2::Div
    );

    let rhs = parse_subfactor(tokens)?;
    let expr = Expr::BinOp(BinOp {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    });

    parse_factor0(tokens, expr)
}

fn parse_subfactor(tokens: &mut TokenStream) -> Result<Expr, ParseError> {
    let lhs = parse_primary(tokens)?;
    parse_subfactor0(tokens, lhs)
}

fn parse_subfactor0(tokens: &mut TokenStream, lhs: Expr) -> Result<Expr, ParseError> {
    match tokens.peek() {
        Some(Token::Dot) => parse_field_access_or_method_call(tokens, lhs),
        _ => Ok(lhs),
    }
}

fn parse_primary(tokens: &mut TokenStream) -> Result<Expr, ParseError> {
    let expr = match tokens.peek() {
        Some(Token::Int64(_)) => parse_int64(tokens),
        // Some(Token::Float64(_)) => parse_float64(tokens),
        Some(Token::String(_)) => parse_string(tokens),
        Some(Token::Bool(_)) => parse_bool(tokens),
        Some(Token::Identifier(_)) => parse_var_or_fn_call_or_init(tokens),
        Some(Token::Dot) => parse_enum_init(tokens, None),
        Some(Token::Let) => parse_let(tokens),
        Some(Token::If) => parse_if(tokens),
        Some(Token::ParenOpen) => parse_unit_parens_or_tuple(tokens),
        Some(Token::Bang | Token::Dash) => parse_unary_op(tokens),
        Some(token) => Err(ParseError::UnexpectedToken(token.clone())),
        None => Err(ParseError::UnexpectedEof),
    }?;

    Ok(expr)
}

fn parse_unary_op(tokens: &mut TokenStream) -> Result<Expr, ParseError> {
    let op = eat_match!(
        tokens,
        Token::Bang => Op1::Not,
        Token::Dash => Op1::Neg,
    );

    let expr = parse_primary(tokens)?;
    Ok(Expr::UnaryOp(UnaryOp {
        op,
        expr: Box::new(expr),
    }))
}

fn parse_enum_init(tokens: &mut TokenStream, ty: Option<Name>) -> Result<Expr, ParseError> {
    tokens.eat(Token::Dot)?;

    let variant = parse_name(tokens)?;

    let arg = if let Some(Token::ParenOpen) = tokens.peek() {
        tokens.eat(Token::ParenOpen)?;

        let arg = parse_expr(tokens)?;

        tokens.eat(Token::ParenClose)?;

        Some(Box::new(arg))
    } else {
        None
    };

    Ok(Expr::EnumInit(EnumInit { ty, variant, arg }))
}

fn parse_struct_init(tokens: &mut TokenStream, ty: Name) -> Result<Expr, ParseError> {
    let args = parse_named_args(tokens)?;

    Ok(Expr::StructInit(StructInit { id: ty, args }))
}

fn parse_int64(tokens: &mut TokenStream) -> Result<Expr, ParseError> {
    eat_match!(tokens, Token::Int64(value) => Ok(Expr::Lit(Literal::Int64(value))))
}

// fn parse_float64(tokens: &mut TokenStream) -> Result<Expr, ParseError> {
//     eat_match!(tokens, Token::Float64(value) => Ok(Expr::Lit(Literal::Float64(value))))
// }

fn parse_bool(tokens: &mut TokenStream) -> Result<Expr, ParseError> {
    eat_match!(tokens, Token::Bool(value) => Ok(Expr::Lit(Literal::Bool(value))))
}

fn parse_string(tokens: &mut TokenStream) -> Result<Expr, ParseError> {
    eat_match!(tokens, Token::String(value) => Ok(Expr::Lit(Literal::String(value))))
}

fn parse_unit_parens_or_tuple(tokens: &mut TokenStream) -> Result<Expr, ParseError> {
    tokens.eat(Token::ParenOpen)?;

    let expr = if let Some(Token::ParenClose) = tokens.peek() {
        Expr::Lit(Literal::Unit)
    } else {
        let expr = parse_expr(tokens)?;

        if let Some(Token::Comma) = tokens.peek() {
            let mut items = vec![expr];

            while let Some(Token::Comma) = tokens.peek() {
                tokens.next();
                items.push(parse_expr(tokens)?);
            }

            Expr::Tuple(Tuple { items })
        } else {
            expr
        }
    };

    tokens.eat(Token::ParenClose)?;

    Ok(expr)
}

fn parse_var_or_fn_call_or_init(tokens: &mut TokenStream) -> Result<Expr, ParseError> {
    let name = parse_name(tokens)?;

    match tokens.peek() {
        Some(Token::ParenOpen) if name.is_camel_case() => parse_struct_init(tokens, name),
        Some(Token::ParenOpen) => parse_fn_call(tokens, name),
        Some(Token::Dot) if name.is_camel_case() => parse_enum_init(tokens, Some(name)),
        _ => Ok(Expr::Var(name)),
    }
}

fn parse_fn_call(tokens: &mut TokenStream, name: Name) -> Result<Expr, ParseError> {
    let args = parse_args(tokens)?;
    Ok(Expr::FnCall(FnCall { name, args }))
}

fn parse_field_access_or_method_call(
    tokens: &mut TokenStream,
    receiver: Expr,
) -> Result<Expr, ParseError> {
    tokens.eat(Token::Dot)?;

    let name = parse_name(tokens)?;

    let expr = if let Some(Token::ParenOpen) = tokens.peek() {
        let args = parse_args(tokens)?;
        Expr::MethodCall(MethodCall {
            receiver: Box::new(receiver),
            name,
            args,
        })
    } else {
        Expr::FieldAccess(FieldAccess {
            receiver: Box::new(receiver),
            name,
        })
    };

    parse_subfactor0(tokens, expr)
}

fn parse_let(tokens: &mut TokenStream) -> Result<Expr, ParseError> {
    tokens.eat(Token::Let)?;
    let name = parse_name(tokens)?;
    tokens.eat(Token::Equal)?;
    let value = Box::new(parse_expr(tokens)?);
    let body = Box::new(parse_expr(tokens)?);

    Ok(Expr::Let(Let { name, value, body }))
}

fn parse_if(tokens: &mut TokenStream) -> Result<Expr, ParseError> {
    tokens.eat(Token::If)?;
    let cnd = Box::new(parse_expr(tokens)?);
    let thn = Box::new(parse_expr(tokens)?); // parse_block(tokens)?;
    tokens.eat(Token::Else)?;
    let els = Box::new(parse_expr(tokens)?); // parse_block(tokens)?;

    Ok(Expr::If(If { cnd, thn, els }))
}

fn parse_args(tokens: &mut TokenStream) -> Result<Vec<Arg>, ParseError> {
    tokens.eat(Token::ParenOpen)?;

    let mut args = Vec::new();

    while let Some(arg) = parse_arg(tokens)? {
        args.push(arg);

        match tokens.peek() {
            Some(Token::Comma) => {
                tokens.next();
            }
            Some(Token::ParenClose) => break,
            Some(_) => continue,
            None => return Err(ParseError::UnexpectedEof),
        }
    }

    tokens.eat(Token::ParenClose)?;

    Ok(args)
}

fn parse_arg(tokens: &mut TokenStream) -> Result<Option<Arg>, ParseError> {
    if let Some(Token::ParenClose) = tokens.peek() {
        return Ok(None);
    }

    let expr = parse_expr(tokens)?;

    if tokens.peek() == Some(&Token::Colon) {
        if let Expr::Var(name) = expr {
            parse_named_arg_cont(tokens, name).map(|arg| Some(Arg::Named(arg)))
        } else {
            Err(ParseError::ExpectedIdentifier)
        }
    } else {
        Ok(Some(Arg::Anon(expr)))
    }
}

fn parse_named_args(tokens: &mut TokenStream) -> Result<Vec<NamedArg>, ParseError> {
    tokens.eat(Token::ParenOpen)?;

    let mut args = Vec::new();

    while let Some(arg) = parse_named_arg(tokens)? {
        args.push(arg);

        match tokens.peek() {
            Some(Token::Comma) => {
                tokens.next();
            }
            Some(Token::ParenClose) => break,
            Some(_) => continue,
            None => return Err(ParseError::UnexpectedEof),
        }
    }

    tokens.eat(Token::ParenClose)?;

    Ok(args)
}

fn parse_named_arg(tokens: &mut TokenStream) -> Result<Option<NamedArg>, ParseError> {
    if let Some(Token::ParenClose) = tokens.peek() {
        return Ok(None);
    }

    let name = parse_name(tokens)?;
    let arg = parse_named_arg_cont(tokens, name)?;

    Ok(Some(arg))
}

fn parse_named_arg_cont(tokens: &mut TokenStream, name: Name) -> Result<NamedArg, ParseError> {
    tokens.eat(Token::Colon)?;
    let value = parse_expr(tokens)?;
    let arg = NamedArg { name, value };
    Ok(arg)
}

fn parse_params(tokens: &mut TokenStream) -> Result<Vec<Param>, ParseError> {
    tokens.eat(Token::ParenOpen)?;

    let mut params = Vec::new();
    while let Some(param) = parse_param(tokens)? {
        params.push(param);

        if tokens.peek() == Some(&Token::Comma) {
            tokens.next();
        }
    }

    tokens.eat(Token::ParenClose)?;

    Ok(params)
}

fn parse_param(tokens: &mut TokenStream) -> Result<Option<Param>, ParseError> {
    if let Some(Token::ParenClose) = tokens.peek() {
        return Ok(None);
    }

    let anon = if let Some(Token::Under) = tokens.peek() {
        tokens.next();
        true
    } else {
        false
    };

    let name = parse_name(tokens)?;
    tokens.eat(Token::Colon)?;
    let ty = parse_type(tokens)?;
    let param = Param { name, ty, anon };
    Ok(Some(param))
}

fn parse_fields(tokens: &mut TokenStream) -> Result<Vec<Param>, ParseError> {
    let mut params = Vec::new();

    while let Some(param) = parse_field(tokens)? {
        params.push(param);

        if tokens.peek() == Some(&Token::Comma) {
            tokens.next();
        }
    }

    Ok(params)
}

fn parse_field(tokens: &mut TokenStream) -> Result<Option<Param>, ParseError> {
    if let Some(Token::BraceClose) = tokens.peek() {
        return Ok(None);
    }

    let name = parse_name(tokens)?;
    tokens.eat(Token::Colon)?;
    let ty = parse_type(tokens)?;
    let param = Param {
        name,
        ty,
        anon: false,
    };
    Ok(Some(param))
}

fn parse_variants(tokens: &mut TokenStream) -> Result<Vec<Variant>, ParseError> {
    let mut variants = Vec::new();
    while let Some(variant) = parse_variant(tokens)? {
        variants.push(variant);

        if tokens.peek() == Some(&Token::Comma) {
            tokens.next();
        }
    }

    Ok(variants)
}

fn parse_variant(tokens: &mut TokenStream) -> Result<Option<Variant>, ParseError> {
    match tokens.peek() {
        Some(Token::BraceClose) => Ok(None),
        _ => {
            tokens.eat(Token::Dot)?;
            let name = parse_name(tokens)?;
            tokens.eat(Token::ParenOpen)?;
            let ty = parse_type(tokens)?;
            tokens.eat(Token::ParenClose)?;
            let variant = Variant { name, ty };
            Ok(Some(variant))
        }
    }
}

trait TokenStreamExt {
    fn eat(&mut self, token: Token) -> Result<(), ParseError>;

    fn eat_if(&mut self, cond: impl FnOnce(&Token) -> bool) -> Result<Token, ParseError>;
}

impl TokenStreamExt for TokenStream {
    fn eat(&mut self, token: Token) -> Result<(), ParseError> {
        match self.next() {
            Some(t) if t == token => Ok(()),
            Some(t) => Err(ParseError::ExpectedToken(token, t)),
            None => Err(ParseError::UnexpectedEof),
        }
    }

    fn eat_if(&mut self, cond: impl FnOnce(&Token) -> bool) -> Result<Token, ParseError> {
        match self.next() {
            Some(token) if cond(&token) => Ok(token),
            Some(token) => Err(ParseError::UnexpectedToken(token)),
            None => Err(ParseError::UnexpectedEof),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::path::Path;

    fn parse_file(path: impl AsRef<Path>) -> Result<Module, ParseError> {
        let contents = std::fs::read_to_string(path).unwrap();
        parse(&contents)
    }

    #[test]
    fn parse_examples() {
        let examples = std::fs::read_dir("examples").unwrap();
        for example in examples {
            let example = example.unwrap();
            let path = example.path();
            if path.extension().unwrap() == "self" {
                println!("Parsing {:?}", path);
                match parse_file(path) {
                    Ok(module) => println!("{module:#?}"),
                    Err(e) => panic!("{e}"),
                }
            }
        }
    }

    #[test]
    fn parse_arith() {
        let mut tokens = lex(r#" x + 2 * y - 3 / 4 + z"#)
            .unwrap()
            .into_iter()
            .peekable();
        let expr = parse_expr(&mut tokens).unwrap();
        dbg!(&tokens);
        println!("{:#?}", expr);
    }
}
