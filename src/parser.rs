//! A nom parser for the following language:
//!
//! ```
//! fn add(_ x: Int64, y: Int64) -> Int64 {
//!   let z = x + y;
//!   z
//! }
//!
//! fn main() {
//!   add(42, y: 123)
//! }
//! ```

macro_rules! eat_match {
    ($tokens:ident, $token:pat => $expr:expr) => {
        match $tokens.next() {
            Some($token) => $expr,
            Some(token) => return Err(ParseError::UnexpectedToken(token)),
            None => return Err(ParseError::UnexpectedEof),
        }
    };
}

use core::fmt;

use crate::ast::{FnCall, FnDecl, If, Let, *};
use crate::lexer::{lex, Token};

#[derive(Debug)]
pub enum ParseError {
    ExpectedToken(Token, Token),
    UnexpectedToken(Token),
    UnexpectedEof,
    Lex(String),
    ExpectedIdentifier,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::Lex(msg) => write!(f, "Lex error: {}", msg),
            ParseError::ExpectedToken(expected, got) => {
                write!(f, "Expected token {expected:?}, instead got {got:?}")
            }
            ParseError::UnexpectedToken(token) => write!(f, "Unexpected token: {:?}", token),
            ParseError::UnexpectedEof => write!(f, "Unexpected end of file"),
            ParseError::ExpectedIdentifier => write!(f, "Expected identifier"),
        }
    }
}

type TokenStream = std::iter::Peekable<std::vec::IntoIter<Token>>;

pub fn parse(contents: &str) -> Result<Module, ParseError> {
    let tokens = lex(contents).map_err(|e| ParseError::Lex(e.to_string()))?;
    let decls = parse_decls(tokens)?;
    Ok(Module { decls })
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
    while let Some(expr) = parse_expr(tokens)? {
        exprs.push(expr);
    }
    tokens.eat(Token::BraceClose)?;
    Ok(exprs)
}

fn parse_expr(tokens: &mut TokenStream) -> Result<Option<Expr>, ParseError> {
    match tokens.peek() {
        Some(Token::BraceClose) => Ok(None),
        _ => parse_expr_inner(tokens).map(Some),
    }
}

fn parse_expr_inner(tokens: &mut TokenStream) -> Result<Expr, ParseError> {
    match tokens.peek() {
        Some(Token::Int64(_)) => parse_int64(tokens),
        Some(Token::Float64(_)) => parse_float64(tokens),
        Some(Token::String(_)) => parse_string(tokens),
        Some(Token::Identifier(_)) => parse_call_or_var(tokens),
        Some(Token::Let) => parse_let(tokens),
        Some(Token::If) => parse_if(tokens),
        Some(token) => Err(ParseError::UnexpectedToken(token.clone())),
        None => Err(ParseError::UnexpectedEof),
    }
}

fn parse_int64(tokens: &mut TokenStream) -> Result<Expr, ParseError> {
    eat_match!(tokens, Token::Int64(value) => Ok(Expr::Int64(value)))
}

fn parse_float64(tokens: &mut TokenStream) -> Result<Expr, ParseError> {
    eat_match!(tokens, Token::Float64(value) => Ok(Expr::Float64(value)))
}

fn parse_string(tokens: &mut TokenStream) -> Result<Expr, ParseError> {
    eat_match!(tokens, Token::String(value) => Ok(Expr::String(value)))
}

fn parse_call_or_var(tokens: &mut TokenStream) -> Result<Expr, ParseError> {
    let name = parse_name(tokens)?;
    match tokens.peek() {
        Some(Token::ParenOpen) => parse_call(tokens, name),
        _ => Ok(Expr::Var(name)),
    }
}

fn parse_call(tokens: &mut TokenStream, name: Name) -> Result<Expr, ParseError> {
    let args = parse_args(tokens)?;
    Ok(Expr::FnCall(FnCall { name, args }))
}

fn parse_let(tokens: &mut TokenStream) -> Result<Expr, ParseError> {
    tokens.eat(Token::Let)?;
    let name = parse_name(tokens)?;
    tokens.eat(Token::Equals)?;
    let value = Box::new(parse_expr_inner(tokens)?);
    let body = Box::new(parse_expr_inner(tokens)?);

    Ok(Expr::Let(Let { name, value, body }))
}

fn parse_if(tokens: &mut TokenStream) -> Result<Expr, ParseError> {
    tokens.eat(Token::If)?;
    let cnd = Box::new(parse_expr_inner(tokens)?);
    let thn = parse_block(tokens)?;
    tokens.eat(Token::Else)?;
    let els = parse_block(tokens)?;

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
    match tokens.peek() {
        Some(Token::ParenClose) => Ok(None),
        _ => {
            let expr = parse_expr(tokens)?;

            if tokens.peek() == Some(&Token::Colon) {
                if let Some(Expr::Var(name)) = expr {
                    parse_named_arg(tokens, name)
                } else {
                    Err(ParseError::ExpectedIdentifier)
                }
            } else {
                Ok(expr.map(Arg::Anon))
            }
        }
    }
}

fn parse_named_arg(tokens: &mut TokenStream, name: Name) -> Result<Option<Arg>, ParseError> {
    tokens.eat(Token::Colon)?;
    let value = parse_expr_inner(tokens)?;
    let arg = Arg::Named(NamedArg { name, value });
    Ok(Some(arg))
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
    match tokens.peek() {
        Some(Token::ParenClose) => Ok(None),
        _ => {
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
    }
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
    match tokens.peek() {
        Some(Token::BraceClose) => Ok(None),
        _ => {
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
    }
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
                    Err(e) => println!("{e}"),
                }
            }
        }
    }
}
