#![feature(trait_alias)]

use std::path::Path;

use chumsky::input::{Input, SpannedInput, Stream};
use chumsky::primitive::{choice, end, just};
use chumsky::recursive::recursive;
use chumsky::span::SimpleSpan;
use chumsky::{extra, select, IterParser};

use selfie_ast::*;
use selfie_lexer::{lex, Token};

mod error;
pub use error::Error;

pub type Span = SimpleSpan<usize>;
pub type Spanned<T> = (T, Span);
pub type ParserInput = SpannedInput<Token, Span, Stream<std::vec::IntoIter<(Token, Span)>>>;

pub fn parse_file(path: impl AsRef<Path>) -> Result<Module, Vec<Error>> {
    let contents = std::fs::read_to_string(path).unwrap();
    parse_module(&contents)
}

fn text_to_input(text: &str) -> Result<ParserInput, Vec<Error>> {
    let tokens = lex(text).map_err(|e| vec![Error::Lex(e)])?;
    let len = tokens.len();
    let tokens: Vec<_> = tokens.into_iter().map(|(t, s)| (t, s.into())).collect();
    Ok(Stream::from_iter(tokens).spanned((len..len).into()))
}

pub fn parse_module(contents: &str) -> Result<Module, Vec<Error>> {
    let input = text_to_input(contents)?;

    let decls = parse_decl()
        .repeated()
        .collect::<Vec<_>>()
        .then_ignore(end())
        .parse(input)
        .into_result()?;

    Ok(Module { decls })
}

pub trait Parser<A> = chumsky::Parser<'static, ParserInput, A, extra::Err<Error>> + Clone;

pub fn parse_identifier() -> impl Parser<Name> {
    select!(Token::Identifier(id) => Name::interned(id))
}

pub fn parse_upper() -> impl Parser<Name> {
    parse_identifier().filter(|id| id.as_str().starts_with(char::is_uppercase))
}

pub fn parse_lower() -> impl Parser<Name> {
    parse_identifier().filter(|id| {
        id.as_str()
            .starts_with(|c: char| c.is_lowercase() || c == '_')
    })
}

pub fn parse_type() -> impl Parser<Type> {
    fn to_type(id: Name) -> Type {
        match id.as_str() {
            "String" => Type::String,
            "Bool" => Type::Bool,
            "Unit" => Type::Unit,
            "Int64" => Type::Int64,
            "Float64" => Type::Float64,
            _ => Type::Named(id),
        }
    }

    recursive(|ty| {
        let tys = ty
            .separated_by(just(Token::Comma))
            .at_least(1)
            .allow_trailing()
            .collect::<Vec<_>>();

        choice((parens(tys).map(Type::Tuple), parse_upper().map(to_type)))
    })
}

pub fn parse_decl() -> impl Parser<Decl> {
    choice((
        parse_fn().map(Decl::Fn),
        parse_struct().map(Decl::Struct),
        parse_enum().map(Decl::Enum),
    ))
}

pub fn parse_struct() -> impl Parser<StructDecl> {
    let fields = parse_field().repeated().collect::<Vec<_>>();

    just(Token::Struct)
        .ignore_then(parse_upper())
        .then(braces(fields))
        .map(|(name, fields)| StructDecl { name, fields })
}

pub fn parse_field() -> impl Parser<Field> {
    parse_lower()
        .then_ignore(just(Token::Colon))
        .then(parse_type())
        .map(|(name, ty)| Field { name, ty })
}

pub fn parse_enum() -> impl Parser<EnumDecl> {
    let variants = parse_variant().repeated().collect::<Vec<_>>();

    just(Token::Enum)
        .ignore_then(parse_upper())
        .then(braces(variants))
        .map(|(name, variants)| EnumDecl { name, variants })
}

pub fn parse_variant() -> impl Parser<Variant> {
    just(Token::Dot)
        .ignore_then(parse_lower())
        .then(parens(parse_type()).or_not())
        .map(|(name, ty)| Variant { name, ty })
}

fn parse_fn() -> impl Parser<FnDecl> {
    let params = parse_param()
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>();

    just(Token::Fn)
        .ignore_then(parse_lower())
        .then(parens(params))
        .then_ignore(just(Token::Colon))
        .then(parse_type())
        .then(braces(parse_expr()))
        .map(|(((name, params), return_type), body)| FnDecl {
            name,
            params,
            return_type,
            body,
        })
}

pub fn parse_param() -> impl Parser<Param> {
    parse_param_kind()
        .then_ignore(just(Token::Colon))
        .then(parse_type())
        .map(|((kind, name), ty)| Param { name, ty, kind })
}

pub fn parse_param_kind() -> impl Parser<(ParamKind, Name)> {
    // _ foo: Int
    let anon = just(Token::Under)
        .ignore_then(parse_lower())
        .map(|name| (ParamKind::Anon, name));

    // bar foo: Int
    let alias = parse_lower().map(ParamKind::Alias).then(parse_lower());

    // foo: Int
    let normal = parse_lower().map(|name| (ParamKind::Normal, name));

    choice((anon, alias, normal))
}

pub fn parse_expr() -> impl Parser<Expr> {
    recursive(|expr| {
        choice((
            parse_lit().map(Expr::Lit),
            parse_field_access_or_method_call(expr.clone()),
            parse_fn_call(expr.clone()).map(Expr::FnCall),
            parse_struct_init(expr.clone()).map(Expr::StructInit),
            parse_enum_init(expr.clone()).map(Expr::EnumInit),
            parse_let(expr.clone()).map(Expr::Let),
            parse_tuple(expr.clone()).map(Expr::Tuple),
            parse_identifier().map(Expr::Var),
        ))
    })
}

pub fn parse_lit() -> impl Parser<Literal> {
    let other = select! {
        Token::String(s) => Literal::String(s),
        Token::Float64(f) => Literal::Float64(f),
        Token::Int64(i) => Literal::Int64(i),
        Token::Bool(b) => Literal::Bool(b),
    };

    let unit = just(Token::ParenOpen)
        .then(just(Token::ParenClose))
        .to(Literal::Unit);

    choice((other, unit))
}

pub fn parse_field_access_or_method_call(expr: impl Parser<Expr>) -> impl Parser<Expr> {
    enum MF {
        FieldAccess(Name),
        MethodCall(Name, Vec<Arg>),
    }

    let mf = choice((
        parse_fn_call(expr.clone()).map(|FnCall { name, args }| MF::MethodCall(name, args)),
        parse_lower().map(MF::FieldAccess),
    ));

    // FIXME: Avoid left-recursion
    expr.memoized().foldl(
        just(Token::Dot).ignore_then(mf).repeated().at_least(1),
        |expr, mf| match mf {
            MF::FieldAccess(name) => Expr::FieldAccess(FieldAccess {
                expr: Box::new(expr),
                name,
            }),
            MF::MethodCall(name, args) => Expr::MethodCall(MethodCall {
                expr: Box::new(expr),
                name,
                args,
            }),
        },
    )
}

pub fn parse_fn_call(expr: impl Parser<Expr>) -> impl Parser<FnCall> {
    parse_lower()
        .then(parens(parse_args(expr)))
        .map(|(name, args)| FnCall { name, args })
}

pub fn parse_let(expr: impl Parser<Expr>) -> impl Parser<Let> {
    just(Token::Let)
        .ignore_then(parse_lower())
        .then_ignore(just(Token::Equal))
        .then(expr.clone())
        .then(expr.clone())
        .map(|((name, value), body)| Let {
            name,
            value: Box::new(value),
            body: Box::new(body),
        })
}

fn parse_tuple(expr: impl Parser<Expr>) -> impl Parser<Tuple> {
    let items = expr
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>();

    parens(items).map(|items| Tuple { items })
}

pub fn parse_struct_init(expr: impl Parser<Expr>) -> impl Parser<StructInit> {
    let args = parse_named_arg(expr)
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>();

    parse_upper()
        .then(parens(args))
        .map(|(id, args)| StructInit { id, args })
}

pub fn parse_args(expr: impl Parser<Expr>) -> impl Parser<Vec<Arg>> {
    parse_arg(expr)
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
}

pub fn parse_arg(expr: impl Parser<Expr>) -> impl Parser<Arg> {
    let parse_named = parse_named_arg(expr.clone()).map(Arg::Named);
    let parse_anon = expr.map(Arg::Anon);
    choice((parse_named, parse_anon))
}

pub fn parse_named_arg(expr: impl Parser<Expr>) -> impl Parser<NamedArg> {
    parse_identifier()
        .then_ignore(just(Token::Colon))
        .then(expr)
        .map(|(name, value)| NamedArg { name, value })
}

pub fn parse_enum_init(expr: impl Parser<Expr>) -> impl Parser<EnumInit> {
    parse_upper()
        .or_not()
        .then_ignore(just(Token::Dot))
        .then(parse_lower())
        .then(parens(expr.map(Box::new)).or_not())
        .map(|((ty, variant), arg)| EnumInit { ty, variant, arg })
}

fn braces<A>(p: impl Parser<A>) -> impl Parser<A> {
    p.delimited_by(just(Token::BraceOpen), just(Token::BraceClose))
}

fn parens<A>(p: impl Parser<A>) -> impl Parser<A> {
    p.delimited_by(just(Token::ParenOpen), just(Token::ParenClose))
}

#[cfg(test)]
#[allow(unused_variables)]
mod tests {
    use super::*;

    #[test]
    fn parse_examples() {
        let examples = std::fs::read_dir("../examples").unwrap();
        for example in examples {
            let example = example.unwrap();
            let path = example.path();
            if path.extension().unwrap() == "self" {
                println!("Parsing {:?}", path);

                match parse_file(path) {
                    Ok(module) => println!("{module:#?}"),
                    Err(es) => {
                        for e in es {
                            eprintln!("{e}");
                        }
                        panic!("one or more error occured");
                    }
                }
            }
        }
    }

    #[test]
    fn parse_braces() {
        let input = text_to_input(r#" { hello_world }"#).unwrap();
        let expr = braces(parse_identifier()).parse(input).unwrap();
    }

    #[test]
    fn parse_sep_by() {
        let input = text_to_input(r#"foo, bar"#).unwrap();
        let expr = parse_identifier()
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .parse(input)
            .unwrap();

        let input = text_to_input(r#"foo, bar,"#).unwrap();
        let expr = parse_identifier()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .parse(input)
            .unwrap();

        let input = text_to_input(r#" { foo, bar, }"#).unwrap();
        let expr = braces(
            parse_identifier()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .parse(input)
        .unwrap();

        let input = text_to_input(r#" { foo: Int64, bar: String, }"#).unwrap();
        let expr = braces(
            parse_field()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .parse(input)
        .unwrap();
    }

    // #[test]
    // fn parse_arith() {
    //     let mut tokens = lex(r#" x + 2 * y - 3 / 4 + z"#)
    //         .unwrap()
    //         .into_iter()
    //         .peekable();
    //     let expr = parse_expr(&mut tokens).unwrap();
    //     dbg!(&tokens);
    //     println!("{:#?}", expr);
    // }
}
