#![feature(trait_alias)]

use std::path::Path;

use chumsky::input::{Input, SpannedInput, Stream};
use chumsky::primitive::{choice, end, just};
use chumsky::recursive::recursive;
use chumsky::{extra, select, IterParser};

use selfie_ast::*;
use selfie_lexer::{lex, Token};

mod error;
pub use error::Error;

pub type ParserInput = SpannedInput<Token, Span, Stream<std::vec::IntoIter<(Token, Span)>>>;
pub trait Parser<A> = chumsky::Parser<'static, ParserInput, A, extra::Err<Error>> + Clone;

pub fn parse_file(path: impl AsRef<Path>) -> Result<Module, Vec<Error>> {
    let path = path.as_ref();
    let contents = std::fs::read_to_string(path).unwrap();

    parse_module(&contents, path)
}

pub fn parse_module(contents: &str, path: &Path) -> Result<Module, Vec<Error>> {
    let path = Ustr::from(&path.display().to_string());
    let input = text_to_input(contents, path)?;

    let decls = parse_decl().repeated().collect::<Vec<_>>();

    let parser = just(Token::Module)
        .ignore_then(parse_upper())
        .then(decls)
        .then_ignore(end())
        .map_with(|(name, decls), meta| Module {
            name,
            decls,
            span: meta.span(),
        });

    let module = parser.parse(input).into_result()?;
    Ok(module)
}

fn span(path: Ustr, range: std::ops::Range<usize>) -> Span {
    <Span as chumsky::span::Span>::new(path, range)
}

fn text_to_input(text: &str, path: Ustr) -> Result<ParserInput, Vec<Error>> {
    let tokens = lex(text).map_err(|e| vec![Error::Lex(e)])?;
    let count = tokens.len();

    let tokens: Vec<_> = tokens
        .into_iter()
        .map(|(token, range)| (token, span(path, range)))
        .collect();

    Ok(Stream::from_iter(tokens).spanned(span(path, count..count)))
}

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
            .at_least(2)
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
        .map_with(|(name, fields), meta| StructDecl {
            name,
            fields,
            span: meta.span(),
        })
}

pub fn parse_field() -> impl Parser<Field> {
    parse_lower()
        .then_ignore(just(Token::Colon))
        .then(parse_type())
        .map_with(|(name, ty), meta| Field {
            name,
            ty,
            span: meta.span(),
        })
}

pub fn parse_enum() -> impl Parser<EnumDecl> {
    let variants = parse_variant().repeated().collect::<Vec<_>>();

    just(Token::Enum)
        .ignore_then(parse_upper())
        .then(braces(variants))
        .map_with(|(name, variants), meta| EnumDecl {
            name,
            variants,
            span: meta.span(),
        })
}

pub fn parse_variant() -> impl Parser<Variant> {
    just(Token::Dot)
        .ignore_then(parse_lower())
        .then(parens(parse_type()).or_not())
        .map_with(|(name, ty), meta| Variant {
            name,
            ty,
            span: meta.span(),
        })
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
        .map_with(|(((name, params), return_type), body), meta| FnDecl {
            name,
            params,
            return_type,
            body,
            span: meta.span(),
        })
}

pub fn parse_param() -> impl Parser<Param> {
    parse_param_kind()
        .then_ignore(just(Token::Colon))
        .then(parse_type())
        .map_with(|((kind, name), ty), meta| Param {
            name,
            kind,
            ty,
            span: meta.span(),
        })
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
    use chumsky::pratt::*;
    use chumsky::span::Span as _;

    let op1 = |op| {
        move |e: Expr| {
            Expr::UnaryOp(UnaryOp {
                span: e.span(),
                op,
                expr: Box::new(e),
            })
        }
    };

    let op2 = |op| {
        move |l: Expr, r: Expr| {
            Expr::BinaryOp(BinaryOp {
                span: l.span().union(r.span()),
                op,
                lhs: Box::new(l),
                rhs: Box::new(r),
            })
        }
    };

    recursive(|expr| {
        parse_atom(expr.clone()).pratt((
            // - !
            prefix(10, just(Token::Dash), op1(Op1::Neg)),
            prefix(10, just(Token::Bang), op1(Op1::Not)),
            // * / %
            infix(left(9), just(Token::Star), op2(Op2::Mul)),
            infix(left(9), just(Token::Slash), op2(Op2::Div)),
            infix(left(9), just(Token::Percent), op2(Op2::Mod)),
            // + -
            infix(left(8), just(Token::Plus), op2(Op2::Add)),
            infix(left(8), just(Token::Dash), op2(Op2::Sub)),
            // >> <<
            // infix(left(7), just(Token::ShiftLeft), op2(Op2::Shl)),
            // infix(left(7), just(Token::ShiftRight), op2(Op2::Shr)),
            // < <= > >=
            infix(left(6), just(Token::Less), op2(Op2::Lt)),
            infix(left(6), just(Token::LessEqual), op2(Op2::Leq)),
            infix(left(6), just(Token::Greater), op2(Op2::Gt)),
            infix(left(6), just(Token::GreaterEqual), op2(Op2::Geq)),
            // == !=
            infix(left(6), just(Token::EqualEqual), op2(Op2::Eq)),
            infix(left(6), just(Token::BangEqual), op2(Op2::Neq)),
            // &
            // infix(left(5), just(Token::And), op2(Op2::And)),
            // ^
            // infix(left(4), just(Token::Caret), op2(Op2::Xor)),
            // |
            // infix(left(3), just(Token::Or), op2(Op2::Or)),
            // &&
            infix(left(2), just(Token::AndAnd), op2(Op2::And)),
            // ||
            infix(left(1), just(Token::OrOr), op2(Op2::Or)),
        ))
    })
}

pub fn parse_var() -> impl Parser<Var> {
    parse_identifier().map_with(|name, meta| Var {
        name,
        span: meta.span(),
    })
}

pub fn parse_atom(expr: impl Parser<Expr>) -> impl Parser<Expr> {
    let init = choice((
        parse_lit().map(Expr::Lit),
        parse_fn_call(expr.clone()).map(Expr::FnCall),
        parse_struct_init(expr.clone()).map(Expr::StructInit),
        parse_enum_init(expr.clone()).map(Expr::EnumInit),
        parse_let(expr.clone()).map(Expr::Let),
        parse_tuple(expr.clone()).map(Expr::Tuple),
        parens(expr.clone()),
        parse_var().map(Expr::Var),
    ));

    parse_field_access_or_method_call(init, expr)
}

pub fn parse_lit() -> impl Parser<Literal> {
    let other = select! {
        Token::String(s) = meta => Literal::String(meta.span(), s),
        Token::Float64(f) = meta => Literal::Float64(meta.span(), f),
        Token::Int64(i) = meta => Literal::Int64(meta.span(), i),
        Token::Bool(b) = meta => Literal::Bool(meta.span(), b),
    };

    let unit = just(Token::ParenOpen)
        .then(just(Token::ParenClose))
        .map_with(|_, meta| Literal::Unit(meta.span()));

    choice((other, unit))
}

pub fn parse_field_access_or_method_call(
    init: impl Parser<Expr>,
    expr: impl Parser<Expr>,
) -> impl Parser<Expr> {
    use chumsky::span::Span as _;

    enum MF {
        FieldAccess(Name, Span),
        MethodCall(Name, Vec<Arg>, Span),
    }

    let mf = choice((
        parse_fn_call(expr.clone())
            .map_with(|call, meta| MF::MethodCall(call.name, call.args, meta.span())),
        parse_lower().map_with(|name, meta| MF::FieldAccess(name, meta.span())),
    ));

    init.foldl(
        just(Token::Dot).ignore_then(mf).repeated(),
        |expr, mf| match mf {
            MF::FieldAccess(name, span) => Expr::FieldAccess(FieldAccess {
                span: expr.span().union(span),
                expr: Box::new(expr),
                name,
            }),
            MF::MethodCall(name, args, span) => Expr::MethodCall(MethodCall {
                span: expr.span().union(span),
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
        .map_with(|(name, args), meta| FnCall {
            name,
            args,
            span: meta.span(),
        })
}

pub fn parse_let(expr: impl Parser<Expr>) -> impl Parser<Let> {
    just(Token::Let)
        .ignore_then(parse_lower())
        .then_ignore(just(Token::Equal))
        .then(expr.clone())
        .then(expr.clone())
        .map_with(|((name, value), body), meta| Let {
            name,
            value: Box::new(value),
            body: Box::new(body),
            span: meta.span(),
        })
}

fn parse_tuple(expr: impl Parser<Expr>) -> impl Parser<Tuple> {
    let items = expr
        .separated_by(just(Token::Comma))
        .at_least(2)
        .allow_trailing()
        .collect::<Vec<_>>();

    parens(items).map_with(|items, meta| Tuple {
        items,
        span: meta.span(),
    })
}

pub fn parse_struct_init(expr: impl Parser<Expr>) -> impl Parser<StructInit> {
    let args = parse_named_arg(expr)
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>();

    parse_upper()
        .then(parens(args))
        .map_with(|(id, args), meta| StructInit {
            id,
            args,
            span: meta.span(),
        })
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
        .map_with(|(name, value), meta| NamedArg {
            name,
            value,
            span: meta.span(),
        })
}

pub fn parse_enum_init(expr: impl Parser<Expr>) -> impl Parser<EnumInit> {
    parse_upper()
        .or_not()
        .then_ignore(just(Token::Dot))
        .then(parse_lower())
        .then(parens(expr.map(Box::new)).or_not())
        .map_with(|((ty, variant), arg), meta| EnumInit {
            ty,
            variant,
            arg,
            span: meta.span(),
        })
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
        let input = text_to_input(r#" { hello_world }"#, Ustr::from(file!())).unwrap();
        let expr = braces(parse_identifier()).parse(input).unwrap();
    }

    #[test]
    fn parse_sep_by() {
        let input = text_to_input(r#"foo, bar"#, Ustr::from(file!())).unwrap();
        let expr = parse_identifier()
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .parse(input)
            .unwrap();

        let input = text_to_input(r#"foo, bar,"#, Ustr::from(file!())).unwrap();
        let expr = parse_identifier()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .parse(input)
            .unwrap();

        let input = text_to_input(r#" { foo, bar, }"#, Ustr::from(file!())).unwrap();
        let expr = braces(
            parse_identifier()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .parse(input)
        .unwrap();

        let input = text_to_input(r#" { foo: Int64, bar: String, }"#, Ustr::from(file!())).unwrap();
        let expr = braces(
            parse_field()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .parse(input)
        .unwrap();
    }
}
