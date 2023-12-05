#![feature(trait_alias)]

use std::path::Path;

use chumsky::input::{Input, SpannedInput, Stream};
use chumsky::primitive::{choice, end, just};
use chumsky::recursive::recursive;
use chumsky::{extra, select, IterParser};

use selfie_ast::*;
use selfie_lexer::Token;

mod error;
pub use error::Error;

pub type ParserInput = SpannedInput<Token, Span, Stream<std::vec::IntoIter<(Token, Span)>>>;
pub trait Parser<A> = chumsky::Parser<'static, ParserInput, A, extra::Err<Error>> + Clone;

pub type Tokens = Vec<(Token, selfie_lexer::Span)>;

pub fn parse_module(tokens: Tokens, path: &Path) -> Result<Module, Vec<Error>> {
    let path = Ustr::from(&path.display().to_string());
    let input = tokens_to_input(tokens, path)?;

    let decls = parse_decl().repeated().collect::<Vec<_>>();

    let parser = just(Token::Module)
        .ignore_then(parse_upper())
        .then(decls)
        .then_ignore(end())
        .map_with(|(sym, decls), meta| Module {
            sym,
            decls,
            span: meta.span(),
        });

    let module = parser.parse(input).into_result()?;
    Ok(module)
}

fn span(path: Ustr, range: std::ops::Range<usize>) -> Span {
    <Span as chumsky::span::Span>::new(path, range)
}

fn tokens_to_input(tokens: Tokens, path: Ustr) -> Result<ParserInput, Vec<Error>> {
    let count = tokens.len();

    let tokens: Vec<_> = tokens
        .into_iter()
        .map(|(token, range)| (token, span(path, range)))
        .collect();

    Ok(Stream::from_iter(tokens).spanned(span(path, count..count)))
}

pub fn parse_identifier() -> impl Parser<Sym> {
    select!(Token::Identifier(id) => Sym::new(Name::interned(id)))
}

pub fn parse_upper() -> impl Parser<Sym> {
    parse_identifier().filter(|id| id.as_str().starts_with(char::is_uppercase))
}

pub fn parse_lower() -> impl Parser<Sym> {
    parse_identifier().filter(|id| {
        id.as_str()
            .starts_with(|c: char| c.is_lowercase() || c == '_')
    })
}

pub fn parse_type() -> impl Parser<Type> {
    fn to_type(sym: Sym, span: Span) -> Type {
        match sym.as_str() {
            "String" => Type::String(span),
            "Bool" => Type::Bool(span),
            "Unit" => Type::Unit(span),
            "Int64" => Type::Int64(span),
            "Float64" => Type::Float64(span),
            _ => Type::Named(span, sym),
        }
    }

    recursive(|ty| {
        let tys = ty
            .separated_by(just(Token::Comma))
            .at_least(2)
            .allow_trailing()
            .collect::<Vec<_>>();

        choice((
            parens(tys).map_with(|tys, meta| Type::Tuple(meta.span(), tys)),
            parse_upper().map_with(|sym, meta| to_type(sym, meta.span())),
        ))
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
        .map_with(|(sym, fields), meta| StructDecl {
            sym,
            fields,
            span: meta.span(),
        })
}

pub fn parse_field() -> impl Parser<Field> {
    parse_lower()
        .then_ignore(just(Token::Colon))
        .then(parse_type())
        .map_with(|(sym, ty), meta| Field {
            sym,
            ty,
            span: meta.span(),
        })
}

pub fn parse_enum() -> impl Parser<EnumDecl> {
    let variants = parse_variant()
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>();

    just(Token::Enum)
        .ignore_then(parse_upper())
        .then(braces(variants))
        .map_with(|(sym, variants), meta| EnumDecl {
            sym,
            variants,
            span: meta.span(),
        })
}

pub fn parse_variant() -> impl Parser<Variant> {
    just(Token::Dot)
        .ignore_then(parse_lower())
        .then(parens(parse_type()).or_not())
        .map_with(|(sym, ty), meta| Variant {
            sym,
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
        .map_with(|(((sym, params), return_type), body), meta| FnDecl {
            sym,
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
        .map_with(|((kind, sym), ty), meta| Param {
            sym,
            kind,
            ty,
            span: meta.span(),
        })
}

pub fn parse_param_kind() -> impl Parser<(ParamKind, Sym)> {
    // _ foo: Int
    let anon = just(Token::Under)
        .ignore_then(parse_lower())
        .map(|sym| (ParamKind::Anon, sym));

    // bar foo: Int
    let alias = parse_lower().map(ParamKind::Alias).then(parse_lower());

    // foo: Int
    let normal = parse_lower().map(|sym| (ParamKind::Normal, sym));

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
    parse_lower().map_with(|sym, meta| Var {
        sym,
        span: meta.span(),
    })
}

pub fn parse_atom(expr: impl Parser<Expr>) -> impl Parser<Expr> {
    let init = choice((
        parse_lit().map(Expr::Lit),
        parse_fn_call(expr.clone()).map(Expr::FnCall),
        parse_struct_init(expr.clone()).map(Expr::StructInit),
        parse_enum_init(expr.clone()).map(Expr::EnumInit),
        parse_if(expr.clone()).map(Expr::If),
        parse_let(expr.clone()).map(Expr::Let),
        parse_tuple(expr.clone()).map(Expr::Tuple),
        parse_match(expr.clone()).map(Expr::Match),
        parens(expr.clone()),
        braces(expr.clone()),
        parse_var().map(Expr::Var),
    ));

    parse_field_select_or_method_call(init, expr)
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

pub fn parse_field_select_or_method_call(
    init: impl Parser<Expr>,
    expr: impl Parser<Expr>,
) -> impl Parser<Expr> {
    use chumsky::span::Span as _;

    enum MF {
        FieldSelect(Sym, Span),
        TupleSelect(u16, Span),
        MethodCall(Sym, Vec<Arg>, Span),
    }

    fn parse_tuple_idx() -> impl Parser<u16> {
        select! {
            Token::Int64(i) if i >= 0 && i < u16::MAX as i64 => i as u16,
        }
    }

    let mf = choice((
        parse_fn_call(expr.clone())
            .map_with(|call, meta| MF::MethodCall(call.sym, call.args, meta.span())),
        parse_lower().map_with(|sym, meta| MF::FieldSelect(sym, meta.span())),
        parse_tuple_idx().map_with(|idx, meta| MF::TupleSelect(idx, meta.span())),
    ));

    init.foldl(
        just(Token::Dot).ignore_then(mf).repeated(),
        |expr, mf| match mf {
            MF::FieldSelect(sym, span) => Expr::FieldSelect(FieldSelect {
                span: expr.span().union(span),
                expr: Box::new(expr),
                sym,
            }),
            MF::TupleSelect(index, span) => Expr::TupleSelect(TupleSelect {
                span: expr.span().union(span),
                expr: Box::new(expr),
                index,
            }),
            MF::MethodCall(sym, args, span) => Expr::MethodCall(MethodCall {
                span: expr.span().union(span),
                expr: Box::new(expr),
                sym,
                args,
            }),
        },
    )
}

pub fn parse_fn_call(expr: impl Parser<Expr>) -> impl Parser<FnCall> {
    parse_lower()
        .then(parens(parse_args(expr)))
        .map_with(|(sym, args), meta| FnCall {
            sym,
            args,
            span: meta.span(),
        })
}

pub fn parse_if(expr: impl Parser<Expr>) -> impl Parser<If> {
    just(Token::If)
        .ignore_then(expr.clone())
        .then(braces(expr.clone()))
        .then_ignore(just(Token::Else))
        .then(braces(expr.clone()))
        .map_with(|((cnd, thn), els), meta| If {
            cnd: Box::new(cnd),
            thn: Box::new(thn),
            els: Box::new(els),
            span: meta.span(),
        })
}

pub fn parse_let(expr: impl Parser<Expr>) -> impl Parser<Let> {
    just(Token::Let)
        .ignore_then(parse_lower())
        .then_ignore(just(Token::Equal))
        .then(expr.clone())
        .then_ignore(just(Token::Semi).or_not())
        .then(expr.clone())
        .map_with(|((sym, value), body), meta| Let {
            sym,
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

fn parse_pattern() -> impl Parser<Pattern> {
    recursive(|pat| {
        choice((
            just(Token::Under).map_with(|_, meta| Pattern::Wildcard(meta.span())),
            parse_var().map(Pattern::Var),
            parse_enum_init_generic(pat.map(Box::new)).map_with(|(ty, variant, arg), meta| {
                Pattern::Enum(EnumPattern {
                    ty,
                    variant,
                    arg,
                    span: meta.span(),
                })
            }),
        ))
    })
}

fn parse_match(expr: impl Parser<Expr>) -> impl Parser<Match> {
    let cases = parse_case(expr.clone())
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>();

    just(Token::Match)
        .ignore_then(expr.clone())
        .then(braces(cases))
        .map_with(|(scrut, cases), meta| Match {
            scrut: Box::new(scrut),
            cases,
            span: meta.span(),
        })
}

fn parse_case(expr: impl Parser<Expr>) -> impl Parser<MatchCase> {
    parse_pattern()
        .then_ignore(just(Token::FatArrow))
        .then(expr)
        .map_with(|(pat, expr), meta| MatchCase {
            pattern: pat,
            expr,
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
            sym: id,
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
    parse_lower()
        .then_ignore(just(Token::Colon))
        .then(expr)
        .map_with(|(sym, value), meta| NamedArg {
            sym,
            value,
            span: meta.span(),
        })
}

pub fn parse_enum_init_generic<A>(
    arg: impl Parser<A>,
) -> impl Parser<(Option<Sym>, Sym, Option<A>)> {
    parse_upper()
        .or_not()
        .then_ignore(just(Token::Dot))
        .then(parse_lower())
        .then(parens(arg).or_not())
        .map(|((id, variant), arg)| (id, variant, arg))
}

pub fn parse_enum_init(expr: impl Parser<Expr>) -> impl Parser<EnumInit> {
    parse_enum_init_generic(expr.clone()).map_with(|(id, variant, arg), meta| EnumInit {
        ty: id,
        variant,
        arg: arg.map(Box::new),
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
    use selfie_lexer::lex;

    use super::*;

    fn text_to_input(text: &str, path: Ustr) -> Result<ParserInput, Vec<Error>> {
        let tokens = lex(text)
            .map_err(|e| vec![Error::custom(span(path, e.span().clone()), e.to_string())])?;

        let count = tokens.len();

        let tokens: Vec<_> = tokens
            .into_iter()
            .map(|(token, range)| (token, span(path, range)))
            .collect();

        Ok(Stream::from_iter(tokens).spanned(span(path, count..count)))
    }

    fn parse_file(path: impl AsRef<Path>) -> Result<Module, Vec<Error>> {
        let path = path.as_ref();
        let contents = std::fs::read_to_string(path).unwrap();
        let tokens = lex(&contents).map_err(|e| {
            let file = Ustr::from(&path.display().to_string());
            vec![Error::custom(span(file, e.span().clone()), e.to_string())]
        })?;

        parse_module(tokens, path)
    }

    #[test]
    fn parse_examples() {
        let examples = std::fs::read_dir("../examples").unwrap();
        for example in examples {
            let example = example.unwrap();
            let path = example.path();
            if path.file_stem().unwrap().to_str().unwrap().ends_with("Bad") {
                continue;
            }
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
