//! Ported from nom to chumsky from:
//! https://github.com/rust-bakery/nom/blob/main/examples/string.rs
//!
//! This example shows an example of how to parse an escaped string. The
//! rules for the string are similar to JSON and rust. A string is:
//!
//! - Enclosed by double quotes
//! - Can contain any raw unescaped code point besides \ and "
//! - Matches the following escape sequences: \b, \f, \n, \r, \t, \", \\, \/
//! - Matches code points like Rust: \u{XXXX}, where XXXX can be up to 6
//!   hex characters
//! - an escape followed by whitespace consumes all whitespace between the
//!   escape and the next non-whitespace character

// parser combinators are constructed from the bottom up:
// first we write parsers for the smallest elements (escaped characters),
// then combine them into larger parsers.

use chumsky::prelude::*;
use chumsky::text::whitespace;

/// `take_while_m_n` parses between `m` and `n` bytes (inclusive) that match
/// a predicate. `parse_hex` here parses between 1 and 6 hexadecimal numerals.
fn take_while_m_n<'a>(
    m: usize,
    n: usize,
    f: impl Fn(char) -> bool,
) -> impl Parser<'a, &'a str, &'a str> {
    any()
        .filter(move |&c| f(c))
        .repeated()
        .at_least(m)
        .at_most(n)
        .map_with(|_, meta| meta.slice())
}

/// Parse a unicode sequence, of the form u{XXXX}, where XXXX is 1 to 6
/// hexadecimal numerals. We will combine this later with parse_escaped_char
/// to parse sequences like \u{00AC}.
fn parse_unicode<'a>() -> impl Parser<'a, &'a str, char> {
    // `take_while_m_n` parses between `m` and `n` bytes (inclusive) that match
    // a predicate. `parse_hex` here parses between 1 and 6 hexadecimal numerals.
    let parse_hex = take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit());

    let parse_delimited_hex = just('u').ignore_then(parse_hex.delimited_by(just('{'), just('}')));

    // `map_res` takes the result of a parser and applies a function that returns
    // a Result. In this case we take the hex bytes from parse_hex and attempt to
    // convert them to a u32.
    let parse_u32 = parse_delimited_hex
        .try_map(move |hex, _| u32::from_str_radix(hex, 16).map_err(|_| EmptyErr::default()));

    // map_opt is like map_res, but it takes an Option instead of a Result. If
    // the function returns None, map_opt returns an error. In this case, because
    // not all u32 values are valid unicode code points, we have to fallibly
    // convert to char with from_u32.
    parse_u32.try_map(|i, _| match std::char::from_u32(i) {
        Some(c) => Ok(c),
        None => Err(EmptyErr::default()),
    })
}

/// Parse an escaped character: \n, \t, \r, \u{00AC}, etc.
fn parse_escaped_char<'a>() -> impl Parser<'a, &'a str, char> {
    just('\\').ignore_then(
        // `alt` tries each parser in sequence, returning the result of
        // the first successful match
        choice((
            parse_unicode(),
            // The `value` parser returns a fixed value (the first argument) if its
            // parser (the second argument) succeeds. In these cases, it looks for
            // the marker characters (n, r, t, etc) and returns the matching
            // character (\n, \r, \t, etc).
            just('n').to('\n'),
            just('r').to('\r'),
            just('t').to('\t'),
            just('b').to('\u{08}'),
            just('f').to('\u{0C}'),
            just('\\').to('\\'),
            just('/').to('/'),
            just('"').to('"'),
            just('\'').to('\''),
        )),
    )
}

/// Parse a backslash, followed by any amount of whitespace. This is used later
/// to discard any escaped whitespace.
fn parse_escaped_whitespace<'a>() -> impl Parser<'a, &'a str, ()> {
    just('\\').ignore_then(whitespace().at_least(1).collect::<()>())
}

/// Parse a non-empty block of text that doesn't include \ or "
fn parse_literal<'a>() -> impl Parser<'a, &'a str, &'a str> {
    any()
        .filter(|&c| c != '"' && c != '\\')
        .repeated()
        .at_least(1)
        .map_with(|_, meta| meta.slice())
}

/// A string fragment contains a fragment of a string being parsed: either
/// a non-empty Literal (a series of non-escaped characters), a single
/// parsed escaped character, or a block of escaped whitespace.
#[derive(Debug, Clone, PartialEq, Eq)]
enum StringFragment<'a> {
    Literal(&'a str),
    EscapedChar(char),
    EscapedWS,
}

/// Combine parse_literal, parse_escaped_whitespace, and parse_escaped_char
/// into a StringFragment.
fn parse_fragment<'a>() -> impl Parser<'a, &'a str, StringFragment<'a>> {
    choice((
        // The `map` combinator runs a parser, then applies a function to the output
        // of that parser.
        parse_literal().map(StringFragment::Literal),
        parse_escaped_char().map(StringFragment::EscapedChar),
        parse_escaped_whitespace().to(StringFragment::EscapedWS),
    ))
}

/// Parse a string. Use a loop of parse_fragment and push all of the fragments
/// into an output string.
pub fn parse_string<'a>() -> impl Parser<'a, &'a str, String> {
    // fold_many0 is the equivalent of iterator::fold. It runs a parser in a loop,
    // and for each output value, calls a folding function on each output value.
    let build_string = just(String::new()).foldl(
        parse_fragment().repeated(),
        // Our folding function. For each fragment, append the fragment to the
        // string.
        |mut string, fragment| {
            match fragment {
                StringFragment::Literal(s) => string.push_str(s),
                StringFragment::EscapedChar(c) => string.push(c),
                StringFragment::EscapedWS => {}
            }
            string
        },
    );

    // Finally, parse the string. Note that, if `build_string` could accept a raw
    // " character, the closing delimiter " would never match. When using
    // `delimited` with a looping parser (like fold_many0), be sure that the
    // loop won't accidentally match your closing delimiter!
    build_string.delimited_by(just('"'), just('"'))
}

pub fn parse_char<'a>() -> impl Parser<'a, &'a str, char> {
    let one_char = choice((
        parse_escaped_char(),
        any().filter(|&c| c != '\'' && c != '\\'),
    ));

    one_char.delimited_by(just('\''), just('\''))
}
