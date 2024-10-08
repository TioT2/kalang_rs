//! Token iterator (lexer core, actually) implementaion module

use comb::PResult;

use crate::lexer::{Literal, Symbol};

use super::Token;

/// Lexer-specific character propertiy identefication trait
trait CharLexerProperties {
    /// If true character is allowed to be starting for ident token
    fn is_ident_head(self) -> bool;

    /// If true character is allowed to be next token character if previous character is
    fn is_ident_tail(self) -> bool;
} // trait CharLexerProperties

impl CharLexerProperties for char {
    fn is_ident_head(self) -> bool {
        self.is_alphabetic() || self == '_' || self == '$'
    }

    fn is_ident_tail(self) -> bool {
        self.is_ident_head() || self.is_numeric()
    }
} // impl CharLexerProperties for Char

/// Point in source text representation structure
pub struct SourcePoint {
    /// Line index
    pub line: usize,

    /// Character index
    pub char: usize,
} // struct SourcePoint

impl std::fmt::Display for SourcePoint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.char)
    }
}

/// Source -> Tokens conversion iterator
#[derive(Clone, Debug)]
pub struct TokenIterator<'t> {
    /// Rest of source to parse
    rest: &'t str,
} // struct TokenIterator

impl<'t> TokenIterator<'t> {
    /// Iterator constructor
    pub fn new(str: &'t str) -> Self {
        Self { rest: str }
    } // fn new
}

#[derive(Copy, Clone, PartialEq)]
enum CharType {
    /// Raw character
    Raw,
    /// Character, parsed as escape sequence
    Escape,
} // enum CharType

fn string_literal<'t>(str: &'t str) -> PResult<'t, &'t str, String> {
    comb::collect_repeat::<String, _, _>(
        comb::map(
            comb::filter(any_escape_char, |ch| *ch != ('\"', CharType::Raw)),
            |(ch, _)| ch
        )
    )(str)
}

/// Ident parsing function
fn ident<'t>(str: &'t str) -> PResult<'t, &'t str, &'t str> {
    let mut chars = str.chars();

    let first = chars.next().ok_or(str)?;

    if !(first.is_ident_head()) {
        return Err(str);
    }

    let total_len = chars
        .take_while(|v| v.is_ident_tail())
        .fold(first.len_utf8(), |total, ch| total + ch.len_utf8());

    Ok((
        &str[total_len..],
        &str[..total_len],
    ))
} // fn ident

/// Symbol parsing function
fn symbol<'t>(string: &'t str) -> PResult<'t, &'t str, Symbol> {
    macro_rules! match_start {
        {$($name: expr => $value: expr)*} => {
            'match_start: {
                Ok({
                    $(
                        if string.starts_with($name) {
                            (&string[$name.len()..], $value)
                        } else
                    )*
                    {
                        break 'match_start Err(string);
                    }
                })
            }
        };
    }

    match_start! {
        "fn"     => Symbol::Fn
        "let"    => Symbol::Let
        "export" => Symbol::Export
        "as"     => Symbol::As
        "enum"   => Symbol::Enum
        "struct" => Symbol::Struct
        "mut"    => Symbol::Mut
        "const"  => Symbol::Const
        "if"     => Symbol::If
        "else"   => Symbol::Else
        "while"  => Symbol::While
        "return" => Symbol::Return
        "<<="    => Symbol::ShlEqual
        ">>="    => Symbol::ShrEqual
        "<="     => Symbol::TriBrOpenEqual
        ">="     => Symbol::TriBrCloseEqual
        "<<"     => Symbol::Shl
        ">>"     => Symbol::Shr
        "<"      => Symbol::TriBrOpen
        ">"      => Symbol::TriBrClose
        "&&="    => Symbol::DoubleAmpersandEqual
        "||="    => Symbol::DoublePipelineEqual
        "|="     => Symbol::PipelineEqual
        "&="     => Symbol::AmpersandEqual
        "^="     => Symbol::CircumflexEqual
        "&&"     => Symbol::DoubleAmpersand
        "||"     => Symbol::DoublePipeline
        "|"      => Symbol::Pipeline
        "&"      => Symbol::Ampersand
        "^"      => Symbol::Circumflex
        "::"     => Symbol::DoubleColon
        "."      => Symbol::Dot
        ";"      => Symbol::Semicolon
        ":"      => Symbol::Colon
        "#"      => Symbol::Hash
        ","      => Symbol::Comma
        "->"     => Symbol::Arrow
        "=>"     => Symbol::FatArrow
        "=="     => Symbol::DoubleEqual
        "!="     => Symbol::ExclamationEqual
        "+="     => Symbol::PlusEqual
        "-="     => Symbol::MinusEqual
        "/="     => Symbol::SlashEqual
        "*="     => Symbol::AsteriskEqual
        "="      => Symbol::Equal
        "!"      => Symbol::Exclamation
        "+"      => Symbol::Plus
        "-"      => Symbol::Minus
        "/"      => Symbol::Slash
        "*"      => Symbol::Asterisk
        "("      => Symbol::RoundBrOpen
        ")"      => Symbol::RoundBrClose
        "["      => Symbol::SquareBrOpen
        "]"      => Symbol::SquareBrClose
        "{"      => Symbol::CurlyBrOpen
        "}"      => Symbol::CurlyBrClose
    }
} // fn symbol

/// Character with escape sequence parsing function
fn any_escape_char<'t>(str: &'t str) -> PResult<'t, &'t str, (char, CharType)> {
    let mut chs = str.chars();

    let first = chs.next().ok_or(str)?;

    if first == '\\' {
        let next = chs.next().ok_or(str)?;

        Ok((&str[first.len_utf8() + next.len_utf8()..], (match next {
            '\'' => 0x27 as char,
            '\"' => 0x22 as char,
            '?'  => 0x3F as char,
            '\\' => 0x5C as char,
            'a'  => 0x07 as char,
            'b'  => 0x08 as char,
            'f'  => 0x0C as char,
            'n'  => 0x0A as char,
            'r'  => 0x0D as char,
            't'  => 0x09 as char,
            'v'  => 0x0B as char,
            _ => return Err(str),
        }, CharType::Escape)))
    } else {
        Ok((
            &str[first.len_utf8()..],
            (first, CharType::Raw),
        ))
    }
} // fn any_escape_char

impl<'t> Iterator for TokenIterator<'t> {
    type Item = Token<'t>;

    fn next(&mut self) -> Option<Self::Item> {

        /// Numeric literal postfix parsing function
        fn numeric_literal_postfix<'t>(str: &'t str) -> PResult<'t, &'t str, &'t str> {
            comb::or(ident, || &str[0..0])(str)
        } // fn numeric_literal_postfix

        let literal = comb::any((
            // Try to parse FP literal
            comb::map(
                comb::all((
                    comb::floating_number,
                    numeric_literal_postfix
                )),
                |(number, postfix)| Literal::Floating { number, postfix }
            ),

            // Try to parse integer literal
            comb::map(
                comb::all((
                    comb::any((
                        comb::all((comb::literal("0b"), comb::binary_number     )), // Binary literal
                        comb::all((comb::literal("0x"), comb::hexadecimal_number)), // Hexadecimal literal
                        comb::all((comb::literal("0o"), comb::octal_number      )), // Octal literal
                        comb::all((comb::identity     , comb::decimal_number    )), // Decimal literal
                    )),
                    numeric_literal_postfix,
                )),
                |((_, number), postfix)| Literal::Integer { number, postfix }
            ),

            // Try to parse string literal
            comb::map(
                comb::all((
                    comb::literal("\""),
                    string_literal,
                    comb::literal("\""),
                )),
                |(_, str, _)| Literal::String(str.to_string())
            ),

            comb::map(
                comb::all((
                    comb::literal("\'"),
                    any_escape_char,
                    comb::literal("\'"),
                )),
                |(_, (ch, _), _)| Literal::Char(ch),
            ),
        ));

        let comment = comb::any((
            // Parse singleline comment
            comb::ignore(
                comb::all((
                    comb::literal("//"),
                    comb::repeat(
                        comb::filter(comb::any_char, |ch| *ch != '\n'),
                        || (),
                        |_, _| ()
                    ),
                ))
            ),
            // Parse multiline comment
            comb::ignore(
                comb::all((
                    comb::literal("/*"),
                    comb::repeat(
                        comb::filter(
                            comb::any_with_next,
                            |(c0, c1)| *c0 != '*' && *c1 != '/'
                        ),
                        || (),
                        |_, _| ()
                    ),
                    comb::any_char,
                    comb::any_char,
                ))
            ),
        ));

        let token = comb::any((
            comb::value(comment, None),
            comb::map(
                // Token parsing
                comb::any((
                    comb::map(literal, Token::Literal),
                    comb::map(symbol, Token::Symbol),
                    comb::map(ident, Token::Ident),
                )),
                |token| Some(token)
            ),
        ));

        let whitespace = comb::repeat(
            comb::any_whitespace,
            || (),
            |_, _| ()
        );

        loop {
            let next;
            (self.rest, next) = token(self.rest).ok()?;
            (self.rest, _) = whitespace(self.rest).unwrap();

            if let Some(next) = next {
                return Some(next);
            }
        }
    } // fn next
}

// mod iter.rs
