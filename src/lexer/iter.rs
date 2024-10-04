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
            ident(str)
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

        let symbol = comb::any((
            comb::any((
                comb::value(comb::literal("fn"    ), Symbol::Fn    ),
                comb::value(comb::literal("let"   ), Symbol::Let   ),
                comb::value(comb::literal("export"), Symbol::Export),
                comb::value(comb::literal("as"    ), Symbol::As    ),
                comb::value(comb::literal("enum"  ), Symbol::Enum  ),
                comb::value(comb::literal("struct"), Symbol::Struct),
                comb::value(comb::literal("mut"   ), Symbol::Mut   ),
                comb::value(comb::literal("const" ), Symbol::Const ),
                comb::value(comb::literal("if"    ), Symbol::If    ),
                comb::value(comb::literal("else"  ), Symbol::Else  ),
                comb::value(comb::literal("while" ), Symbol::While ),
                comb::value(comb::literal("return"), Symbol::Return),
            )),
            comb::any((
                comb::value(comb::literal("<<="), Symbol::ShlEqual),
                comb::value(comb::literal(">>="), Symbol::ShrEqual),
                comb::value(comb::literal("<=" ), Symbol::TriBrOpenEqual),
                comb::value(comb::literal(">=" ), Symbol::TriBrCloseEqual),

                comb::value(comb::literal("<<" ), Symbol::Shl),
                comb::value(comb::literal(">>" ), Symbol::Shr),
                comb::value(comb::literal("<"  ), Symbol::TriBrOpen),
                comb::value(comb::literal(">"  ), Symbol::TriBrClose),
            )),
            comb::any((
                comb::value(comb::literal("&&="), Symbol::DoubleAmpersandEqual),
                comb::value(comb::literal("||="), Symbol::DoublePipelineEqual),
                comb::value(comb::literal("|=" ), Symbol::PipelineEqual),
                comb::value(comb::literal("&=" ), Symbol::AmpersandEqual),
                comb::value(comb::literal("^=" ), Symbol::CircumflexEqual),

                comb::value(comb::literal("&&" ), Symbol::DoubleAmpersand),
                comb::value(comb::literal("||" ), Symbol::DoublePipeline),
                comb::value(comb::literal("|"  ), Symbol::Pipeline),
                comb::value(comb::literal("&"  ), Symbol::Ampersand),
                comb::value(comb::literal("^"  ), Symbol::Circumflex),
            )),
            comb::any((
                comb::value(comb::literal("::" ), Symbol::DoubleColon),
                comb::value(comb::literal("."  ), Symbol::Dot),
                comb::value(comb::literal(";"  ), Symbol::Semicolon),
                comb::value(comb::literal(":"  ), Symbol::Colon),
                comb::value(comb::literal("#"  ), Symbol::Hash),
                comb::value(comb::literal(","  ), Symbol::Comma),
                comb::value(comb::literal("->" ), Symbol::Arrow),
                comb::value(comb::literal("=>" ), Symbol::FatArrow),
            )),
            comb::any((
                comb::value(comb::literal("==" ), Symbol::DoubleEqual),
                comb::value(comb::literal("!=" ), Symbol::ExclamationEqual),
                comb::value(comb::literal("+=" ), Symbol::PlusEqual),
                comb::value(comb::literal("-=" ), Symbol::MinusEqual),
                comb::value(comb::literal("/=" ), Symbol::SlashEqual),
                comb::value(comb::literal("*=" ), Symbol::AsteriskEqual),

                comb::value(comb::literal("="  ), Symbol::Equal),
                comb::value(comb::literal("!"  ), Symbol::Exclamation),
                comb::value(comb::literal("+"  ), Symbol::Plus),
                comb::value(comb::literal("-"  ), Symbol::Minus),
                comb::value(comb::literal("/"  ), Symbol::Slash),
                comb::value(comb::literal("*"  ), Symbol::Asterisk),
            )),
            comb::any((
                comb::value(comb::literal("("  ), Symbol::RoundBrOpen),
                comb::value(comb::literal(")"  ), Symbol::RoundBrClose),
                comb::value(comb::literal("["  ), Symbol::SquareBrOpen),
                comb::value(comb::literal("]"  ), Symbol::SquareBrClose),
                comb::value(comb::literal("{"  ), Symbol::CurlyBrOpen),
                comb::value(comb::literal("}"  ), Symbol::CurlyBrClose),
            )),
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
