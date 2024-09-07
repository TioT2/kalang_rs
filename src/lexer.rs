use comb::PResult;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Keyword {
    /// Function
    Fn,

    /// Variable declaration
    Let,

    /// Object visibility specifier
    Export,

    /// Transformation
    As,
} // enum Keyword

/// Symbol representation enumeration
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Symbol {
    /// (
    RoundBrOpen,

    /// )
    RoundBrClose,

    /// [
    SquareBrOpen,

    /// ]
    SquareBrClose,

    /// {
    CurlyBrOpen,

    /// }
    CurlyBrClose,

    /// <
    TriBrOpen,

    /// >
    TriBrClose,

    /// ;
    Semicolon,

    /// :
    Colon,

    /// +
    Plus,

    /// +=
    PlusEqual,

    /// -=
    MinusEqual,

    /// /=
    SlashEqual,

    /// *=
    AsteriskEqual,

    /// <<
    Shl,

    /// >>
    Shr,

    /// <<=
    ShlEqual,

    /// >>=
    ShrEqual,

    /// -
    Minus,

    /// ,
    Comma,

    /// /
    Slash,

    /// *
    Asterisk,

    /// #
    Hash,

    /// =
    Equal,

    /// ==
    EqualEqual,

    /// !
    Exclamation,

    /// !=
    ExclamationEqual,

    /// <=
    TriBrOpenEqual,

    /// >=
    TriBrCloseEqual,
} // enum Symbol

/// Literal representation structure
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Literal {
    /// Floating point literal
    Floating(f64),

    /// Integer literal
    Integer(u64),
} // enum Literal

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Keyword(Keyword),
    Ident(String),
    Symbol(Symbol),
    Literal(Literal),
}

/// Source -> Tokens conversion iterator
#[derive(Debug)]
pub struct TokenIterator<'t> {
    /// rest of source to parse
    rest: &'t str,
} // struct TokenIterator

impl<'t> TokenIterator<'t> {
    /// Iterator constructor
    pub fn new(str: &'t str) -> Self {
        Self { rest: str }
    } // fn new
}

impl<'t> Iterator for TokenIterator<'t> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let keyword = comb::map(
            comb::all((
                comb::any((
                    comb::map(comb::literal("fn"    ), |_| Keyword::Fn    ),
                    comb::map(comb::literal("let"   ), |_| Keyword::Let   ),
                    comb::map(comb::literal("export"), |_| Keyword::Export),
                    comb::map(comb::literal("as"    ), |_| Keyword::As    ),
                )),
                comb::filter(
                    comb::any_char,
                    |ch| !ch.is_alphanumeric(),
                ),
            )),
            |(kw, _)| kw,
        );

        let ident = |mut str: &'t str| -> PResult<'t, &str, String> {
            let first;

            (str, first) = comb::filter(
                comb::any_char,
                |ch| ch.is_alphabetic() || *ch == '_',
            )(str)?;

            comb::repeat(
                comb::filter(comb::any_char, |ch| ch.is_alphanumeric() || *ch == '_'),
                move || first.to_string(),
                |mut string, char| {
                    string.push(char);
                    string
                }
            )(str)
        };
        
        let literal = comb::any((
            // Try to parse FP literal
            comb::map(comb::floating_number, Literal::Floating),

            // Try to parse integer literal
            comb::map(
                comb::any((
                    comb::map(
                        comb::all((
                            comb::literal("0b"),
                            comb::binary_number,
                        )),
                        |(_, n)| n
                    ),
                    comb::map(
                        comb::all((
                            comb::literal("0x"),
                            comb::hexadecimal_number,
                        )),
                        |(_, n)| n
                    ),
                    comb::decimal_number,
                )),
                Literal::Integer,
            )
        ));

        let symbol = comb::any((
            comb::any((
                comb::map(comb::literal("<<="), |_| Symbol::ShlEqual),
                comb::map(comb::literal(">>="), |_| Symbol::ShrEqual),
                comb::map(comb::literal("<=" ), |_| Symbol::TriBrOpenEqual),
                comb::map(comb::literal(">=" ), |_| Symbol::TriBrCloseEqual),

                comb::map(comb::literal("<<" ), |_| Symbol::Shl),
                comb::map(comb::literal(">>" ), |_| Symbol::Shr),
                comb::map(comb::literal("<"  ), |_| Symbol::TriBrOpen),
                comb::map(comb::literal(">"  ), |_| Symbol::TriBrClose),
            )),
            comb::any((
                comb::map(comb::literal("==" ), |_| Symbol::EqualEqual),
                comb::map(comb::literal("!=" ), |_| Symbol::ExclamationEqual),
                comb::map(comb::literal("+=" ), |_| Symbol::PlusEqual),
                comb::map(comb::literal("-=" ), |_| Symbol::MinusEqual),
                comb::map(comb::literal("/=" ), |_| Symbol::SlashEqual),
                comb::map(comb::literal("*=" ), |_| Symbol::AsteriskEqual),
                
                comb::map(comb::literal("="  ), |_| Symbol::Equal),
                comb::map(comb::literal("!"  ), |_| Symbol::Exclamation),
                comb::map(comb::literal("+"  ), |_| Symbol::Plus),
                comb::map(comb::literal("-"  ), |_| Symbol::Minus),
                comb::map(comb::literal("/"  ), |_| Symbol::Slash),
                comb::map(comb::literal("*"  ), |_| Symbol::Asterisk),
            )),
            comb::map(comb::literal("("  ), |_| Symbol::RoundBrOpen),
            comb::map(comb::literal(")"  ), |_| Symbol::RoundBrClose),
            comb::map(comb::literal("["  ), |_| Symbol::SquareBrOpen),
            comb::map(comb::literal("]"  ), |_| Symbol::SquareBrClose),
            comb::map(comb::literal("{"  ), |_| Symbol::CurlyBrOpen),
            comb::map(comb::literal("}"  ), |_| Symbol::CurlyBrClose),
            comb::map(comb::literal(";"  ), |_| Symbol::Semicolon),
            comb::map(comb::literal(":"  ), |_| Symbol::Colon),
            comb::map(comb::literal("#"  ), |_| Symbol::Hash),
            comb::map(comb::literal(","  ), |_| Symbol::Comma),
        ));

        let comment = comb::all((
            comb::literal("//"),
            comb::repeat(
                comb::filter(comb::any_char, |ch| *ch != '\n'),
                || (),
                |_, _| ()
            )
        ));

        let token = comb::any((
            comb::map(
                comment,
                |_| None,
            ),
            comb::map(
                // Token parsing
                comb::any((
                    comb::map(keyword, Token::Keyword),
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
    }
}
