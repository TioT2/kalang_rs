use comb::PResult;

/// Symbol representation enumeration
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Symbol {
    /// fn
    Fn,

    /// let
    Let,

    /// export
    Export,

    /// enum
    Enum,

    /// struct
    Struct,

    /// mutable
    Mut,

    /// constant
    Const,

    /// as
    As,

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
    DoubleEqual,

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

/// Token representation structure
#[derive(Clone, Debug, PartialEq)]
pub enum Token<'t> {
    /// Symbol
    Symbol(Symbol),

    /// Literal
    Literal(Literal),

    /// Just string, lol
    Ident(&'t str),
}

/// Ident parsing function
fn ident<'t>(str: &'t str) -> PResult<'t, &'t str, &'t str> {
    let mut chars = str.chars();

    let first = chars.next().ok_or(str)?;

    if !(first.is_alphabetic() || first == '_') {
        return Err(str);
    }

    let total_len = chars
        .take_while(|v| v.is_alphanumeric() || *v == '_')
        .fold(first.len_utf8(), |total, ch| total + ch.len_utf8());

    Ok((
        &str[total_len..],
        &str[..total_len],
    ))
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
    type Item = Token<'t>;

    fn next(&mut self) -> Option<Self::Item> {
        let literal = comb::any((
            // Try to parse FP literal
            comb::map(comb::floating_number, Literal::Floating),

            // Try to parse integer literal
            comb::map(
                comb::any((
                    comb::all((comb::literal("0b"), comb::binary_number     )), // Binary literal
                    comb::all((comb::literal("0x"), comb::hexadecimal_number)), // Hexadecimal literal
                    comb::all((comb::literal("0o"), comb::octal_number      )), // Octal literal
                    comb::all((comb::identity     , comb::decimal_number    )), // Decimal literal
                )),
                |(_, n)| Literal::Integer(n)
            )
        ));

        let symbol = comb::any((
            comb::any((
                comb::map(comb::literal("fn"    ), |_| Symbol::Fn    ),
                comb::map(comb::literal("let"   ), |_| Symbol::Let   ),
                comb::map(comb::literal("export"), |_| Symbol::Export),
                comb::map(comb::literal("as"    ), |_| Symbol::As    ),
                comb::map(comb::literal("enum"  ), |_| Symbol::Enum  ),
                comb::map(comb::literal("struct"), |_| Symbol::Struct),
                comb::map(comb::literal("mut"   ), |_| Symbol::Mut   ),
                comb::map(comb::literal("const" ), |_| Symbol::Const ),
            )),
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
                comb::map(comb::literal("==" ), |_| Symbol::DoubleEqual),
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

// file lexer.rs
