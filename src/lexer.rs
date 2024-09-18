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

    /// if
    If,

    /// else
    Else,

    /// while
    While,

    /// return
    Return,

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

    /// &
    Ampersand,

    /// |
    Pipeline,

    /// ^
    Circumflex,

    /// &&
    DoubleAmpersand,

    /// ||
    DoublePipeline,

    /// &=
    AmpersandEqual,

    /// |=
    PipelineEqual,

    /// ^=
    CircumflexEqual,

    /// &&=
    DoubleAmpersandEqual,

    /// ||=
    DoublePipelineEqual,

    /// ->
    Arrow,

    /// =>
    FatArrow,

    /// .
    Dot,

    /// ::
    DoubleColon,
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
    /// Rest of source to parse
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

// file lexer.rs
