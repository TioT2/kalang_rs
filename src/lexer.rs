use comb::PResult;

#[derive(Debug)]
pub enum Keyword {
    Fn,
    Let,
    Export,
    As,
}

#[derive(Debug)]
pub enum Symbol {
    RoundBrOpen,
    RoundBrClose,

    SquareBrOpen,
    SquareBrClose,

    CurlyBrOpen,
    CurlyBrClose,

    Semicolon,
    Colon,

    Plus,
    Minus,
    Slash,
    Asterisk,
    Hash,
    Equal,
}

#[derive(Debug)]
pub enum Literal {
    Floating(f64),
    Integer(u64),
}

#[derive(Debug)]
pub enum Token {
    Keyword(Keyword),
    Ident(String),
    Symbol(Symbol),
    Literal(Literal),
}

#[derive(Debug)]
pub struct TokenIterator<'t> {
    rest: &'t str,
}

impl<'t> TokenIterator<'t> {
    pub fn new(str: &'t str) -> Self {
        Self { rest: str }
    }
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

        let ident = |mut str: &'t str| -> PResult<'t, String> {
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
            comb::map(comb::decimal_number, Literal::Integer),
            comb::map(comb::floating_number, Literal::Floating),
        ));

        let symbol = comb::any((
            comb::map(comb::literal("("), |_| Symbol::RoundBrOpen),
            comb::map(comb::literal(")"), |_| Symbol::RoundBrClose),
            comb::map(comb::literal("["), |_| Symbol::SquareBrOpen),
            comb::map(comb::literal("]"), |_| Symbol::SquareBrClose),
            comb::map(comb::literal("{"), |_| Symbol::CurlyBrOpen),
            comb::map(comb::literal("}"), |_| Symbol::CurlyBrClose),
            comb::map(comb::literal(";"), |_| Symbol::Semicolon),
            comb::map(comb::literal(":"), |_| Symbol::Colon),
            comb::map(comb::literal("+"), |_| Symbol::Plus),
            comb::map(comb::literal("-"), |_| Symbol::Minus),
            comb::map(comb::literal("/"), |_| Symbol::Slash),
            comb::map(comb::literal("*"), |_| Symbol::Asterisk),
            comb::map(comb::literal("#"), |_| Symbol::Hash),
            comb::map(comb::literal("="), |_| Symbol::Equal),
        ));

        let token = comb::any((
            comb::map(keyword, Token::Keyword),
            comb::map(symbol, Token::Symbol),
            comb::map(literal, Token::Literal),
            comb::map(ident, Token::Ident),
        ));

        let whitespace = comb::repeat(
            comb::any_whitespace,
            || (),
            |_, _| ()
        );

        let next;
        (self.rest, next) = token(self.rest).ok()?;
        (self.rest, _) = whitespace(self.rest).unwrap();
        Some(next)
    }
}

impl Token {
    pub fn parse<'t>(str: &'t str) -> TokenIterator {
        TokenIterator::new(str)
    }
}