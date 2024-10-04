//! Source lexical analyzer implementation module

mod iter;

pub use iter::TokenIterator;

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
#[derive(Clone, Debug, PartialEq)]
pub enum Literal<'t> {
    /// Floating point literal
    Floating {
        number: f64,
        postfix: &'t str,
    },

    /// Integer literal
    Integer {
        number: u64,
        postfix: &'t str,
    },

    /// Character literal
    Char(char),

    /// String literal
    String(String),
} // enum Literal

/// Token representation structure
#[derive(Clone, Debug, PartialEq)]
pub enum Token<'t> {
    /// Symbol
    Symbol(Symbol),

    /// Literal
    Literal(Literal<'t>),

    /// Just string, lol
    Ident(&'t str),
} // enum Token

/// Lazy tokenizer representation structure
pub struct TokenQueue<'src> {
    /// Actually, underlying iterator
    iterator: &'src mut dyn Iterator<Item = Token<'src>>,

    /// Already parsed tokens
    collector: Vec<Token<'src>>,
} // struct TokenQueue

impl<'src> TokenQueue<'src> {
    /// Token queue from source building function
    pub fn new(iterator: &'src mut dyn Iterator<Item = Token<'src>>) -> Self {
        Self {
            iterator,
            collector: Vec::new()
        }
    } // fn new

    /// Token queue element by index getting function
    pub fn get<'get: 'src>(&'get mut self, index: usize) -> Option<&'get Token<'src>> {
        while self.collector.len() <= index {
            self.collector.push(self.iterator.next()?);
        }

        Some(self.collector.get(index).unwrap())
    } // fn get
}

/// Something like 'iterator' on token queue representation structure
pub struct TokenQueueView<'queue, 'src> where 'queue: 'src {
    /// Queue this view is exists for reference
    queue: &'queue mut TokenQueue<'src>,

    /// Index of current token in queue
    location: usize,
} // struct TokenQueueView

impl<'queue, 'src: 'queue> TokenQueueView<'queue, 'src> {
    /// Subview from current moment getting function
    pub fn subview<'subview: 'queue>(&'subview mut self) -> TokenQueueView<'subview, 'src> {
        Self {
            location: self.location,
            queue: &mut self.queue
        }
    } // fn subview

    /// Next token getting function
    pub fn next<'get: 'src>(&'get mut self) -> Option<&'get Token<'src>> {
        let token = self.queue.get(self.location);
        self.location += 1;
        token
    } // fn next
}

// file lexer.rs
