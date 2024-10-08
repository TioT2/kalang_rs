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

// file lexer.rs
