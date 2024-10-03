use crate::lexer::Symbol;

/// Binary operator representation structure
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BinaryOperator {
    /// Addition
    Add,

    /// Substraction
    Sub,

    /// Multiplication
    Mul,

    /// Division
    Div,

    /// Left move
    Shl,

    /// Right move
    Shr,

    /// Addition with assignment
    AddAssign,

    /// Substraction with assignment
    SubAssign,

    /// Multiplication with assignment
    MulAssign,

    /// Division with assignment
    DivAssign,

    /// Left move with assignment
    ShlAssign,

    /// Right move with assignment
    ShrAssign,

    /// Less than (<)
    Lt,

    /// Greater than (>)
    Gt,

    /// Less or equal (<=)
    Le,

    /// Greater or equal (>=)
    Ge,

    /// Equal (==)
    Eq,

    /// Not equal (!=)
    Ne,

    /// Logical and (&&)
    And,

    /// Logical or (||)
    Or,

    /// Bitwise and (&)
    BitAnd,

    /// Bitwise or (|)
    BitOr,

    /// Birwise xor (^)
    BitXor,

    /// Logical and with assignment
    AndAssign,

    /// Logical or with assignment
    OrAssign,

    /// Bitwise and with assignment
    BitAndAssign,

    /// Bitwise or with assignment
    BitOrAssign,

    /// Bitwise xor with assignment
    BitXorAssign,

    /// Assignment
    Assign,
} // enum BinaryOperator

/// Binary operator descriptor
#[derive(Copy, Clone, Debug)]
pub struct BinaryOperatorInfo {
    /// Operator priority
    pub priority: u32,

    /// * true  if operator is left-associative : a . b . c === (a . b) . c
    /// * false if operator is right-associative: a . b . c === a . (b . c)
    pub is_left_assoc: bool,
} // struct BinaryOperatorInfo

impl BinaryOperator {
    pub fn from_symbol(sym: Symbol) -> Option<Self> {
        Some(match sym {
            Symbol::Plus                 => Self::Add,
            Symbol::Minus                => Self::Sub,
            Symbol::Asterisk             => Self::Mul,
            Symbol::Slash                => Self::Div,
            Symbol::Shl                  => Self::Shl,
            Symbol::Shr                  => Self::Shr,

            Symbol::PlusEqual            => Self::AddAssign,
            Symbol::MinusEqual           => Self::SubAssign,
            Symbol::AsteriskEqual        => Self::MulAssign,
            Symbol::SlashEqual           => Self::DivAssign,
            Symbol::ShlEqual             => Self::ShlAssign,
            Symbol::ShrEqual             => Self::ShrAssign,

            Symbol::TriBrOpen            => Self::Lt,
            Symbol::TriBrClose           => Self::Gt,
            Symbol::TriBrOpenEqual       => Self::Le,
            Symbol::TriBrCloseEqual      => Self::Ge,
            Symbol::DoubleEqual          => Self::Eq,
            Symbol::ExclamationEqual     => Self::Ne,
            Symbol::Equal                => Self::Assign,

            Symbol::Ampersand            => Self::BitAnd,
            Symbol::Pipeline             => Self::BitOr,
            Symbol::Circumflex           => Self::BitXor,
            Symbol::DoubleAmpersand      => Self::And,
            Symbol::DoublePipeline       => Self::Or,

            Symbol::AmpersandEqual       => Self::BitAndAssign,
            Symbol::PipelineEqual        => Self::BitOrAssign,
            Symbol::CircumflexEqual      => Self::BitXorAssign,
            Symbol::DoubleAmpersandEqual => Self::AndAssign,
            Symbol::DoublePipelineEqual  => Self::OrAssign,
            _ => return None,
        })
    } // fn from_symbol

    /// Binary operator info getting function
    pub fn info(self) -> BinaryOperatorInfo {
        match self {
            Self::Add          => BinaryOperatorInfo { priority:  6, is_left_assoc: true  },
            Self::Sub          => BinaryOperatorInfo { priority:  6, is_left_assoc: true  },
            Self::Mul          => BinaryOperatorInfo { priority:  5, is_left_assoc: true  },
            Self::Div          => BinaryOperatorInfo { priority:  5, is_left_assoc: true  },
            Self::Shl          => BinaryOperatorInfo { priority:  7, is_left_assoc: true  },
            Self::Shr          => BinaryOperatorInfo { priority:  7, is_left_assoc: true  },
            Self::AddAssign    => BinaryOperatorInfo { priority: 15, is_left_assoc: false },
            Self::SubAssign    => BinaryOperatorInfo { priority: 15, is_left_assoc: false },
            Self::MulAssign    => BinaryOperatorInfo { priority: 15, is_left_assoc: false },
            Self::DivAssign    => BinaryOperatorInfo { priority: 15, is_left_assoc: false },
            Self::ShlAssign    => BinaryOperatorInfo { priority: 15, is_left_assoc: false },
            Self::ShrAssign    => BinaryOperatorInfo { priority: 15, is_left_assoc: false },
            Self::Lt           => BinaryOperatorInfo { priority:  8, is_left_assoc: true  },
            Self::Gt           => BinaryOperatorInfo { priority:  8, is_left_assoc: true  },
            Self::Le           => BinaryOperatorInfo { priority:  8, is_left_assoc: true  },
            Self::Ge           => BinaryOperatorInfo { priority:  8, is_left_assoc: true  },
            Self::Eq           => BinaryOperatorInfo { priority:  9, is_left_assoc: true  },
            Self::Ne           => BinaryOperatorInfo { priority:  9, is_left_assoc: true  },
            Self::And          => BinaryOperatorInfo { priority: 13, is_left_assoc: true  },
            Self::Or           => BinaryOperatorInfo { priority: 14, is_left_assoc: true  },
            Self::BitAnd       => BinaryOperatorInfo { priority: 10, is_left_assoc: true  },
            Self::BitOr        => BinaryOperatorInfo { priority: 12, is_left_assoc: true  },
            Self::BitXor       => BinaryOperatorInfo { priority: 11, is_left_assoc: true  },
            Self::AndAssign    => BinaryOperatorInfo { priority: 15, is_left_assoc: false },
            Self::OrAssign     => BinaryOperatorInfo { priority: 15, is_left_assoc: false },
            Self::BitAndAssign => BinaryOperatorInfo { priority: 15, is_left_assoc: false },
            Self::BitOrAssign  => BinaryOperatorInfo { priority: 15, is_left_assoc: false },
            Self::BitXorAssign => BinaryOperatorInfo { priority: 15, is_left_assoc: false },
            Self::Assign       => BinaryOperatorInfo { priority: 15, is_left_assoc: false },
        }
    } // fn info
} // impl BinaryOperator
