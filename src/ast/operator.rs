use crate::lexer::Symbol;

/// Mathematical operation representation structure
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Operator {
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

    /// Negation operator
    UnaryMinus,

    /// Unary addition operator
    UnaryPlus,

    /// Referencing operator
    Reference,

    /// Pointer dereference operator
    Dereference,
} // enum Operator

/// Operator descriptor
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum OperatorInfo {
    /// Binary operator info
    Binary {
        /// Operator priority
        priority: u32,

        /// * true  if operator is left-associative : a . b . c === (a . b) . c
        /// * false if operator is right-associative: a . b . c === a . (b . c)
        is_left_assoc: bool,
    },
    /// Unary operator info
    Unary,
}

impl Operator {
    /// Operator info getting function
    pub fn info(self) -> OperatorInfo {
        match self {
            Self::Add          => OperatorInfo::Binary { priority:  6, is_left_assoc: true  },
            Self::Sub          => OperatorInfo::Binary { priority:  6, is_left_assoc: true  },
            Self::Mul          => OperatorInfo::Binary { priority:  5, is_left_assoc: true  },
            Self::Div          => OperatorInfo::Binary { priority:  5, is_left_assoc: true  },
            Self::Shl          => OperatorInfo::Binary { priority:  7, is_left_assoc: true  },
            Self::Shr          => OperatorInfo::Binary { priority:  7, is_left_assoc: true  },
            Self::AddAssign    => OperatorInfo::Binary { priority: 15, is_left_assoc: false },
            Self::SubAssign    => OperatorInfo::Binary { priority: 15, is_left_assoc: false },
            Self::MulAssign    => OperatorInfo::Binary { priority: 15, is_left_assoc: false },
            Self::DivAssign    => OperatorInfo::Binary { priority: 15, is_left_assoc: false },
            Self::ShlAssign    => OperatorInfo::Binary { priority: 15, is_left_assoc: false },
            Self::ShrAssign    => OperatorInfo::Binary { priority: 15, is_left_assoc: false },
            Self::Lt           => OperatorInfo::Binary { priority:  8, is_left_assoc: true  },
            Self::Gt           => OperatorInfo::Binary { priority:  8, is_left_assoc: true  },
            Self::Le           => OperatorInfo::Binary { priority:  8, is_left_assoc: true  },
            Self::Ge           => OperatorInfo::Binary { priority:  8, is_left_assoc: true  },
            Self::Eq           => OperatorInfo::Binary { priority:  9, is_left_assoc: true  },
            Self::Ne           => OperatorInfo::Binary { priority:  9, is_left_assoc: true  },
            Self::And          => OperatorInfo::Binary { priority: 13, is_left_assoc: true  },
            Self::Or           => OperatorInfo::Binary { priority: 14, is_left_assoc: true  },
            Self::BitAnd       => OperatorInfo::Binary { priority: 10, is_left_assoc: true  },
            Self::BitOr        => OperatorInfo::Binary { priority: 12, is_left_assoc: true  },
            Self::BitXor       => OperatorInfo::Binary { priority: 11, is_left_assoc: true  },
            Self::AndAssign    => OperatorInfo::Binary { priority: 15, is_left_assoc: false },
            Self::OrAssign     => OperatorInfo::Binary { priority: 15, is_left_assoc: false },
            Self::BitAndAssign => OperatorInfo::Binary { priority: 15, is_left_assoc: false },
            Self::BitOrAssign  => OperatorInfo::Binary { priority: 15, is_left_assoc: false },
            Self::BitXorAssign => OperatorInfo::Binary { priority: 15, is_left_assoc: false },
            Self::Assign       => OperatorInfo::Binary { priority: 15, is_left_assoc: false },
            Self::UnaryMinus   => OperatorInfo::Unary,
            Self::UnaryPlus    => OperatorInfo::Unary,
            Self::Reference    => OperatorInfo::Unary,
            Self::Dereference  => OperatorInfo::Unary,
        }
    }

    pub fn from_symbol(symbol: Symbol) -> Option<Self> {
        let sym = match symbol {
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
        };

        Some(sym)
    } // fn from_symbol

    pub fn into_unary(self) -> Option<Self> {
        Some(match self {
            Self::Add    => Self::UnaryPlus,
            Self::Sub    => Self::UnaryMinus,
            Self::BitAnd => Self::Reference,
            Self::Mul    => Self::Dereference,
            _            => return None,
        })
    }
}
