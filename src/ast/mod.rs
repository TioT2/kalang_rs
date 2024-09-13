
mod parse;

use std::collections::HashMap;

use comb::PResult;

use crate::lexer::{Literal, Symbol, Token};

/// Mathematical operation representation structure
#[derive(Debug, Clone, PartialEq, Eq)]
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

    /// Assignment
    Assign,
} // enum Operator

impl Operator {
    pub fn from_symbol(symbol: Symbol) -> Option<Operator> {
        let sym = match symbol {
            Symbol::Plus             => Self::Add,
            Symbol::Minus            => Self::Sub,
            Symbol::Asterisk         => Self::Mul,
            Symbol::Slash            => Self::Div,
            Symbol::Shl              => Self::Shl,
            Symbol::Shr              => Self::Shr,
            Symbol::PlusEqual        => Self::AddAssign,
            Symbol::MinusEqual       => Self::SubAssign,
            Symbol::AsteriskEqual    => Self::MulAssign,
            Symbol::SlashEqual       => Self::DivAssign,
            Symbol::ShlEqual         => Self::ShlAssign,
            Symbol::ShrEqual         => Self::ShrAssign,
            Symbol::TriBrOpen        => Self::Lt,
            Symbol::TriBrClose       => Self::Gt,
            Symbol::TriBrOpenEqual   => Self::Le,
            Symbol::TriBrCloseEqual  => Self::Ge,
            Symbol::EqualEqual       => Self::Eq,
            Symbol::ExclamationEqual => Self::Ne,
            Symbol::Equal            => Self::Assign,
            _ => return None,
        };

        Some(sym)
    }
}

/// Variable mutability
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Mutability {
    /// Constant (can be calculated in compiletime)
    Const,

    /// Immutable (may not be changed, calculated during initialization)
    Immut,

    /// Mutable (may be changed)
    Mut,
} // enum Mutability

/// Expression representation structure
#[derive(Debug)]
pub enum Expression {
    /// Just ident
    Ident {
        /// Ident
        ident: String,
    },

    /// Literal (inline value)
    Literal {
        /// Literal
        literal: Literal,
    },

    /// Code block
    Block {
        /// Block code
        code: Vec<Statement>,

        /// Return value calculator
        return_value: Option<Box<Expression>>,
    },

    /// Function call
    Call {
        /// Called object
        object: Box<Expression>,

        /// Call parameters
        parameters: Vec<Expression>,
    },

    /// Stack access
    Access {
        /// Accessed object (array/tuple, initially)
        object: Box<Expression>,

        /// Access indices
        indices: Vec<Expression>,
    },

    /// Binary operator
    BinaryOperator {
        /// Left hand side
        lhs: Box<Expression>,

        /// Right hand side
        rhs: Box<Expression>,

        /// Operator, actually
        operator: Operator,
    },

    /// Unary operator
    UnaryOperator {
        /// Operand
        operand: Box<Expression>,

        /// Operator
        operator: Operator,
    },
} // enum Expression

/// The core object of AST - statement
#[derive(Debug)]
pub enum Statement {
    /// Variable declaration
    Let {
        /// Name of connection to declare
        variable_name: String,

        /// Mutability (actually, evaluation rule)
        mutability: Mutability,

        /// Type
        ty: Box<Type>,

        /// Initializer expression
        initializer: Box<Expression>,
    },

    /// Actually, just expression
    Expression {
        /// Contents
        expr: Box<Expression>,
    },

    /// While statement
    While {
        /// While condition
        condition: Box<Expression>,

        /// While operator code
        code: Vec<Statement>,
    },

    /// If statement
    If {
        /// Condition
        condition: Box<Expression>,

        /// Code executed then condition -> true
        then_code: Vec<Statement>,

        /// Code executed then condition -> false
        else_code: Vec<Statement>,
    },

    /// Function return statement
    Return {
        /// Resulting expression
        expression: Box<Expression>,
    },
} // enum Statement

/// Expression value representation structures
#[derive(Debug)]
pub enum Value {
    /// Integer literal
    Integer(u64),

    /// FP literal
    Floating(f64),

    /// Strnig (reference to nametable element)
    Variable(String),
} // enum Value

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum PrimitiveType {
    /// 8 bit signed integer
    I8,

    /// 8 bit unsigned integer
    U8,

    /// 16 bit signed integer
    I16,

    /// 16 bit unsigned integer
    U16,

    /// 32 bit signed integer
    I32,

    /// 32 bit unsigned integer
    U32,

    /// 32 bit FP number
    F32,

    /// 64 bit signed integer
    I64,

    /// 64 bit unsigned integer
    U64,

    /// 64 bit FP number
    F64,

    /// Unsigned number with size of pointer
    Usize,

    /// Signed number with size of pointer
    Isize,

    /// Empty type
    Void,

    /// Never-assigned type (type of return/break expressions, error handlers, etc)
    Never,
} // enum PrimitiveType

impl PrimitiveType {
    /// Parsing function
    pub fn parse(str: &str) -> Option<PrimitiveType> {
        match str {
            "i8"    => Some(PrimitiveType::I8   ),
            "u8"    => Some(PrimitiveType::U8   ),
            "i16"   => Some(PrimitiveType::I16  ),
            "u16"   => Some(PrimitiveType::U16  ),
            "i32"   => Some(PrimitiveType::I32  ),
            "u32"   => Some(PrimitiveType::U32  ),
            "f32"   => Some(PrimitiveType::F32  ),
            "i64"   => Some(PrimitiveType::I64  ),
            "u64"   => Some(PrimitiveType::U64  ),
            "f64"   => Some(PrimitiveType::F64  ),
            "usize" => Some(PrimitiveType::Usize),
            "isize" => Some(PrimitiveType::Isize),
            "void"  => Some(PrimitiveType::Void ),
            "!"     => Some(PrimitiveType::Never),
            _ => None,
        }
    } // fn parse
}

/// Type representation structure
#[derive(Debug, Clone)]
pub enum Type {
    /// Named type (reference to previously declared structure/enumeration/etc.)
    Named(String),

    /// Primitive
    Primitive(PrimitiveType),

    /// Array
    Array {
        /// Single element type
        element_type: Box<Type>,

        /// Array size
        size: usize,
    },

    /// Pointer
    Pointer {
        /// Element type
        element_type: Box<Type>,

        /// Element mutability
        is_mutable: bool,
    },

    /// Tuple
    Tuple {
        /// Set of element types
        element_types: Vec<Type>,
    },

    /// Pointer to function
    FunctionPointer {
        /// Set of function pointer inputs
        inputs: Vec<Type>,

        /// Function output value
        output: Box<Type>,
    },
} // enum Type

/// Enumeration variant representaion structure
#[derive(Clone, Debug)]
pub struct EnumVariant {
    /// Variant name
    pub name: String,

    /// Explicit variant index
    pub explicit_index: Option<usize>,
} // struct EnumVariant

/// Declaration representation enumeration
#[derive(Debug)]
pub enum Declaration {
    /// Function
    Function {
        /// Input set
        inputs: Vec<(String, Type)>,

        /// return value
        output: Type,
    },

    /// Variable (ded uebet)
    Variable {
        /// Type
        ty: Type,

        /// Initializer expression
        initializer: Option<Box<Expression>>,
    },

    /// Structure
    Structure {
        /// Structure elements
        elements: Vec<(String, Type)>,
    },

    /// Enum
    Enumeration {
        /// Variants
        variants: Vec<EnumVariant>,
    },
} // enum Declaration

/// Module representation structure
#[derive(Debug)]
pub struct Module {
    /// Declaration set
    pub declarations: HashMap<String, Declaration>,
} // struct Module

#[derive(Debug)]
enum InverseStackValue {
    /// Operator representation structure
    Operator(Operator),

    /// Expression
    Expression(Expression),
}

/// Internal helper enum
enum IndexingOrCall {
    /// Is indexing
    Indexing,
    /// Is call
    Call,
}

/// Indexing or function call
fn parse_indexing_or_call<'t>(mut tl: &'t [Token<'t>]) -> PResult<'t, &'t [Token], (IndexingOrCall, Vec<Expression>)> {
    let (indexing_or_call, expected_end) = match tl.get(0) {
        Some(Token::Symbol(Symbol::RoundBrOpen)) => (IndexingOrCall::Call, Symbol::RoundBrClose),
        Some(Token::Symbol(Symbol::SquareBrOpen)) => (IndexingOrCall::Indexing, Symbol::SquareBrClose),
        _ => return Err(tl),
    };

    // parse set of subexpressions
    tl = &tl[1..];

    let arguments;

    (tl, (arguments, _)) = comb::all((
        // parse arguments
        comb::repeat_with_separator(
            parse_expression,
            parse::symbol(Symbol::Comma),
            Vec::new,
            |mut vec, arg| {
                vec.push(arg);
                vec
            }
        ),
        // parse symbol
        parse::symbol(expected_end),
    ))(tl)?;

    Ok((tl, (indexing_or_call, arguments)))
}

/// Expression parsing function
/// * `tokens` - token set
/// * Returns parsing result.
fn parse_expression<'t>(mut tl: &'t [Token<'t>]) -> PResult<'t, &'t [Token<'t>], Expression> {

    // Stack of operators
    let mut operator_stack = Vec::<Operator>::new();

    // (Toklist -> Inverse) parsing result
    let mut result = Vec::<InverseStackValue>::new();

    // Add parser state??

    // Postfix parsing
    'expr_loop: loop {
        let token = match tl.get(0) {
            Some(tok) => tok,
            None => break 'expr_loop,
        };

        match token {
            // Subexpression or ident (thing that can be indexed or called, actually)
            Token::Symbol(Symbol::RoundBrOpen) | Token::Ident(_) => {
                // parse subexpression
                let subexpr;
                let indexing_or_call;

                (tl, (subexpr, indexing_or_call)) = comb::all((
                    comb::any((
                        // Subexpression enclosed in parentheses
                        comb::map(
                            comb::all((
                                parse::symbol(Symbol::RoundBrOpen),
                                parse_expression,
                                parse::symbol(Symbol::RoundBrClose),
                            )),
                            |(_, subexpr, _)| subexpr,
                        ),

                        // Just ident, lol
                        comb::map(
                            parse::ident,
                            |ident| Expression::Ident { ident: ident.to_string() },
                        ),
                    )),
                    comb::any((
                        // Indexing/Call
                        comb::map(parse_indexing_or_call, |v| Some(v)),

                        // Default case
                        comb::map(comb::identity, |_| None),
                    ))
                ))(tl)?;

                let expr = match indexing_or_call {

                    // Subexpression was indexed
                    Some((IndexingOrCall::Indexing, indices)) => Expression::Access {
                        object: Box::new(subexpr),
                        indices,
                    },

                    // Subexpression was called
                    Some((IndexingOrCall::Call, parameters)) => Expression::Call {
                        object: Box::new(subexpr),
                        parameters,
                    },

                    // Subexpression last as-is
                    None => subexpr,
                };

                // Append subexpression to result
                result.push(InverseStackValue::Expression(expr));
            }
            Token::Literal(literal) => {
                // no indexing or call may occur after parsing
                result.push(InverseStackValue::Expression(Expression::Literal { literal: *literal }));
                tl = &tl[1..];
            }
            Token::Symbol(sym) => if let Some(op) = Operator::from_symbol(*sym) {
                operator_stack.push(op);
                tl = &tl[1..];
            } else {
                // Unexpected no-op symbol in expression, it'll be handled later...
                break 'expr_loop;
            }
        }

        break 'expr_loop;
    }

    // At least now this function can handle only literal...
    comb::map(
        parse::literal,
        |literal| Expression::Literal { literal }
    )(tl)
} // fn parse_expression

/// Variable parsing function
fn parse_variable<'t>(tokens: &'t [Token<'t>]) -> PResult<'t, &'t [Token<'t>], (&'t str, Declaration)> {
    comb::map(
        comb::all((
            parse::symbol(Symbol::Let),
            parse::ident,
            parse::symbol(Symbol::Colon),
            parse::ty,
            comb::any((
                comb::map(
                    comb::all((
                        parse::symbol(Symbol::Equal),
                        comb::map(
                            parse_expression,
                            Box::new
                        ),
                    )),
                    |(_, exp)| Some(exp),
                ),
                comb::map(
                    comb::identity,
                    |_| None,
                ),
            )),
            parse::symbol(Symbol::Semicolon),
        )),
        |(_, name, _, ty, initializer, _)| (name, Declaration::Variable { ty, initializer })
    )(tokens)
} // fn parse_variable

/// Function parsing function
fn parse_function<'t>(tokens: &'t [Token<'t>]) -> PResult<'t, &'t [Token<'t>], (&'t str, Declaration)> {
    comb::map(
        comb::all((
            parse::symbol(Symbol::Fn),
            parse::ident,
            parse::symbol(Symbol::RoundBrOpen),
            comb::repeat_with_separator(
                comb::all((
                    parse::ident,
                    parse::symbol(Symbol::Colon),
                    parse::ty,
                )),
                parse::symbol(Symbol::Comma),
                Vec::new,
                |mut vec, (name, _, type_name)| {
                    vec.push((name.to_string(), type_name));
                    vec
                }
            ),
            parse::symbol(Symbol::RoundBrClose),
            parse::ty,
        )),
        |(_, name, _, inputs, _, output)| (name, Declaration::Function {
            inputs,
            output,
        })
    )(tokens)
} // fn parse_function

/// Structure parsing function
fn parse_structure<'t>(tokens: &'t [Token<'t>]) -> PResult<'t, &'t [Token<'t>], (&'t str, Declaration)> {
    comb::map(
        comb::all((
            parse::symbol(Symbol::Struct),
            parse::ident,
            parse::symbol(Symbol::CurlyBrOpen),
            comb::repeat_with_separator(
                comb::all((
                    comb::map(parse::ident, str::to_string),
                    parse::symbol(Symbol::Colon),
                    parse::ty,
                )),
                parse::symbol(Symbol::Comma),
                Vec::new,
                |mut vec, (name, _, ty)| {
                    vec.push((name, ty));
                    vec
                }
            ),
            parse::symbol(Symbol::CurlyBrClose),
        )),
        |(_, name, _, elements, _)| (
            name,
            Declaration::Structure { elements }
        )
    )(tokens)
} // fn parse_structure

/// Enumeration declaration parsing function
fn parse_enumeration<'t>(tokens: &'t [Token<'t>]) -> PResult<'t, &'t [Token<'t>], (&'t str, Declaration)> {
    comb::map(
        comb::all((
            parse::symbol(Symbol::Enum),
            parse::ident,
            parse::symbol(Symbol::CurlyBrOpen),
            comb::repeat_with_separator(
                comb::all((
                    comb::map(parse::ident, str::to_string),
                    comb::any((
                        comb::map(
                            comb::all((
                                parse::symbol(Symbol::Equal),
                                parse::integer_literal,
                            )),
                            |(_, v)| Some(v as usize)
                        ),
                        comb::map(
                            comb::identity,
                            |_| Option::<usize>::None,
                        ),
                    ))
                )),
                parse::symbol(Symbol::Comma),
                Vec::new,
                |mut vec, (name, explicit_index)| {
                    vec.push(EnumVariant { explicit_index, name });
                    vec
                }
            ),
            parse::symbol(Symbol::CurlyBrClose),
        )),
        |(_, name, _, variants, _)| (name, Declaration::Enumeration { variants })
    )(tokens)
} // fn parse_enumeration

impl Module {
    /// Module from token list parsing function
    pub fn parse(tokens: &[Token]) -> Option<Module> {
        let declaration = comb::any((
            parse_variable,
            parse_function,
            parse_structure,
            parse_enumeration,
        ));

        let declarations = comb::repeat(
            declaration,
            HashMap::<String, Declaration>::new,
            |mut hmap, (name, declaration)| {
                hmap.insert(name.to_string(), declaration);
                hmap
            }
        );

        Some(Module {
            declarations: declarations(tokens).ok()?.1,
        })
    } // fn parse
} // impl Module

// fn ast.rs
