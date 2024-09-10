mod parse;

use std::collections::HashMap;

use comb::PResult;

use crate::lexer::{Literal, Symbol, Token};

pub enum OperatorInfo {
    Binary {
        is_left_assoc: bool,
        priority: u8,
    },
    BrOpen,
    BrClose,
    Unary {
        is_prefix: bool,
    },
}

/// Mathematical operation representation structure
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operator {
    /// Value ignore postfix unary operator
    Ignore,

    /// Addition
    Add,

    /// Substraction
    Sub,

    /// Multiplication
    Mul,

    /// Division
    Div,

    /// Addition with assignment
    AddAssign,

    /// Substraction with assignment
    SubAssign,

    /// Multiplication with assignment
    MulAssign,

    /// Division with assignment
    DivAssign,
} // enum Operator

// pub enum Statement {
//     Const {
//         name: String,
//         ty: Box<Type>,
//         initializer: RawExpression,
//     },
//     Let {
//         variable_name: String,
//         is_mutable: bool,
//         ty: Box<Type>,
//         initializer: RawExpression,
//     },
//     Fn {
//         name: String,
//         inputs: Vec<(String, Type)>,
//         output: Box<Type>,
//         code: Vec<Statement>,
//     },
//     Expression {
//         expr: RawExpression,
//     },
// }

/// Non-parsed expression representation structure
#[derive(Debug, Clone)]
pub enum RawExpressionElement {
    /// Operator
    Operator(Operator),

    /// Literal
    Literal(Literal),

    /// Variable reference
    Reference(String),

    /// Sub expression (expression in parentheses)
    SubExpression(Box<RawExpression>),

    /// Sub expression (expression in square brackets)
    IndexingOperator(Box<RawExpression>),
} // enum RawExpressionElement

/// Non-parsed expression representation structure
#[derive(Debug, Clone)]
pub struct RawExpression {
    /// Set of expression elements
    pub elements: Vec<RawExpressionElement>,
} // struct RawExpression

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

#[derive(Debug, Copy, Clone, PartialEq)]
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
    Named(String),
    Primitive(PrimitiveType),
    Array {
        element_type: Box<Type>,
        size: usize,
    },
    Pointer {
        element_type: Box<Type>,
        is_mutable: bool,
    },
    Tuple {
        element_types: Vec<Type>,
    },
    FunctionPointer {
        inputs: Vec<Type>,
        output: Box<Type>,
    },
}

/// Enumeration variant representaion structure
#[derive(Clone, Debug)]
pub struct EnumVariant {
    /// Variant name
    pub name: String,

    /// Explicit variant index
    pub explicit_index: Option<usize>,
} // struct EnumVariant

/// Some declaration representation enumeration (enum and struct will be added later)
#[derive(Debug)]
pub enum Declaration {
    /// Function
    Function {
        /// Input set
        inputs: Vec<(String, Type)>,

        /// return value
        output: Type,
    },
    /// Global variable (ded uebet)
    Variable {
        /// Type
        ty: Type,

        /// Initializer expression
        initializer: Option<RawExpression>,
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

/// Expression parsing function
/// * `tokens` - token set
/// * Returns parsing result.
fn parse_raw_expression<'t>(tl: &'t [Token<'t>]) -> PResult<'t, &'t [Token<'t>], RawExpression> {
    // At least now this function can handle only literal...
    comb::map(
        parse::literal,
        |lit| RawExpression {
            elements: vec![RawExpressionElement::Literal(lit)]
        }
    )(tl)
} // fn parse_raw_expression

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
                        parse_raw_expression,
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
    }
} // fn parse

// fn ast.rs
