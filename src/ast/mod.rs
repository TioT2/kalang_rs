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

pub struct BlockCode {
    pub expressions: Vec<RawExpression>,
}

pub enum BlockType {
    While {
        condition: Box<RawExpression>,
    },
    IfElse {
        condition: Box<RawExpression>,
    }
}

pub struct Block {
    pub blocks: Vec<Block>,
}

/// Non-parsed expression representation structure
pub enum RawExpressionElement {
    /// Operator
    Operator(Operator),

    /// Literal or variable reference
    Literal(Literal),

    /// Sub expression (expression in parentheses)
    SubExpression(Box<RawExpression>),

    /// Sub expression (expression in square brackets)
    IndexingOperator(Box<RawExpression>),
} // enum RawExpressionElement

/// Non-parsed expression representation structure
pub struct RawExpression {
    /// Set of expression elements
    pub elements: Vec<RawExpressionElement>,
} // struct RawExpression

/// Unary operation representation structure
#[derive(Debug)]
pub struct UnaryOperation {
    /// Operand, actually
    pub operand: Box<Expression>,
} // struct UnaryOperation

/// Binary operation representation structure
#[derive(Debug)]
pub struct BinaryOperation {
    /// Left Hand Side
    pub lhs: Box<Expression>,

    /// Right Hand Side
    pub rhs: Box<Expression>,
} // struct BinaryOperation

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
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    F32,
    I64,
    U64,
    F64,

    Void,
    Never,
}

impl PrimitiveType {
    pub fn parse(str: &str) -> Option<PrimitiveType> {
        match str {
            "i8"   => Some(PrimitiveType::I8 ),
            "u8"   => Some(PrimitiveType::U8 ),
            "i16"  => Some(PrimitiveType::I16),
            "u16"  => Some(PrimitiveType::U16),
            "i32"  => Some(PrimitiveType::I32),
            "u32"  => Some(PrimitiveType::U32),
            "f32"  => Some(PrimitiveType::F32),
            "i64"  => Some(PrimitiveType::I64),
            "u64"  => Some(PrimitiveType::U64),
            "f64"  => Some(PrimitiveType::F64),
            "void" => Some(PrimitiveType::Void),
            "!"    => Some(PrimitiveType::Never),
            _ => None,
        }
    }
}

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

/// Expression tree element representation enumeration
#[derive(Debug)]
pub enum Expression {
    /// Value
    Value(Value),

    /// Binary operation
    BinaryOperation(BinaryOperation),

    /// Unary operation
    UnaryOperation(UnaryOperation),
}

#[derive(Clone, Debug)]
pub struct EnumVariant {
    pub name: String,
    pub explicit_index: Option<usize>,
}


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
        initializer: Option<Expression>,
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

fn parse_expression<'t>(tokens: &'t [Token<'t>]) -> PResult<'t, &'t [Token<'t>], RawExpression> {
    todo!()
}

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
                        parse::literal,
                    )),
                    |(_, val)| Some(Expression::Value(match val {
                        Literal::Floating(flt) => Value::Floating(flt),
                        Literal::Integer(int) => Value::Integer(int),
                    }))
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
}

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
}

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
}

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
                                comb::filter_map(
                                    parse::literal,
                                    |literal| match literal {
                                        Literal::Integer(i) => Some(i as usize),
                                        _ => None,
                                    }
                                )
                            )),
                            |(_, v)| Some(v)
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
}

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
}

// fn ast.rs
