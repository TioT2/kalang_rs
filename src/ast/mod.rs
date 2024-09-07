mod parse;

use std::collections::HashMap;

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

    /// Addition with assignment
    AddAssign,

    /// Substraction with assignment
    SubAssign,

    /// Multiplication with assignment
    MulAssign,

    /// Division with assignment
    DivAssign,
} // enum Operator

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

/// Type representation enumeration (MUST be fully reworked later)
#[derive(Debug)]
pub enum Type {
    /// 32-bit integer
    I32,

    /// 32-bit FP number
    F32,

    /// 64-bit integer
    I64,

    /// 64-bit FP number
    F64,
} // enum Type

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
    }
} // enum Declaration

/// Module representation structure
#[derive(Debug)]
pub struct Module {
    /// Declaration set
    pub declarations: HashMap<String, Declaration>,
} // struct Module

impl Module {
    /// Module from token list parsing function
    pub fn parse(tokens: &[Token]) -> Option<Module> {
        // Variable declaration parser
        let variable = comb::map(
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
        );

        // Function declaration parser
        let function = comb::map(
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
        );

        let declaration = comb::any((
            variable,
            function,
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
