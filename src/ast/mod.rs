mod parse;

use std::collections::HashMap;

use crate::lexer::{Keyword, Literal, Symbol, Token};

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
}

/// Binary operation representation structure
#[derive(Debug)]
pub struct BinaryOperation {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug)]
pub enum Value {
    Integer(u64),
    Floating(f64),
    Variable(String),
}

#[derive(Debug)]
pub enum Type {
    I32,
    F32,
    I64,
    F64,
}

#[derive(Debug)]
pub enum Expression {
    Value(Value),
    BinaryOperation(BinaryOperation),
    UnaryOperation(UnaryOperation),
}

#[derive(Debug)]
pub enum Declaration {
    Function {
        inputs: Vec<(String, Type)>,
        output: Type,
    },
    Variable {
        ty: Type,
        initializer: Option<Expression>,
    }
}

#[derive(Debug)]
pub struct Module {
    pub declarations: HashMap<String, Declaration>,
}

impl Module {
    pub fn parse(tokens: &[Token]) -> Option<Module> {
        let variable = comb::map(
            comb::all((
                parse::keyword(Keyword::Let),
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

        let function = comb::map(
            comb::all((
                parse::keyword(Keyword::Fn),
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
