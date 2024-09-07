use std::collections::HashMap;

use comb::{PResult, Parser};

use crate::lexer::{Keyword, Literal, Symbol, Token};

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
} // enum Operator

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
        inputs: HashMap<String, Type>,
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

fn keyword<'t>(match_kw: Keyword) -> impl Parser<'t, &'t [Token], ()> {
    move |tl: &'t [Token]| -> PResult<'t, &'t [Token], ()> {
        if let Some(Token::Keyword(kw)) = tl.get(0) {
            if *kw == match_kw {
                return Ok((&tl[1..], ()))
            }
        }

        return Err(tl);
    }
}

fn symbol<'t>(match_sm: Symbol) -> impl Parser<'t, &'t [Token], ()> {
    move |tl: &'t [Token]| -> PResult<'t, &'t [Token], ()> {
        if let Some(Token::Symbol(sm)) = tl.get(0) {
            if *sm == match_sm {
                return Ok((&tl[1..], ()));
            }
        }

        return Err(tl);
    }
}

fn operator(tl: &[Token]) -> PResult<&[Token], Operator> {
    comb::any((
        comb::map(
            symbol(Symbol::Plus),
            |_| Operator::Add,
        ),
        comb::map(
            symbol(Symbol::Minus),
            |_| Operator::Sub,
        ),
        comb::map(
            symbol(Symbol::Asterisk),
            |_| Operator::Mul
        ),
        comb::map(
            symbol(Symbol::Slash),
            |_| Operator::Div,
        )
    ))(tl)
}

fn ident(tl: &[Token]) -> PResult<&[Token], &str> {
    if let Some(Token::Ident(str)) = tl.get(0) {
        Ok((&tl[1..], str.as_str()))
    } else {
        Err(tl)
    }
}

fn literal(tl: &[Token]) -> PResult<&[Token], Literal> {
    if let Some(Token::Literal(lit)) = tl.get(0) {
        Ok((&tl[1..], *lit))
    } else {
        Err(tl)
    }
}

fn parse_type(tl: &[Token]) -> PResult<&[Token], Type> {
    let (new_tl, ident) = ident(tl)?;

    match ident {
        "f32" => Ok((new_tl, Type::F32)),
        "i32" => Ok((new_tl, Type::I32)),
        "i64" => Ok((new_tl, Type::I64)),
        "f64" => Ok((new_tl, Type::F64)),

        _ => Err(tl),
    }
}

pub fn expresson(tl: &[Token]) -> PResult<&[Token], Expression> {

    todo!()
}

impl Module {
    pub fn parse(tokens: &[Token]) -> Option<Module> {
        let variable = comb::map(
            comb::all((
                keyword(Keyword::Let),
                ident,
                symbol(Symbol::Colon),
                parse_type,
                comb::any((
                    comb::map(
                        comb::all((
                            symbol(Symbol::Equal),
                            literal,
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
                symbol(Symbol::Semicolon),
            )),
            |(_, name, _, ty, initializer, _)| (name, Declaration::Variable { ty, initializer })
        );

        let function = comb::map(
            comb::all((
                keyword(Keyword::Fn),
                ident,
                symbol(Symbol::RoundBrOpen),
                comb::repeat_with_separator(
                    comb::all((
                        ident,
                        symbol(Symbol::Colon),
                        parse_type,
                    )),
                    symbol(Symbol::Comma),
                    HashMap::new,
                    |mut hmap, (name, _, type_name)| {
                        hmap.insert(name.to_string(), type_name);
                        hmap
                    }
                ),
                symbol(Symbol::RoundBrClose),
                parse_type,
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
