use std::collections::HashMap;

use comb::{all, any, filter, map, repeat, repeat_with_separator, PResult, Parser};

#[derive(Debug)]
pub struct UnaryOperation {
    pub operand: Box<Expression>,
}

#[derive(Debug)]
pub struct BinaryOperation {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug)]
pub enum Value {
    Integer(i64),
    Floating(f64),
    Variable(String),
}

#[derive(Debug)]
pub enum Type {
    I32,
    F32,
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
        code: Expression,
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
    pub fn parse(source: &str) -> Option<Module> {
        fn ty<'t>(str: &'t str) -> PResult<'t, Type> {
            any((
                map(comb::literal("i32"), |_| Type::I32),
                map(comb::literal("f32"), |_| Type::F32),
            ))(str)
        }

        fn ident<'t>(str: &'t str) -> PResult<'t, String> {
            repeat(
                filter(comb::any_char, |v| v.is_alphabetic()),
                String::new,
                |mut string, char| {
                    string.push(char);
                    string
                }
            )(str)
        }

        fn whitespace<'t>(source: &'t str) -> PResult<'t, ()> {
            repeat(
                comb::any_whitespace,
                || (),
                |_, _| ()
            )(source)
        }

        fn surround_whitespace<'t, T>(parser: impl Parser<'t, T>) -> impl Parser<'t, T> {
            map(
                all((
                    whitespace,
                    parser,
                    whitespace,
                )),
                |(_, v, _)| v,
            )
        }

        let variable = map(
            all((
                comb::literal("let"),
                surround_whitespace(ident),
                comb::literal(":"),
                surround_whitespace(ty),
                comb::literal(";"),
            )),
            |(_, name, _, ty, _)| (name, Declaration::Variable {
                ty,
                initializer: None,
            }),
        );

        let function = map(
            all((
                comb::literal("fn"),
                surround_whitespace(ident),
                comb::literal("("),
                whitespace,
                repeat_with_separator(
                    map(
                        all((
                            ident,
                            surround_whitespace(comb::literal(":")),
                            ty,
                        )),
                        |(name, _, ty)| (name, ty),
                    ),
                    surround_whitespace(comb::literal(",")),
                    HashMap::new,
                    |mut map, (name, ty)| {
                        map.insert(name, ty);
                        map
                    }
                ),
                surround_whitespace(comb::literal(")")),
                map(
                    all((
                        comb::literal("->"),
                        whitespace,
                        ty
                    )),
                    |(_, _, v)| v
                )
            )),
            |(_, name, _, _, inputs, _, output)| (
                name,
                Declaration::Function {
                    code: Expression::Value(Value::Floating(-47.2)),
                    inputs,
                    output,
                }
            ),
        );

        let declaration = any((
            surround_whitespace(variable),
            surround_whitespace(function),
        ));

        let module = map(
            repeat(
                declaration,
                HashMap::new,
                |mut declarations, (name, declaration)| {
                    declarations.insert(name, declaration);
                    declarations
                }
            ),
            |declarations| Module {
                declarations,
            }
        );

        module(source)
            .ok()
            .map(|(_, v)| v)
    }
}

// fn ast.rs
