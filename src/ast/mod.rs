
mod parse;
mod expression;
mod operator;

pub use operator::*;

use std::collections::HashMap;
use comb::PResult;
use crate::lexer::{Literal, Symbol, Token};
use expression::parse_expression;

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
#[derive(Clone, Debug)]
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
        code: Box<Block>,
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

/// Block parsing function
#[derive(Clone, Debug)]
pub struct Block {
    /// Statement set
    pub statements: Vec<Statement>,

    /// Last block expression
    pub result_expression: Option<Box<Expression>>,
} // struct Block

/// The core object of AST - statement
#[derive(Clone, Debug)]
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
        initializer: Option<Box<Expression>>,
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
        code: Box<Block>,
    },

    /// If statement
    If {
        /// Condition
        condition: Box<Expression>,

        /// Code executed then condition -> true
        then_code: Box<Block>,

        /// Code executed then condition -> false
        else_code: Option<Box<Block>>,
    },

    /// Function return statement
    Return {
        /// Resulting expression
        expression: Box<Expression>,
    },

    Block {
        code: Box<Block>,
    }
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
        output: Box<Type>,
    },

    /// Variable (ded uebet)
    Variable {
        /// Type
        ty: Box<Type>,

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

pub struct VariableInfo<'t> {
    pub name: &'t str,
    pub mutability: Mutability,
    pub ty: Box<Type>,
    pub initializer: Option<Box<Expression>>,
}

/// Variable parsing function
fn parse_variable<'t>(tl: &'t [Token<'t>]) -> PResult<'t, &'t [Token<'t>], VariableInfo<'t>> {
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
        |(_, name, _, ty, initializer, _)| VariableInfo {
            name,
            ty: Box::new(ty),
            initializer: initializer,
            mutability: Mutability::Mut,
        }
    )(tl)
} // fn parse_variable

fn parse_block<'t>(tl: &'t [Token<'t>]) -> PResult<'t, &'t [Token<'t>], Block> {
    comb::map(
        comb::all((
            parse::symbol(Symbol::CurlyBrOpen),
            comb::repeat(
                parse_statement,
                Vec::new,
                |mut vec, stmt| {
                    vec.push(stmt);
                    vec
                }
            ),
            parse::symbol(Symbol::CurlyBrClose),
        )),
        |(_, statements, _)| Block {
            statements,
            result_expression: None,
        }
    )(tl)
}

fn parse_statement<'t>(tl: &'t [Token<'t>]) -> PResult<'t, &'t [Token<'t>], Statement> {
    let stmt_if_else = comb::map(
        comb::all((
            parse::symbol(Symbol::If),
            parse_expression,
            parse_block,
            comb::any((
                comb::map(
                    comb::all((
                        parse::symbol(Symbol::Else),
                        parse_block
                    )),
                    |(_, block)| Some(block),
                ),
                comb::map(comb::identity, |_| None)
            )),
        )),
        |(_, condition, then_code, else_code)| Statement::If {
            condition: Box::new(condition),
            then_code: Box::new(then_code),
            else_code: else_code.map(Box::new),
        }
    );

    let stmt_while = comb::map(
        comb::all((
            parse::symbol(Symbol::While),
            parse_expression,
            parse_block,
        )),
        |(_, condition, code)| Statement::While {
            condition: Box::new(condition),
            code: Box::new(code),
        }
    );

    let stmt_expr = comb::map(
        comb::all((
            parse_expression,
            parse::symbol(Symbol::Semicolon),
        )),
        |(expr, _)| Statement::Expression { expr: Box::new(expr) }
    );

    let stmt_let = comb::map(
        parse_variable,
        |info| Statement::Let {
            variable_name: info.name.to_string(),
            mutability: info.mutability,
            ty: info.ty,
            initializer: info.initializer,
        }
    );

    let stmt_return = comb::map(
        comb::all((
            parse::symbol(Symbol::Return),
            parse_expression,
        )),
        |(_, expression)| Statement::Return {
            expression: Box::new(expression)
        }
    );

    let stmt_block = comb::map(
        parse_block,
        |code| Statement::Block {
            code: Box::new(code),
        }
    );

    comb::any((
        stmt_if_else,
        stmt_while,
        stmt_return,
        stmt_let,
        stmt_expr,
        stmt_block,
    ))(tl)
}

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
            comb::map(parse::ty, Box::new),
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
            parse_function,
            comb::map(
                parse_variable,
                |info| (info.name, Declaration::Variable {
                    ty: info.ty,
                    initializer: info.initializer,
                }
            )),
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
