//! Abstract Syntax Tree implementation module

mod parse;
mod expression;
mod operator;
mod display;

pub use operator::*;

use comb::PResult;
use crate::lexer;
use expression::parse_expression;

/// Variable mutability
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Mutability {
    /// Constant (can be calculated in compiletime)
    Const,

    /// Mutable (may be changed)
    Mut,
} // enum Mutability

/// Prefix/Postifx unary operator representation enumeration
#[derive(Clone, Debug)]
pub enum UnaryOperator {
    /// Casting operator
    Cast(Box<Type>),

    /// Indexing operator
    Index(Vec<Expression>),

    /// Call operator
    Call(Vec<Expression>),

    /// Referencing operator
    Reference(Mutability),

    /// Dereferencing operator
    Dereference,

    /// Unary + operator
    UnaryPlus,

    /// Unary minus operator
    UnaryMinus,

    /// Object field access
    FieldAccess(String),

    /// Dereferenced field access
    DereferencedFieldAccess(String),

    /// Namespace access
    NamespaceAccess(String),
} // enum UnaryOperator

/// Expression representation structure
#[derive(Clone, Debug)]
pub enum Expression {
    /// Just ident
    Ident(String),

    /// Integer literal
    IntegerConstant {
        number: u64,
        postfix: String,
    },

    /// Floating point literal
    FloatingConstant {
        number: f64,
        postfix: String,
    },

    /// String literal
    StringConstant(String),

    /// Character literal
    CharacterConstant(char),

    /// Array initializer
    Array {
        /// Array initializer
        elements: Vec<Expression>,
    },

    /// Structure initializer
    Structure {
        /// Structure type name
        type_name: String,

        /// Fields
        fields: Vec<(String, Expression)>,
    },

    /// Unary operator
    UnaryOperator {
        /// Operand
        operand: Box<Expression>,
        /// Operator (yes, it's too large to use it without box)
        operator: Box<UnaryOperator>,
    },

    /// Binary operator
    BinaryOperator {
        /// Left hand side
        lhs: Box<Expression>,

        /// Right hand side
        rhs: Box<Expression>,

        /// Operator, actually
        operator: BinaryOperator,
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
    /// Declarative statement
    Declarative {
        /// Object name
        name: String,

        /// Declaration
        declaration: Declaration,
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

    /// Block
    Block {
        /// Block code
        code: Box<Block>,
    },
} // enum Statement

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

    /// Character type
    Char,

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
            "char"  => Some(PrimitiveType::Char ),
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
        mutability: Mutability,
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

/// Function AST representaiton structure
#[derive(Clone, Debug)]
pub struct Function {
    /// Function input types
    pub inputs: Vec<(String, Mutability, Type)>,

    /// Function output type
    pub output: Box<Type>,

    /// Function code
    pub implementation: Option<Box<Block>>,
} // struct Function

/// Variable AST (global and in-block) representation structure
#[derive(Clone, Debug)]
pub struct Variable {
    /// Type
    pub ty: Box<Type>,

    /// Mutability
    pub mutability: Mutability,

    /// Initializer expression
    pub initializer: Option<Box<Expression>>,
} // struct Variable

/// Structure AST representation structure
#[derive(Clone, Debug)]
pub struct Structure {
    /// Structure element names and types
    pub elements: Vec<(String, Type)>,
} // struct Structure

/// Enumeration AST representation structure
#[derive(Clone, Debug)]
pub struct Enumeration {
    /// Enumeration variants (name - initializer)
    pub variants: Vec<(String, Option<Box<Expression>>)>,
} // struct Enumeration

/// Declaration representation enumeration
#[derive(Clone, Debug)]
pub enum Declaration {
    /// Function
    Function(Function),

    /// Variable (ded uebet)
    Variable(Variable),

    /// Structure
    Structure(Structure),

    /// Enumeration
    Enumeration(Enumeration),
} // enum Declaration

/// Module representation structure
#[derive(Debug)]
pub struct Module {
    /// Declaration set
    pub declarations: Vec<(String, Declaration)>,
} // struct Module

/// Variable parsing function
fn parse_variable<'t>(tl: &'t [lexer::Token<'t>]) -> PResult<'t, &'t [lexer::Token<'t>], (&'t str, Variable)> {
    comb::map(
        comb::all((
            parse::symbol(lexer::Symbol::Let),
            parse::mutability,
            parse::ident,
            parse::symbol(lexer::Symbol::Colon),
            parse::ty,
            comb::any((
                comb::map(
                    comb::all((
                        parse::symbol(lexer::Symbol::Equal),
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
            parse::symbol(lexer::Symbol::Semicolon),
        )),
        |(_, mutability, name, _, ty, initializer, _)| (name, Variable {
            ty: Box::new(ty),
            initializer: initializer,
            mutability,
        })
    )(tl)
} // fn parse_variable

fn parse_block<'t>(tl: &'t [lexer::Token<'t>]) -> PResult<'t, &'t [lexer::Token<'t>], Block> {
    comb::map(
        comb::all((
            parse::symbol(lexer::Symbol::CurlyBrOpen),
            comb::repeat(
                parse_statement,
                Vec::new,
                |mut vec, stmt| {
                    vec.push(stmt);
                    vec
                }
            ),
            parse::symbol(lexer::Symbol::CurlyBrClose),
        )),
        |(_, statements, _)| Block {
            statements,
            result_expression: None,
        }
    )(tl)
}

fn parse_statement<'t>(tl: &'t [lexer::Token<'t>]) -> PResult<'t, &'t [lexer::Token<'t>], Statement> {
    let stmt_if_else = comb::map(
        comb::all((
            parse::symbol(lexer::Symbol::If),
            parse_expression,
            parse_block,
            comb::or(
                comb::map(
                    comb::all((
                        parse::symbol(lexer::Symbol::Else),
                        parse_block,
                    )),
                    |(_, block)| Some(block),
                ),
                || Option::<Block>::None,
            ),
        )),
        |(_, condition, then_code, else_code)| Statement::If {
            condition: Box::new(condition),
            then_code: Box::new(then_code),
            else_code: else_code.map(Box::new),
        }
    );

    let stmt_while = comb::map(
        comb::all((
            parse::symbol(lexer::Symbol::While),
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
            parse::symbol(lexer::Symbol::Semicolon),
        )),
        |(expr, _)| Statement::Expression { expr: Box::new(expr) }
    );

    let stmt_decl = comb::map(
        parse_declaration,
        |(name, declaration)| Statement::Declarative {
            name: name.to_string(),
            declaration,
        }
    );

    let stmt_return = comb::map(
        comb::all((
            parse::symbol(lexer::Symbol::Return),
            parse_expression,
            parse::symbol(lexer::Symbol::Semicolon),
        )),
        |(_, expression, _)| Statement::Return {
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
        stmt_decl,
        stmt_expr,
        stmt_block,
    ))(tl)
} // fn parse_statement

/// Function parsing function
fn parse_function<'t>(tokens: &'t [lexer::Token<'t>]) -> PResult<'t, &'t [lexer::Token<'t>], (&'t str, Function)> {
    comb::map(
        comb::all((
            parse::symbol(lexer::Symbol::Fn),
            parse::ident,
            parse::symbol(lexer::Symbol::RoundBrOpen),
            comb::repeat_with_separator(
                comb::all((
                    parse::mutability,
                    parse::ident,
                    parse::symbol(lexer::Symbol::Colon),
                    parse::ty,
                )),
                parse::symbol(lexer::Symbol::Comma),
                Vec::new,
                |mut vec, (mutability, name, _, type_name)| {
                    vec.push((name.to_string(), mutability, type_name));
                    vec
                }
            ),
            parse::symbol(lexer::Symbol::RoundBrClose),
            comb::map(
                comb::or(parse::ty, || Type::Primitive(PrimitiveType::Void)),
                Box::new
            ),
            comb::any((
                comb::map(
                    parse::symbol(lexer::Symbol::Semicolon),
                    |_| None,
                ),
                comb::map(
                    parse_block,
                    |code| Some(Box::new(code)),
                ),
            )),
        )),
        |(_, name, _, inputs, _, output, implementation)| (name, Function {
            inputs,
            output,
            implementation,
        })
    )(tokens)
} // fn parse_function

/// Structure parsing function
fn parse_structure<'t>(tokens: &'t [lexer::Token<'t>]) -> PResult<'t, &'t [lexer::Token<'t>], (&'t str, Structure)> {
    comb::map(
        comb::all((
            parse::symbol(lexer::Symbol::Struct),
            parse::ident,
            parse::symbol(lexer::Symbol::CurlyBrOpen),
            comb::repeat_with_separator(
                comb::all((
                    comb::map(parse::ident, str::to_string),
                    parse::symbol(lexer::Symbol::Colon),
                    parse::ty,
                )),
                parse::symbol(lexer::Symbol::Comma),
                Vec::new,
                |mut vec, (name, _, ty)| {
                    vec.push((name, ty));
                    vec
                }
            ),
            parse::symbol(lexer::Symbol::CurlyBrClose),
        )),
        |(_, name, _, elements, _)| (name, Structure { elements })
    )(tokens)
} // fn parse_structure

/// Enumeration declaration parsing function
fn parse_enumeration<'t>(tokens: &'t [lexer::Token<'t>]) -> PResult<'t, &'t [lexer::Token<'t>], (&'t str, Enumeration)> {
    comb::map(
        comb::all((
            parse::symbol(lexer::Symbol::Enum),
            parse::ident,
            parse::symbol(lexer::Symbol::CurlyBrOpen),
            comb::repeat_with_separator(
                comb::all((
                    comb::map(parse::ident, str::to_string),
                    comb::or(
                        comb::map(
                            comb::all((
                                parse::symbol(lexer::Symbol::Equal),
                                parse_expression,
                            )),
                            |(_, v)| Some(Box::new(v))
                        ),
                        || Option::<Box<Expression>>::None,
                    )
                )),
                parse::symbol(lexer::Symbol::Comma),
                Vec::new,
                |mut vec, (name, explicit_index)| {
                    vec.push((name, explicit_index));
                    vec
                }
            ),
            parse::symbol(lexer::Symbol::CurlyBrClose),
        )),
        |(_, name, _, variants, _)| (
            name,
            Enumeration { variants }
        )
    )(tokens)
} // fn parse_enumeration

fn parse_declaration<'t>(
    tl: &'t [lexer::Token<'t>]
) -> PResult<'t, &'t [lexer::Token<'t>], (&'t str, Declaration)> {

    comb::any((
        comb::map(
            parse_function,
            |(name, func)| (name, Declaration::Function(func))
        ),
        comb::map(
            parse_variable,
            |(name, var)| (name, Declaration::Variable(var))
        ),
        comb::map(
            parse_structure,
            |(name, str)| (name, Declaration::Structure(str))
        ),
        comb::map(
            parse_enumeration,
            |(name, en)| (name, Declaration::Enumeration(en))
        ),
    ))(tl)
}

impl Module {
    /// Module from token list parsing function
    pub fn parse(tokens: &[lexer::Token]) -> Option<Module> {

        let declarations = comb::collect_repeat(
            comb::map(
                parse_declaration,
                |(n, d)| (n.to_string(), d)
            )
        );

        Some(Module {
            declarations: declarations(tokens).ok()?.1,
        })
    } // fn parse
} // impl Module

// fn ast.rs
