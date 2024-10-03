mod builder;

use comb::PResult;
use crate::lexer::{Literal, Symbol, Token};
use super::{parse, BinaryOperator, Expression, Mutability, Type, UnaryOperator};

use builder::ExpressionAstBuilder;

/// Internal helper enum
enum IndexingOrCall {
    /// Is indexing
    Indexing,

    /// Is call
    Call,
} // enum IndexingOrCall

/// Indexing or function call postfix operator parsing function
fn parse_indexing_or_call<'t>(
    mut tl: &'t [Token<'t>]
) -> PResult<'t, &'t [Token<'t>], (IndexingOrCall, Vec<Expression>)> {
    let (indexing_or_call, expected_end) = match tl.get(0) {
        Some(Token::Symbol(Symbol::RoundBrOpen)) => (IndexingOrCall::Call, Symbol::RoundBrClose),
        Some(Token::Symbol(Symbol::SquareBrOpen)) => (IndexingOrCall::Indexing, Symbol::SquareBrClose),
        _ => return Err(tl),
    };

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
} // fn parse_indexing_or_call

/// Prefix unary operator parsing function
#[derive(Debug, Copy, Clone)]
pub enum PrefixUnaryOperator {
    /// Negation operator
    Minus,

    /// Unary addition operator
    Plus,

    /// Referencing operator
    Reference(Mutability),

    /// Pointer dereference operator
    Dereference,
} // enum PrefixUnaryOperator

/// Postfix unary operator representation structure
pub enum PostfixUnaryOperator {
    /// Indexing arguments
    Index(Vec<Expression>),

    /// Call arguments
    Call(Vec<Expression>),

    /// Casting to type operator
    Cast(Box<Type>),

    /// Access to field
    FieldAccess(String),

    /// Field access to dereferenced object
    DereferencedFieldAccess(String),

    /// Access to namespace
    NamespaceAccess(String),
} // enum PostfixUnaryOperator

/// Prefix operator parsing function
fn parse_prefix_operator<'t>(tl: &'t [Token<'t>]) -> PResult<'t, &'t [Token<'t>], PrefixUnaryOperator> {
    comb::any((
        // dereference
        comb::value(parse::symbol(Symbol::Asterisk), PrefixUnaryOperator::Dereference),

        // unary +
        comb::value(parse::symbol(Symbol::Plus), PrefixUnaryOperator::Plus),

        // unary -
        comb::value(parse::symbol(Symbol::Minus), PrefixUnaryOperator::Minus),

        // reference
        comb::map(
            comb::all((
                parse::symbol(Symbol::Ampersand),
                parse::mutability,
            )),
            |(_, mutability)| PrefixUnaryOperator::Reference(mutability)
        )
    ))(tl)
} // fn parse_prefix_operator

/// Postfix operator parsing function
fn parse_postfix_operator<'t>(tl: &'t [Token<'t>]) -> PResult<'t, &'t [Token<'t>], PostfixUnaryOperator> {
    comb::any((
        comb::map(
            parse_indexing_or_call,
            |(ioc, args)| {
                match ioc {
                    IndexingOrCall::Call => PostfixUnaryOperator::Index(args),
                    IndexingOrCall::Indexing => PostfixUnaryOperator::Call(args),
                }
            }
        ),
        comb::map(
            comb::all((
                parse::symbol(Symbol::As),
                parse::ty,
            )),
            |(_, ty)| PostfixUnaryOperator::Cast(Box::new(ty)),
        ),
        comb::map(
            comb::all((
                parse::symbol(Symbol::Dot),
                parse::ident,
            )),
            |(_, ident)| PostfixUnaryOperator::FieldAccess(ident.to_string()),
        ),
        comb::map(
            comb::all((
                parse::symbol(Symbol::Arrow),
                parse::ident,
            )),
            |(_, ident)| PostfixUnaryOperator::DereferencedFieldAccess(ident.to_string()),
        ),
        comb::map(
            comb::all((
                parse::symbol(Symbol::DoubleColon),
                parse::ident,
            )),
            |(_, ident)| PostfixUnaryOperator::NamespaceAccess(ident.to_string()),
        ),
    ))(tl)
} // fn parse_postfix_operator

/// Expression value parsing function
fn parse_expression_value<'t>(tl: &'t [Token<'t>]) -> PResult<'t, &'t [Token<'t>], Expression> {
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

        // Literal
        comb::map(
            parse::literal,
            |literal| match literal {
                Literal::Floating(fpn) => Expression::FloatingConstant(fpn),
                Literal::Integer(int) => Expression::IntegerConstant(int),
                Literal::String(str) => Expression::StringConstant(str),
                Literal::Char(chr) => Expression::CharacterConstant(chr),
            }
        ),

        // Array
        comb::map(
            comb::all((
                parse::symbol(Symbol::SquareBrOpen),
                comb::repeat_with_separator(
                    parse_expression,
                    parse::symbol(Symbol::Comma),
                    Vec::new,
                    |mut vec, expr| {
                        vec.push(expr);
                        vec
                    },
                ),
                parse::symbol(Symbol::SquareBrClose),
            )),
            |(_, elements, _)| Expression::Array { elements }
        ),

        // Structure
        comb::map(
            comb::all((
                parse::ident,
                parse::symbol(Symbol::CurlyBrOpen),
                comb::repeat_with_separator(
                    comb::all((
                        parse::ident,
                        parse::symbol(Symbol::Colon),
                        parse_expression,
                    )),
                    parse::symbol(Symbol::Comma),
                    Vec::new,
                    |mut vec, (name, _, initializer)| {
                        vec.push((name.to_string(), initializer));
                        vec
                    },
                ),
                parse::symbol(Symbol::CurlyBrClose),
            )),
            |(name, _, fields, _)| Expression::Structure {
                type_name: name.to_string(),
                fields
            }
        ),

        // Just ident, lol
        comb::map(
            parse::ident,
            |ident| Expression::Ident(ident.to_string()),
        ),
    ))(tl)
} // fn parse_expression_value

/// Expression parser
pub fn parse_expression<'t>(mut tl: &'t [Token<'t>]) -> PResult<'t, &'t [Token<'t>], Expression> {
    // (Toklist -> Inverse) parsing result
    let mut builder = ExpressionAstBuilder::new();

    #[derive(Copy, Clone, PartialEq, Eq)]
    enum ExpectedElement {
        Value,
        Operator,
    }
    let mut expected_element: ExpectedElement = ExpectedElement::Value;

    // Postfix parsing
    'expr_loop: loop {
        let token = match tl.get(0) {
            Some(tok) => tok,
            None => break 'expr_loop,
        };

        match expected_element {
            ExpectedElement::Value => {
                // TODO FIX THIS PERFORMANCE ZALUPA!!!
                let prefix_operators: Vec<_>;
                (tl, prefix_operators) = comb::collect_repeat(parse_prefix_operator)(tl)?;

                // parse actual value
                let mut value;
                (tl, value) = parse_expression_value(tl)?;

                // parse postifx operators
                let postfix_operators: Vec<_>;
                (tl, postfix_operators) = comb::collect_repeat(parse_postfix_operator)(tl)?;

                // chain prefix and postifx operators
                let ops = Iterator::chain(
                    postfix_operators
                        .into_iter()
                        .map(|op| match op {
                            PostfixUnaryOperator::Call(parameters) => UnaryOperator::Call(parameters),
                            PostfixUnaryOperator::Index(indices) => UnaryOperator::Index(indices),
                            PostfixUnaryOperator::Cast(ty) => UnaryOperator::Cast(ty),
                            PostfixUnaryOperator::FieldAccess(field) => UnaryOperator::FieldAccess(field),
                            PostfixUnaryOperator::NamespaceAccess(element) => UnaryOperator::NamespaceAccess(element),
                            PostfixUnaryOperator::DereferencedFieldAccess(field) => UnaryOperator::DereferencedFieldAccess(field),
                        }),
                    prefix_operators
                        .into_iter()
                        .rev()
                        .map(|op| match op {
                            PrefixUnaryOperator::Dereference => UnaryOperator::Dereference,
                            PrefixUnaryOperator::Minus => UnaryOperator::UnaryMinus,
                            PrefixUnaryOperator::Plus => UnaryOperator::UnaryPlus,
                            PrefixUnaryOperator::Reference(mutability) => UnaryOperator::Reference(mutability),
                        }),
                );

                // apply'em
                for op in ops {
                    value = Expression::UnaryOperator {
                        operand: Box::new(value),
                        operator: Box::new(op),
                    };
                }

                // Add expression to builder
                builder.push_expression(value);
                expected_element = ExpectedElement::Operator;
            }

            ExpectedElement::Operator => {
                let sym = match token {
                    Token::Symbol(symbol) => *symbol,
                    _ => break 'expr_loop,
                };
                let op = match BinaryOperator::from_symbol(sym) {
                    Some(v) => v,
                    None => break 'expr_loop,
                };

                tl = &tl[1..];
                builder.push_binary_operator(op);
                expected_element = ExpectedElement::Value;
            }
        }
    } // 'expr_loop

    Ok((
        tl,
        builder
            .build()
            .expect("Error finalizing expression AST")
    ))
} // fn parse_expression

// file expression.rs
