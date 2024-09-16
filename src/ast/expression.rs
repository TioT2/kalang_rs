use comb::PResult;
use crate::lexer::{Symbol, Token};
use super::{parse, UnaryOperator, BinaryOperator, Expression, Mutability, Type};

/// AST building helper
struct ExpressionAstBuilder {
    /// Output stack
    values: Vec<Expression>,

    /// Operators
    operators: Vec<BinaryOperator>,
} // struct ExpressionAstBuilder

impl ExpressionAstBuilder {
    /// Constructor
    pub fn new() -> Self {
        Self {
            operators: Vec::new(),
            values: Vec::new()
        }
    } // fn new

    /// Binary operator pushing function
    pub fn push_binary_operator(&mut self, op: BinaryOperator) -> Option<()> {
        let info = op.info();

        'operator_parsing: while let Some(other) = self.operators.last().cloned() {
            let other_info = other.info();

            let assoc = if info.is_left_assoc {
                other_info.priority <= info.priority
            } else {
                other_info.priority < info.priority
            };

            if assoc {
                let expr = Expression::BinaryOperator {
                    rhs: Box::new(self.values.pop()?),
                    lhs: Box::new(self.values.pop()?),
                    operator: other,
                };
                self.values.push(expr);
                self.operators.pop();
            } else {
                break 'operator_parsing;
            }
        }

        self.operators.push(op);

        Some(())
    } // fn push_binary_operator

    /// Expression (actually, value) pushing function
    pub fn push_expression(&mut self, value: Expression) {
        self.values.push(value);
    } // fn push_expression

    /// Final expression getting function
    pub fn build(self) -> Option<Expression> {
        let mut values = self.values;

        for op in self.operators.into_iter().rev() {
            let rhs = values.pop()?;
            let lhs = values.pop()?;

            values.push(Expression::BinaryOperator {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                operator: op
            })
        }

        if values.len() == 1 {
            Some(values.remove(0))
        } else {
            None
        }
    } // fn build
} // impl ExpressionAstBuilder

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
) -> PResult<'t, &'t [Token], (IndexingOrCall, Vec<Expression>)> {
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
} // enum PostfixUnaryOperator

/// Prefix operator parsing function
fn parse_prefix_operator<'t>(tl: &'t [Token<'t>]) -> PResult<'t, &'t [Token<'t>], PrefixUnaryOperator> {
    comb::any((
        // dereference
        comb::map(
            parse::symbol(Symbol::Asterisk),
            |_| PrefixUnaryOperator::Dereference,
        ),

        // unary +
        comb::map(
            parse::symbol(Symbol::Plus),
            |_| PrefixUnaryOperator::Plus,
        ),

        // unary -
        comb::map(
            parse::symbol(Symbol::Minus),
            |_| PrefixUnaryOperator::Minus,
        ),

        // reference
        comb::map(
            comb::all((
                parse::symbol(Symbol::Ampersand),
                comb::or(
                    comb::any((
                        comb::map(parse::symbol(Symbol::Mut), |_| Mutability::Mut),
                        comb::map(parse::symbol(Symbol::Const), |_| Mutability::Const),
                    )),
                    || Mutability::Const
                ),
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
        )
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
            |literal| Expression::Literal { literal }
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
            |(_, initializer, _)| Expression::Array { initializer }
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
                name: name.to_string(),
                fields
            }
        ),

        // Just ident, lol
        comb::map(
            parse::ident,
            |ident| Expression::Ident { ident: ident.to_string() },
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
