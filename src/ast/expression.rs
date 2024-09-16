use comb::PResult;

use crate::lexer::{Symbol, Token};

use super::{parse, Expression, Operator, OperatorInfo};

struct ExpressionAstBuilder {
    values: Vec<Expression>,
    operators: Vec<Operator>,
}

impl ExpressionAstBuilder {
    pub fn new() -> Self {
        Self {
            operators: Vec::new(),
            values: Vec::new()
        }
    }

    pub fn push_operator(&mut self, op: Operator) -> Option<()> {
        let (priority, is_left_assoc) = match op.info() {
            OperatorInfo::Binary { priority, is_left_assoc } => (priority, is_left_assoc),
            OperatorInfo::Unary => {
                self.operators.push(op);
                return Some(());
            },
        };

        'operator_parsing: while let Some(other) = self.operators.last().cloned() {
            let other_info = other.info();

            match other_info {
                OperatorInfo::Binary { priority: other_priority, is_left_assoc: _ } => {
                    let assoc = if is_left_assoc {
                        other_priority <= priority
                    } else {
                        other_priority < priority
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
                OperatorInfo::Unary => {
                    let expr = Expression::UnaryOperator {
                        operand: Box::new(self.values.pop()?),
                        operator: other,
                    };
                    self.values.push(expr);
                    self.operators.pop();
                }
            }
        }

        self.operators.push(op);

        Some(())
    }

    pub fn push_expression(&mut self, val: Expression) {
        self.values.push(val);
    }

    pub fn build(self) -> Option<Expression> {
        let mut values = self.values;

        for op in self.operators.into_iter().rev() {
            let info = op.info();

            match info {
                OperatorInfo::Binary { .. } => {
                    let rhs = values.pop()?;
                    let lhs = values.pop()?;

                    values.push(Expression::BinaryOperator {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        operator: op
                    })
                }
                OperatorInfo::Unary => {
                    let operand = values.pop()?;
                    values.push(Expression::UnaryOperator {
                        operand: Box::new(operand),
                        operator: op
                    })
                }
            }
        }

        if values.len() == 1 {
            Some(values.remove(0))
        } else {
            None
        }
    }
}

/// Internal helper enum
enum IndexingOrCall {
    /// Is indexing
    Indexing,
    /// Is call
    Call,
}

/// Indexing or function call
fn parse_indexing_or_call<'t>(mut tl: &'t [Token<'t>]) -> PResult<'t, &'t [Token], (IndexingOrCall, Vec<Expression>)> {
    let (indexing_or_call, expected_end) = match tl.get(0) {
        Some(Token::Symbol(Symbol::RoundBrOpen)) => (IndexingOrCall::Call, Symbol::RoundBrClose),
        Some(Token::Symbol(Symbol::SquareBrOpen)) => (IndexingOrCall::Indexing, Symbol::SquareBrClose),
        _ => return Err(tl),
    };

    // parse set of subexpressions
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
}

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

        match token {
            // Value
            Token::Symbol(Symbol::RoundBrOpen) | Token::Ident(_) | Token::Literal(_) => {
                if expected_element != ExpectedElement::Value {
                    break 'expr_loop;
                }

                // parse subexpression
                let subexpr;
                let indexing_or_call;

                (tl, (subexpr, indexing_or_call)) = comb::all((
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

                        // Just ident, lol
                        comb::map(
                            parse::ident,
                            |ident| Expression::Ident { ident: ident.to_string() },
                        ),

                        comb::map(
                            parse::literal,
                            |literal| Expression::Literal { literal }
                        ),
                    )),
                    comb::any((
                        // Indexing/Call
                        comb::map(parse_indexing_or_call, |v| Some(v)),

                        // Default case
                        comb::map(comb::identity, |_| None),
                    ))
                ))(tl)?;

                let expr = match indexing_or_call {

                    // Subexpression was indexed
                    Some((IndexingOrCall::Indexing, indices)) => Expression::Access {
                        object: Box::new(subexpr),
                        indices,
                    },

                    // Subexpression was called
                    Some((IndexingOrCall::Call, parameters)) => Expression::Call {
                        object: Box::new(subexpr),
                        parameters,
                    },

                    // Subexpression last as-is
                    None => subexpr,
                };

                // Append subexpression to result
                builder.push_expression(expr);
                expected_element = ExpectedElement::Operator;
            }

            // Operator
            Token::Symbol(sym) => if let Some(mut op) = Operator::from_symbol(*sym) {
                if expected_element != ExpectedElement::Operator {
                    if let Some(new_op) = op.into_unary() {
                        op = new_op;
                    } else {
                        break 'expr_loop;
                    }
                }
                
                builder
                    .push_operator(op)
                    .expect("Error during AST operator push");
                tl = &tl[1..];
                expected_element = ExpectedElement::Value;
            } else {
                // Unexpected no-op symbol in expression, it'll be handled later...
                break 'expr_loop;
            }
        }
    } // 'expr_loop

    let expression_ast = builder.build().expect("Error finalizing expression AST");

    Ok((tl, expression_ast))
}
