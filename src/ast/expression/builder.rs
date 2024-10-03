use crate::ast::{BinaryOperator, Expression};

/// AST building helper
pub struct ExpressionAstBuilder {
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

    /// Expression behaves as solid value pushing function
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

// file builder.rs
