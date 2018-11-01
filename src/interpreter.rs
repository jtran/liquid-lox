use expr::*;
use value::*;

pub struct Interpreter {
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {}
    }

    pub fn evaluate(&mut self, expr: &Expr) -> Value {
        use value::Value::*;
        match expr {
            Expr::Binary(left, op, right) => {
                let left_val = self.evaluate(left);
                let right_val = self.evaluate(right);

                match op {
                    // Math operators.
                    BinaryOperator::Plus => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => NumberVal(x1 + x2),
                            (StringVal(s1), StringVal(s2)) => {
                                StringVal(format!("{}{}", s1, s2))
                            }
                            _ => panic!("type mismatch evaluating plus in expression: {:?}", expr),
                        }
                    },
                    BinaryOperator::Minus => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => NumberVal(x1 - x2),
                            _ => panic!("type mismatch evaluating minus in expression: {:?}", expr),
                        }
                    },
                    BinaryOperator::Multiply => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => NumberVal(x1 * x2),
                            _ => panic!("type mismatch evaluating multiply in expression: {:?}", expr),
                        }
                    },
                    BinaryOperator::Divide => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => NumberVal(x1 / x2),
                            _ => panic!("type mismatch evaluating divide in expression: {:?}", expr),
                        }
                    },
                    // Comparison operators.
                    BinaryOperator::Equal => BoolVal(left_val.is_equal(&right_val)),
                    BinaryOperator::NotEqual => BoolVal(!left_val.is_equal(&right_val)),
                    BinaryOperator::Less => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => BoolVal(x1 < x2),
                            _ => panic!("type mismatch evaluating less-than in expression: {:?}", expr),
                        }
                    },
                    BinaryOperator::LessEqual => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => BoolVal(x1 <= x2),
                            _ => panic!("type mismatch evaluating less-than-or-equal-to in expression: {:?}", expr),
                        }
                    },
                    BinaryOperator::Greater => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => BoolVal(x1 > x2),
                            _ => panic!("type mismatch evaluating greater-than in expression: {:?}", expr),
                        }
                    },
                    BinaryOperator::GreaterEqual => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => BoolVal(x1 >= x2),
                            _ => panic!("type mismatch evaluating greater-than-or-equal-to in expression: {:?}", expr),
                        }
                    },
                }
            }
            Expr::Grouping(e) => self.evaluate(e),
            Expr::LiteralBool(b) => BoolVal(*b),
            Expr::LiteralNil => NilVal,
            Expr::LiteralNumber(x) => NumberVal(*x),
            Expr::LiteralString(s) => StringVal(s.clone()),
            Expr::Unary(op, e) => {
                let v = self.evaluate(e);

                match op {
                    UnaryOperator::Minus => {
                        match v {
                            NumberVal(x) => NumberVal(-x),
                            _ => panic!("type mismatch evaluating unary operator in expression: {:?}", expr),
                        }
                    },
                    UnaryOperator::Not => BoolVal(!v.is_truthy()),
                }
            }
        }
    }
}
