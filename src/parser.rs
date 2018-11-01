use token::*;
use expr::*;
use util;

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    current: usize,
}

impl <'a> Parser<'a> {
    pub fn new(tokens: Vec<Token<'a>>) -> Parser<'a> {
        Parser {
            tokens,
            current: 0,
        }
    }

    pub fn parse(&mut self) -> Expr {
        self.expression()
    }

    fn expression(&mut self) -> Expr {
        self.equality()
    }

    fn equality(&mut self) -> Expr {
        let mut expr = self.comparison();

        loop {
            match self.matches(&vec![TokenType::BangEqual, TokenType::EqualEqual]) {
                None => break,
                Some(operator) => {
                    let right = self.comparison();
                    let bin_op = match operator {
                        TokenType::BangEqual => BinaryOperator::NotEqual,
                        TokenType::EqualEqual => BinaryOperator::Equal,
                        _ => unreachable!(),
                    };
                    expr = Expr::Binary(Box::new(expr), bin_op, Box::new(right));
                }
            }
        }

        expr
    }

    fn comparison(&mut self) -> Expr {
        let mut expr = self.addition();

        loop {
            match self.matches(&vec![TokenType::Less,
                                     TokenType::Greater,
                                     TokenType::LessEqual,
                                     TokenType::GreaterEqual]) {
                None => break,
                Some(operator) => {
                    let right = self.addition();
                    let bin_op = match operator {
                        TokenType::Less => BinaryOperator::Less,
                        TokenType::LessEqual => BinaryOperator::LessEqual,
                        TokenType::Greater => BinaryOperator::Greater,
                        TokenType::GreaterEqual => BinaryOperator::GreaterEqual,
                        _ => unreachable!(),
                    };
                    expr = Expr::Binary(Box::new(expr), bin_op, Box::new(right));
                }
            }
        }

        expr
    }

    fn addition(&mut self) -> Expr {
        let mut expr = self.multiplication();

        loop {
            match self.matches(&vec![TokenType::Minus, TokenType::Plus]) {
                None => break,
                Some(operator) => {
                    let right = self.multiplication();
                    let bin_op = match operator {
                        TokenType::Minus => BinaryOperator::Minus,
                        TokenType::Plus => BinaryOperator::Plus,
                        _ => unreachable!(),
                    };
                    expr = Expr::Binary(Box::new(expr), bin_op, Box::new(right));
                }
            }
        }

        expr
    }

    fn multiplication(&mut self) -> Expr {
        let mut expr = self.unary();

        loop {
            match self.matches(&vec![TokenType::Slash, TokenType::Star]) {
                None => break,
                Some(operator) => {
                    let right = self.unary();
                    let bin_op = match operator {
                        TokenType::Slash => BinaryOperator::Divide,
                        TokenType::Star => BinaryOperator::Multiply,
                        _ => unreachable!(),
                    };
                    expr = Expr::Binary(Box::new(expr), bin_op, Box::new(right));
                }
            }
        }

        expr
    }

    fn unary(&mut self) -> Expr {
        match self.matches(&vec![TokenType::Bang, TokenType::Minus]) {
            None => self.primary(),
            Some(operator) => {
                let right = self.unary();

                let unary_op = match operator {
                    TokenType::Bang => UnaryOperator::Not,
                    TokenType::Minus => UnaryOperator::Minus,
                    _ => unreachable!(),
                };
                Expr::Unary(unary_op, Box::new(right))
            }
        }
    }

    fn primary(&mut self) -> Expr {
        let (line, token_type) = match self.peek() {
            None => panic!("primary: reached the end of the token stream unexpectedly"),
            Some(token) => (token.line, token.token_type),
        };
        let mut already_advanced = false;

        let expr = match token_type {
            TokenType::False => Expr::LiteralBool(false),
            TokenType::True => Expr::LiteralBool(true),
            TokenType::Nil => Expr::LiteralNil,
            TokenType::Number => {
                match self.peek() {
                    None => panic!("primary: Number case: this shouldn't happen"),
                    Some(token) => Expr::LiteralNumber(token.float_literal.expect("primary: expected float literal to include parsed float")),
                }
            }
            TokenType::String => {
                match self.peek() {
                    None => panic!("primary: String case: this shouldn't happen"),
                    Some(token) => Expr::LiteralString(String::from(token.string_literal.expect("primary: expected float literal to include parsed string"))),
                }
            }
            TokenType::LeftParen => {
                self.advance();
                already_advanced = true;

                let expr = self.expression();
                match self.matches(&vec![TokenType::RightParen]) {
                    Some(_) => (), // Expected.
                    None => {
                        util::error(line, "Missing close parenthesis");
                        ()
                    }
                };

                Expr::Grouping(Box::new(expr))
            }
            _ => {
                util::error(line, &format!("Unexpected token: {:?}", token_type));

                // TODO: What should we return here?
                Expr::LiteralNil
            }
        };

        if ! already_advanced {
            self.advance();
        }

        expr
    }

    fn matches(&mut self, token_types: &[TokenType]) -> Option<TokenType> {
        let token_type: Option<_> = match self.peek() {
            None => None,
            Some(token) => {
                if token_types.contains(&token.token_type) {
                    Some(token.token_type)
                }
                else {
                    None
                }
            }
        };

        if token_type.is_some() {
            self.advance();
        }

        token_type
    }

    fn advance(&mut self) {
        if self.is_at_end() {
            return;
        }
        self.current += 1;
    }

    fn peek(&self) -> Option<&Token<'a>> {
        self.tokens.get(self.current)
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expr::Expr::*;
    use scanner::*;

    fn parse_string(s: &str) -> Expr {
        let mut scanner = Scanner::new(s);
        let tokens = scanner.scan_tokens();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        ast
    }

    #[test]
    fn test_parse_literal() {
        assert_eq!(parse_string("42"), LiteralNumber(42.0));
        assert_eq!(parse_string("\"hello\""), LiteralString("hello".to_string()));
        assert_eq!(parse_string("true"), LiteralBool(true));
        assert_eq!(parse_string("false"), LiteralBool(false));
        assert_eq!(parse_string("nil"), LiteralNil);
    }

    #[test]
    fn test_parse_binary_op() {
        assert_eq!(parse_string("40 + 2"), Binary(Box::new(LiteralNumber(40.0)),
                                                  BinaryOperator::Plus,
                                                  Box::new(LiteralNumber(2.0))));
    }

    #[test]
    fn test_parse_unary_op() {
        assert_eq!(parse_string("-42"), Unary(UnaryOperator::Minus, Box::new(LiteralNumber(42.0))));
        assert_eq!(parse_string("!true"), Unary(UnaryOperator::Not, Box::new(LiteralBool(true))));
    }

    #[test]
    fn test_parse_grouping() {
        assert_eq!(parse_string("(40)"), Grouping(Box::new(LiteralNumber(40.0))));
    }

    #[test]
    fn test_parse_comparison() {
        let mut scanner = Scanner::new("42 == 40 + 2");
        let tokens = scanner.scan_tokens();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();
        assert_eq!(ast, Binary(Box::new(LiteralNumber(42.0)),
                               BinaryOperator::Equal,
                               Box::new(Binary(Box::new(LiteralNumber(40.0)),
                                               BinaryOperator::Plus,
                                               Box::new(LiteralNumber(2.0))))));
    }
}
