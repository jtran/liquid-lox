use ast::*;
use token::*;
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

    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut statements = Vec::new();
        while ! self.is_at_end() {
            statements.push(self.statement());
        }
        self.consume(TokenType::Eof, "Expected end of file");

        statements
    }

    pub fn parse_expression(&mut self) -> Expr {
        let expr = self.expression();
        self.consume(TokenType::Eof, "Expected end of file");

        expr
    }

    fn statement(&mut self) -> Stmt {
        match self.matches(&vec![TokenType::Print]) {
            None => self.expression_statement(),
            Some(_) => self.finish_print_statement(),
        }
    }

    fn finish_print_statement(&mut self) -> Stmt {
        // The Print token has already been consumed.
        let expr = self.expression();
        self.consume(TokenType::Semicolon, "Expected semicolon after print value");

        Stmt::Print(expr)
    }

    fn expression_statement(&mut self) -> Stmt {
        let expr = self.expression();
        self.consume(TokenType::Semicolon, "Expected semicolon after expression");

        Stmt::Expression(expr)
    }

    fn expression(&mut self) -> Expr {
        self.equality()
    }

    fn equality(&mut self) -> Expr {
        let mut expr = self.comparison();

        loop {
            match self.matches(&vec![TokenType::BangEqual, TokenType::EqualEqual]) {
                None => break,
                Some((operator, loc)) => {
                    let right = self.comparison();
                    let bin_op = match operator {
                        TokenType::BangEqual => BinaryOperator::NotEqual,
                        TokenType::EqualEqual => BinaryOperator::Equal,
                        _ => unreachable!(),
                    };
                    expr = Expr::Binary(Box::new(expr), bin_op, Box::new(right), loc);
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
                Some((operator, loc)) => {
                    let right = self.addition();
                    let bin_op = match operator {
                        TokenType::Less => BinaryOperator::Less,
                        TokenType::LessEqual => BinaryOperator::LessEqual,
                        TokenType::Greater => BinaryOperator::Greater,
                        TokenType::GreaterEqual => BinaryOperator::GreaterEqual,
                        _ => unreachable!(),
                    };
                    expr = Expr::Binary(Box::new(expr), bin_op, Box::new(right), loc);
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
                Some((operator, loc)) => {
                    let right = self.multiplication();
                    let bin_op = match operator {
                        TokenType::Minus => BinaryOperator::Minus,
                        TokenType::Plus => BinaryOperator::Plus,
                        _ => unreachable!(),
                    };
                    expr = Expr::Binary(Box::new(expr), bin_op, Box::new(right), loc);
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
                Some((operator, loc)) => {
                    let right = self.unary();
                    let bin_op = match operator {
                        TokenType::Slash => BinaryOperator::Divide,
                        TokenType::Star => BinaryOperator::Multiply,
                        _ => unreachable!(),
                    };
                    expr = Expr::Binary(Box::new(expr), bin_op, Box::new(right), loc);
                }
            }
        }

        expr
    }

    fn unary(&mut self) -> Expr {
        match self.matches(&vec![TokenType::Bang, TokenType::Minus]) {
            None => self.primary(),
            Some((operator, loc)) => {
                let right = self.unary();

                let unary_op = match operator {
                    TokenType::Bang => UnaryOperator::Not,
                    TokenType::Minus => UnaryOperator::Minus,
                    _ => unreachable!(),
                };
                Expr::Unary(unary_op, Box::new(right), loc)
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
                self.consume(TokenType::RightParen, "Missing close parenthesis");

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

    fn matches(&mut self, token_types: &[TokenType]) -> Option<(TokenType, SourceLoc)> {
        let token_type: Option<_> = match self.peek() {
            None => None,
            Some(token) => {
                if token_types.contains(&token.token_type) {
                    let loc = SourceLoc::new(token.line);

                    Some((token.token_type, loc))
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

    fn previous(&self) -> Option<&Token<'a>> {
        self.tokens.get(self.current - 1)
    }

    fn peek_line(&self) -> usize {
        match self.peek() {
            Some(token) => token.line,
            None => {
                // We're at the end of file, so use the line of the last token.
                match self.previous() {
                    Some(token) => token.line,
                    // If there was no last token, this is an edge case where
                    // there were no tokens.
                    None => 1,
                }
            }
        }
    }

    fn is_at_end(&self) -> bool {
        match self.peek() {
            None => true,
            Some(token) => token.token_type == TokenType::Eof,
        }
    }

    fn consume(&mut self, token_type: TokenType, error_message: &str) {
        match self.matches(&vec![token_type]) {
            Some(_) => (), // Expected.
            None => {
                util::error(self.peek_line(), error_message);
                ()
            }
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::Expr::*;
    use scanner::*;
    use util::parse;
    use util::parse_expression;

    #[test]
    fn test_parse_literal() {
        assert_eq!(parse_expression("42"), LiteralNumber(42.0));
        assert_eq!(parse_expression("\"hello\""), LiteralString("hello".to_string()));
        assert_eq!(parse_expression("true"), LiteralBool(true));
        assert_eq!(parse_expression("false"), LiteralBool(false));
        assert_eq!(parse_expression("nil"), LiteralNil);
    }

    #[test]
    fn test_parse_binary_op() {
        assert_eq!(parse_expression("40 + 2"), Binary(Box::new(LiteralNumber(40.0)),
                                                      BinaryOperator::Plus,
                                                      Box::new(LiteralNumber(2.0)),
                                                      SourceLoc::new(1)));
    }

    #[test]
    fn test_parse_unary_op() {
        assert_eq!(parse_expression("-42"), Unary(UnaryOperator::Minus,
                                                  Box::new(LiteralNumber(42.0)),
                                                  SourceLoc::new(1)));
        assert_eq!(parse_expression("!true"), Unary(UnaryOperator::Not,
                                                    Box::new(LiteralBool(true)),
                                                    SourceLoc::new(1)));
    }

    #[test]
    fn test_parse_grouping() {
        assert_eq!(parse_expression("(40)"), Grouping(Box::new(LiteralNumber(40.0))));
    }

    #[test]
    fn test_parse_comparison() {
        let mut scanner = Scanner::new("42 == 40 + 2");
        let tokens = scanner.scan_tokens();
        let mut parser = Parser::new(tokens);
        let ast = parser.expression();
        assert_eq!(ast, Binary(Box::new(LiteralNumber(42.0)),
                               BinaryOperator::Equal,
                               Box::new(Binary(Box::new(LiteralNumber(40.0)),
                                               BinaryOperator::Plus,
                                               Box::new(LiteralNumber(2.0)),
                                               SourceLoc::new(1))),
                               SourceLoc::new(1)));
    }

    #[test]
    fn test_parse_statements() {
        assert_eq!(parse("print \"one\";"), vec![Stmt::Print(LiteralString("one".into()))]);
    }
}
