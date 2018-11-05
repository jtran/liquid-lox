use ast::*;
use scanner::Scanner;
use source_loc::*;
use token::*;

pub fn parse(source: &str) -> Result<Vec<Stmt>, ParseError> {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens();
    let mut parser = Parser::new(tokens);

    parser.parse()
}

pub fn parse_repl_line(source: &str) -> Result<Vec<Stmt>, ParseError> {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens();
    let mut parser = Parser::with_trailing_expression(tokens);

    parser.parse()
}

#[allow(dead_code)]
pub fn parse_expression(code: &str) -> Result<Expr, ParseError> {
    let mut scanner = Scanner::new(code);
    let tokens = scanner.scan_tokens();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse_expression();

    ast
}

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    current: usize,
    allow_trailing_expression: bool,
}

impl <'a> Parser<'a> {
    pub fn new(tokens: Vec<Token<'a>>) -> Parser<'a> {
        Parser {
            tokens,
            current: 0,
            allow_trailing_expression: false,
        }
    }

    // Construct a parser that allows a trailing expression before the end of
    // the token stream.  This is designed for the REPL.
    pub fn with_trailing_expression(tokens: Vec<Token<'a>>) -> Parser<'a> {
        Parser {
            tokens,
            current: 0,
            allow_trailing_expression: true,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = Vec::new();
        let mut errors = Vec::new();
        while ! self.is_at_end() {
            match self.declaration() {
                Ok(statement) => {
                    statements.push(statement);
                }
                Err(error) => {
                    errors.push(error);
                    // TODO: synchronize here.
                }
            }
        }
        let result = self.consume(TokenType::Eof, "Expected end of file");
        if let Err(error) = result {
            errors.push(error);
        }

        if errors.is_empty() {
            Ok(statements)
        }
        else {
            Err(ParseError::new(errors))
        }
    }

    pub fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        let expr = self.expression()?;
        self.consume(TokenType::Eof, "Expected end of file")?;

        Ok(expr)
    }

    fn declaration(&mut self) -> Result<Stmt, ParseErrorCause> {
        match self.matches(&vec![TokenType::Var]) {
            None => self.statement(),
            Some(_) => self.finish_var_declaration(),
        }
    }

    fn finish_var_declaration(&mut self) -> Result<Stmt, ParseErrorCause> {
        // Consume the identifier.
        let id = match self.peek() {
            Some(token) => {
                if token.token_type == TokenType::Identifier {
                    token.lexeme
                }
                else {
                    return Err(self.new_error("Expected identifier after \"var\""));
                }
            }
            None => {
                return Err(self.new_error("Expected identifier after \"var\""));
            }
        };
        // Consume the identifier.
        self.advance();

        let expr = match self.matches(&vec![TokenType::Equal]) {
            None => Expr::LiteralNil,
            Some(_) => self.expression()?,
        };

        self.consume(TokenType::Semicolon, "Expected semicolon after var declaration")?;

        Ok(Stmt::Var(id.to_string(), expr))
    }

    fn statement(&mut self) -> Result<Stmt, ParseErrorCause> {
        match self.matches(&vec![TokenType::LeftBrace, TokenType::Print]) {
            None => self.expression_statement(),
            Some((TokenType::LeftBrace, _)) => {
                self.finish_block().map(|statements| Stmt::Block(statements))
            }
            Some((TokenType::Print, _)) => self.finish_print_statement(),
            Some((token_type, loc)) => panic!("statement: unexpected token type: {:?} loc={:?}", token_type, loc),
        }
    }

    fn finish_block(&mut self) -> Result<Vec<Stmt>, ParseErrorCause> {
        let mut statements = Vec::new();

        while ! self.check(TokenType::RightBrace) && ! self.is_at_end() {
            statements.push(self.declaration()?);
        }

        self.consume(TokenType::RightBrace, "Expected right brace after block")?;

        Ok(statements)
    }

    fn finish_print_statement(&mut self) -> Result<Stmt, ParseErrorCause> {
        // The Print token has already been consumed.
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expected semicolon after print value")?;

        Ok(Stmt::Print(expr))
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParseErrorCause> {
        let expr = self.expression()?;

        // If we allow a trailing expression and it's the end of the file, we
        // don't need to consume.  But if there is a semicolon, always consume
        // it.
        if ! (self.allow_trailing_expression && self.is_at_end())
            || self.check(TokenType::Semicolon)
        {
            self.consume(TokenType::Semicolon, "Expected semicolon after expression")?;
        }

        Ok(Stmt::Expression(expr))
    }

    fn expression(&mut self) -> Result<Expr, ParseErrorCause> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParseErrorCause> {
        let expr = self.equality()?;

        match self.matches(&vec![TokenType::Equal]) {
            None => Ok(expr), // Not actually an assignment at all.
            Some((_, loc)) => {
                // Recurse since this is right associative.
                let right_expr = self.assignment()?;

                let id = match &expr {
                    Expr::Variable(id, _) => id.clone(),
                    _ => return Err(self.new_error(&format!("Invalid assignment target; expected identifier, found: {:?}", &expr))),
                };

                Ok(Expr::Assign(id, Box::new(right_expr), loc))
            }
        }
    }

    fn equality(&mut self) -> Result<Expr, ParseErrorCause> {
        let mut expr = self.comparison()?;

        loop {
            match self.matches(&vec![TokenType::BangEqual, TokenType::EqualEqual]) {
                None => break,
                Some((operator, loc)) => {
                    let right = self.comparison()?;
                    let bin_op = match operator {
                        TokenType::BangEqual => BinaryOperator::NotEqual,
                        TokenType::EqualEqual => BinaryOperator::Equal,
                        _ => unreachable!(),
                    };
                    expr = Expr::Binary(Box::new(expr), bin_op, Box::new(right), loc);
                }
            }
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParseErrorCause> {
        let mut expr = self.addition()?;

        loop {
            match self.matches(&vec![TokenType::Less,
                                     TokenType::Greater,
                                     TokenType::LessEqual,
                                     TokenType::GreaterEqual]) {
                None => break,
                Some((operator, loc)) => {
                    let right = self.addition()?;
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

        Ok(expr)
    }

    fn addition(&mut self) -> Result<Expr, ParseErrorCause> {
        let mut expr = self.multiplication()?;

        loop {
            match self.matches(&vec![TokenType::Minus, TokenType::Plus]) {
                None => break,
                Some((operator, loc)) => {
                    let right = self.multiplication()?;
                    let bin_op = match operator {
                        TokenType::Minus => BinaryOperator::Minus,
                        TokenType::Plus => BinaryOperator::Plus,
                        _ => unreachable!(),
                    };
                    expr = Expr::Binary(Box::new(expr), bin_op, Box::new(right), loc);
                }
            }
        }

        Ok(expr)
    }

    fn multiplication(&mut self) -> Result<Expr, ParseErrorCause> {
        let mut expr = self.unary()?;

        loop {
            match self.matches(&vec![TokenType::Slash, TokenType::Star]) {
                None => break,
                Some((operator, loc)) => {
                    let right = self.unary()?;
                    let bin_op = match operator {
                        TokenType::Slash => BinaryOperator::Divide,
                        TokenType::Star => BinaryOperator::Multiply,
                        _ => unreachable!(),
                    };
                    expr = Expr::Binary(Box::new(expr), bin_op, Box::new(right), loc);
                }
            }
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseErrorCause> {
        match self.matches(&vec![TokenType::Bang, TokenType::Minus]) {
            None => self.primary(),
            Some((operator, loc)) => {
                let right = self.unary()?;

                let unary_op = match operator {
                    TokenType::Bang => UnaryOperator::Not,
                    TokenType::Minus => UnaryOperator::Minus,
                    _ => unreachable!(),
                };
                Ok(Expr::Unary(unary_op, Box::new(right), loc))
            }
        }
    }

    fn primary(&mut self) -> Result<Expr, ParseErrorCause> {
        let token_type = match self.peek() {
            None => panic!("primary: reached the end of the token stream unexpectedly"),
            Some(token) => token.token_type,
        };
        let mut already_advanced = false;

        let expr = match token_type {
            TokenType::False => Expr::LiteralBool(false),
            TokenType::True => Expr::LiteralBool(true),
            TokenType::Nil => Expr::LiteralNil,
            TokenType::Number => {
                match self.peek() {
                    None => panic!("primary: Number case: this shouldn't happen"),
                    Some(token) => {
                        let x = token.float_literal.expect("primary: expected float literal to include parsed float");

                        Expr::LiteralNumber(x)
                    }
                }
            }
            TokenType::String => {
                match self.peek() {
                    None => panic!("primary: String case: this shouldn't happen"),
                    Some(token) => {
                        let s = token.string_literal.expect("primary: expected float literal to include parsed string");

                        Expr::LiteralString(String::from(s))
                    }
                }
            }
            TokenType::Identifier => {
                match self.peek() {
                    None => panic!("primary: identifier case: this shouldn't happen"),
                    Some(token) => {
                        let loc = SourceLoc::new(token.line);

                        Expr::Variable(token.lexeme.to_string(), loc)
                    }
                }
            }
            TokenType::LeftParen => {
                self.advance();
                already_advanced = true;

                let expr = self.expression()?;
                self.consume(TokenType::RightParen, "Missing close parenthesis")?;

                Expr::Grouping(Box::new(expr))
            }
            _ => {
                self.advance();
                return Err(self.new_error(&format!("Unexpected token: {:?}", token_type)));
            }
        };

        if ! already_advanced {
            self.advance();
        }

        Ok(expr)
    }

    fn check(&self, token_type: TokenType) -> bool {
        ! self.is_at_end() && match self.peek() {
            None => false,
            Some(token) => token.token_type == token_type,
        }
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

    fn peek_source_loc(&self) -> SourceLoc {
        SourceLoc::new(self.peek_line())
    }

    fn new_error(&self, message: &str) -> ParseErrorCause {
        ParseErrorCause::new(self.peek_source_loc(), message)
    }

    fn is_at_end(&self) -> bool {
        match self.peek() {
            None => true,
            Some(token) => token.token_type == TokenType::Eof,
        }
    }

    #[must_use]
    fn consume(&mut self, token_type: TokenType, error_message: &str)
        -> Result<(), ParseErrorCause>
    {
        match self.matches(&vec![token_type]) {
            Some(_) => Ok(()), // Expected.
            None => Err(self.new_error(error_message)),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParseError {
    pub causes: Vec<ParseErrorCause>,
}

impl ParseError {
    pub fn new(causes: Vec<ParseErrorCause>) -> ParseError {
        ParseError {
            causes,
        }
    }

    pub fn message(&self) -> String {
        match self.causes.first() {
            None => "unknown cause".to_string(),
            Some(err) => err.message.to_string(),
        }
    }

    pub fn source_loc(&self) -> SourceLoc {
        match self.causes.first() {
            None => SourceLoc::default(),
            Some(err) => err.source_loc,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParseErrorCause {
    pub source_loc: SourceLoc,
    pub message: String,
}

impl ParseErrorCause {
    pub fn new(source_loc: SourceLoc, message: &str) -> ParseErrorCause {
        ParseErrorCause {
            source_loc,
            message: message.to_string(),
        }
    }
}

impl From<ParseErrorCause> for ParseError {
    fn from(error: ParseErrorCause) -> ParseError {
        ParseError { causes: vec![error] }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::Expr::*;
    use scanner::*;

    #[test]
    fn test_parse_literal() {
        assert_eq!(parse_expression("42"), Ok(LiteralNumber(42.0)));
        assert_eq!(parse_expression("\"hello\""), Ok(LiteralString("hello".to_string())));
        assert_eq!(parse_expression("true"), Ok(LiteralBool(true)));
        assert_eq!(parse_expression("false"), Ok(LiteralBool(false)));
        assert_eq!(parse_expression("nil"), Ok(LiteralNil));
    }

    #[test]
    fn test_parse_binary_op() {
        assert_eq!(parse_expression("40 + 2"), Ok(Binary(Box::new(LiteralNumber(40.0)),
                                                         BinaryOperator::Plus,
                                                         Box::new(LiteralNumber(2.0)),
                                                         SourceLoc::new(1))));
    }

    #[test]
    fn test_parse_unary_op() {
        assert_eq!(parse_expression("-42"), Ok(Unary(UnaryOperator::Minus,
                                                     Box::new(LiteralNumber(42.0)),
                                                     SourceLoc::new(1))));
        assert_eq!(parse_expression("!true"), Ok(Unary(UnaryOperator::Not,
                                                       Box::new(LiteralBool(true)),
                                                       SourceLoc::new(1))));
    }

    #[test]
    fn test_parse_grouping() {
        assert_eq!(parse_expression("(40)"), Ok(Grouping(Box::new(LiteralNumber(40.0)))));
    }

    #[test]
    fn test_parse_assign() {
        assert_eq!(parse_expression("x = 1"), Ok(Assign("x".to_string(),
                                                        Box::new(LiteralNumber(1.0)),
                                                        SourceLoc::new(1))));
    }

    #[test]
    fn test_parse_comparison() {
        let mut scanner = Scanner::new("42 == 40 + 2");
        let tokens = scanner.scan_tokens();
        let mut parser = Parser::new(tokens);
        let ast = parser.expression();
        assert_eq!(ast, Ok(Binary(Box::new(LiteralNumber(42.0)),
                                  BinaryOperator::Equal,
                                  Box::new(Binary(Box::new(LiteralNumber(40.0)),
                                                  BinaryOperator::Plus,
                                                  Box::new(LiteralNumber(2.0)),
                                                  SourceLoc::new(1))),
                                  SourceLoc::new(1))));
    }

    #[test]
    fn test_parse_invalid() {
        let causes = vec![
            ParseErrorCause::new(SourceLoc::new(1), "Unexpected token: And"),
            ParseErrorCause::new(SourceLoc::new(1), "Unexpected token: Semicolon"),
        ];
        assert_eq!(parse("and;"), Err(ParseError::new(causes)));
    }

    #[test]
    fn test_parse_statements() {
        assert_eq!(parse("print \"one\";"), Ok(vec![Stmt::Print(LiteralString("one".into()))]));
    }
}
