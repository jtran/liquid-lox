use std::cell::Cell;

use crate::ast::*;
use crate::error::*;
use crate::environment::*;
use crate::scanner::Scanner;
use crate::source_loc::*;
use crate::token::*;

pub fn parse(source: &str) -> Result<Vec<Stmt>, ParseError> {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens()?;
    let mut parser = Parser::new(tokens);

    parser.parse()
}

pub fn parse_repl_line(source: &str) -> Result<Vec<Stmt>, ParseError> {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens()?;
    let mut parser = Parser::with_trailing_expression(tokens);

    parser.parse()
}

#[allow(dead_code)]
pub fn parse_expression(code: &str) -> Result<Expr, ParseError> {
    let mut scanner = Scanner::new(code);
    let tokens = scanner.scan_tokens()?;
    let mut parser = Parser::new(tokens);

    parser.parse_expression()
}

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    current: usize,
    in_loops: usize,
    allow_trailing_expression: bool,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token<'a>>) -> Parser<'a> {
        Parser {
            tokens,
            current: 0,
            in_loops: 0,
            allow_trailing_expression: false,
        }
    }

    // Construct a parser that allows a trailing expression before the end of
    // the token stream.  This is designed for the REPL.
    pub fn with_trailing_expression(tokens: Vec<Token<'a>>) -> Parser<'a> {
        Parser {
            tokens,
            current: 0,
            in_loops: 0,
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
                    self.synchronize();
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
        if self.match_token(TokenType::Class) {
            self.finish_class_declaration()
        } else if self.check(TokenType::Fun) && self.check_next(TokenType::Identifier) {
            self.consume(TokenType::Fun, "Consuming Fun token that we just checked; this error should never happen")?;
            self.finish_fun_declaration()
        } else if self.match_token(TokenType::Var) {
            self.finish_var_declaration()
        } else {
            self.statement()
        }
    }

    fn finish_class_declaration(&mut self) -> Result<Stmt, ParseErrorCause> {
        // The Class token has already been consumed.
        let (id, loc) = self.consume_identifier("Expect identifier after 'class'.")?;

        // Optional superclass.
        let superclass = if self.match_token(TokenType::Less) {
            let (super_id, super_loc) = self.consume_identifier("Expect superclass name.")?;
            let super_expr = Expr::Variable(super_id, Cell::new(VarLoc::placeholder()), super_loc);

            Some(Box::new(super_expr))
        } else {
            None
        };

        self.consume(TokenType::LeftBrace, "Expect '{' after class name.")?;
        let mut methods = Vec::new();
        while ! self.check(TokenType::RightBrace) && ! self.is_at_end() {
            let is_class_method = self.match_token(TokenType::Class);

            match self.finish_fun_declaration()? {
                Stmt::Fun(mut fun_decl) => {
                    fun_decl.fun_def.fun_type = if is_class_method {
                        FunctionType::ClassMethod
                    } else if fun_decl.name == "init" {
                        FunctionType::Initializer
                    } else {
                        FunctionType::Method
                    };
                    methods.push(fun_decl);
                }
                _ => {
                    return Err(self.error_from_last("Expect method definition in class."));
                }
            }
        }
        self.consume(TokenType::RightBrace, "Expect '}' after class method body.")?;

        let class_def = ClassDefinition {
            name: id,
            superclass,
            methods,
            source_loc: loc,
        };

        Ok(Stmt::Class(class_def))
    }

    fn finish_fun_declaration(&mut self) -> Result<Stmt, ParseErrorCause> {
        // The Fun token has already been consumed.
        let (id, loc) = self.consume_identifier("Expect identifier after 'fun'.")?;

        let fun_def = self.function_definition(&loc)?;
        let named_fun_def = NamedFunctionDefinition::new(id, fun_def);

        Ok(Stmt::Fun(named_fun_def))
    }

    fn function_definition(&mut self, loc: &SourceLoc) -> Result<FunctionDefinition, ParseErrorCause> {
        // Start with the parameter list.
        self.consume(TokenType::LeftParen, "Expect '(' after function name.")?;
        let mut parameters = Vec::new();
        if ! self.check(TokenType::RightParen) {
            loop {
                let (param_name, loc) = self.consume_identifier("Expect identifier in function parameters.")?;

                if parameters.len() >= 255 {
                    return Err(ParseErrorCause::new_with_location(loc, &param_name, "Cannot have more than 255 parameters."));
                }

                parameters.push(Parameter::new(param_name, loc));

                if ! self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after parameters.")?;

        self.consume(TokenType::LeftBrace, "Expect '{' before function body.")?;
        let body = self.finish_block()?;

        let fun_def = FunctionDefinition::new(parameters,
                                              body,
                                              FunctionType::PlainFunction,
                                              *loc);

        Ok(fun_def)
    }

    fn finish_var_declaration(&mut self) -> Result<Stmt, ParseErrorCause> {
        // Consume the identifier.
        let (id, loc) = self.consume_identifier("Expect variable name.")?;

        let expr = match self.matches(&[TokenType::Equal]) {
            None => Expr::LiteralNil,
            Some(_) => self.expression()?,
        };

        self.consume(TokenType::Semicolon, "Expect ';' after var declaration.")?;

        Ok(Stmt::Var(id, Cell::new(SlotIndex::placeholder()), expr, loc))
    }

    fn statement(&mut self) -> Result<Stmt, ParseErrorCause> {
        match self.matches(&[TokenType::Break,
                             TokenType::Continue,
                             TokenType::For,
                             TokenType::If,
                             TokenType::LeftBrace,
                             TokenType::Print,
                             TokenType::Return,
                             TokenType::While]) {
            None => self.expression_statement(),
            Some((TokenType::Break, loc)) => self.finish_break_statement(loc),
            Some((TokenType::Continue, loc)) => self.finish_continue_statement(loc),
            Some((TokenType::For, _)) => self.finish_for_statement(),
            Some((TokenType::If, _)) => self.finish_if_statement(),
            Some((TokenType::LeftBrace, _)) => {
                self.finish_block().map(Stmt::Block)
            }
            Some((TokenType::Print, _)) => self.finish_print_statement(),
            Some((TokenType::Return, loc)) => self.finish_return_statement(loc),
            Some((TokenType::While, _)) => self.finish_while_statement(),
            Some((token_type, loc)) => panic!("statement: unexpected token type: {:?} loc={:?}", token_type, loc),
        }
    }

    fn finish_break_statement(&mut self, loc: SourceLoc) -> Result<Stmt, ParseErrorCause> {
        // The Break token has already been consumed.
        if self.in_loops == 0 {
            return Err(self.error_from_last("Found break statement outside of loop body"));
        }
        self.consume(TokenType::Semicolon, "Expect ';' after break.")?;

        Ok(Stmt::Break(loc))
    }

    fn finish_continue_statement(&mut self, loc: SourceLoc) -> Result<Stmt, ParseErrorCause> {
        // The Continue token has already been consumed.
        if self.in_loops == 0 {
            return Err(self.error_from_last("Found continue statement outside of loop body"));
        }
        self.consume(TokenType::Semicolon, "Expect ';' after continue.")?;

        Ok(Stmt::Continue(loc))
    }

    fn finish_for_statement(&mut self) -> Result<Stmt, ParseErrorCause> {
        // The For token has already been consumed.
        self.consume(TokenType::LeftParen, "Expect '(' after for.")?;
        let initializer = if self.match_token(TokenType::Semicolon) {
            None
        }
        else if self.match_token(TokenType::Var) {
            Some(self.finish_var_declaration()?)
        }
        else {
            Some(self.expression_statement()?)
        };
        let condition = if self.check(TokenType::Semicolon) {
            Expr::LiteralBool(true)
        }
        else {
            self.expression()?
        };
        self.consume(TokenType::Semicolon, "Expect ';' after for-loop condition.")?;
        let increment = if self.check(TokenType::RightParen) {
            None
        }
        else {
            Some(Box::new(self.expression()?))
        };
        self.consume(TokenType::RightParen, "Expect ')' after for-loop increment.")?;
        let loop_body = self.loop_body_statement()?;

        // Convert into while loop.
        let while_loop = match increment {
            None => Stmt::While(condition, Box::new(loop_body)),
            Some(inc_expr) => Stmt::WhileIncrement(condition, Box::new(loop_body), inc_expr),
        };

        match initializer {
            None => Ok(while_loop),
            Some(init_stmt) => {
                Ok(Stmt::Block(vec![init_stmt,
                                    while_loop]))
            }
        }
    }

    fn finish_if_statement(&mut self) -> Result<Stmt, ParseErrorCause> {
        self.consume(TokenType::LeftParen, "Expect '(' after if.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after if condition.")?;

        let then_stmt = self.statement()?;

        let else_stmt = if self.match_token(TokenType::Else) {
            Some(Box::new(self.statement()?))
        }
        else {
            None
        };

        Ok(Stmt::If(condition, Box::new(then_stmt), else_stmt))
    }

    fn finish_block(&mut self) -> Result<Vec<Stmt>, ParseErrorCause> {
        let mut statements = Vec::new();

        while ! self.check(TokenType::RightBrace) && ! self.is_at_end() {
            statements.push(self.declaration()?);
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block.")?;

        Ok(statements)
    }

    fn finish_print_statement(&mut self) -> Result<Stmt, ParseErrorCause> {
        // The Print token has already been consumed.
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after print value.")?;

        Ok(Stmt::Print(expr))
    }

    fn finish_return_statement(&mut self, loc: SourceLoc) -> Result<Stmt, ParseErrorCause> {
        // The Return token has already been consumed.
        let expr = if self.check(TokenType::Semicolon) {
            Expr::LiteralNil
        }
        else {
            self.expression()?
        };
        self.consume(TokenType::Semicolon, "Expect ';' after return expression.")?;

        Ok(Stmt::Return(expr, loc))
    }

    fn finish_while_statement(&mut self) -> Result<Stmt, ParseErrorCause> {
        // The While token has already been consumed.
        self.consume(TokenType::LeftParen, "Expect '(' after while.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after while condition.")?;
        let body = self.loop_body_statement()?;

        Ok(Stmt::While(condition, Box::new(body)))
    }

    // Parses a statement while also tracking that we are inside a loop body.
    fn loop_body_statement(&mut self) -> Result<Stmt, ParseErrorCause> {
        self.in_loops = self.in_loops.checked_add(1).expect("Too many nested loops");
        let body_result = self.statement();
        self.in_loops -= 1;

        body_result
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParseErrorCause> {
        let expr = self.expression()?;

        // If we allow a trailing expression and it's the end of the file, we
        // don't need to consume.  But if there is a semicolon, always consume
        // it.
        if ! (self.allow_trailing_expression && self.is_at_end())
            || self.check(TokenType::Semicolon)
        {
            self.consume(TokenType::Semicolon, "Expect ';' after expression.")?;
        }

        Ok(Stmt::Expression(expr))
    }

    fn expression(&mut self) -> Result<Expr, ParseErrorCause> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParseErrorCause> {
        let expr = self.or()?;

        match self.matches(&[TokenType::Equal]) {
            None => Ok(expr), // Not actually an assignment at all.
            Some((_, loc)) => {
                // Recurse since this is right associative.
                let right_expr = self.assignment()?;

                match expr {
                    Expr::Get(object_expr, property_name, _) =>
                        Ok(Expr::Set(object_expr, property_name, Box::new(right_expr), loc)),
                    Expr::GetIndex(array_expr, index, _) =>
                        Ok(Expr::SetIndex(array_expr, index, Box::new(right_expr), loc)),
                    Expr::Variable(id, _, _) => {
                        if id == "this" {
                            Err(ParseErrorCause::new_with_location(loc, "=", "Invalid assignment target."))
                        } else {
                            Ok(Expr::Assign(id, Cell::new(VarLoc::placeholder()), Box::new(right_expr), loc))
                        }
                    }
                    _ => Err(ParseErrorCause::new_with_location(loc, "=", "Invalid assignment target.")),
                }
            }
        }
    }

    fn or(&mut self) -> Result<Expr, ParseErrorCause> {
        let mut expr = self.and()?;

        while self.match_token(TokenType::Or) {
            let right_expr = self.and()?;
            expr = Expr::Logical(Box::new(expr), LogicalOperator::Or, Box::new(right_expr));
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr, ParseErrorCause> {
        let mut expr = self.equality()?;

        while self.match_token(TokenType::And) {
            let right_expr = self.equality()?;
            expr = Expr::Logical(Box::new(expr), LogicalOperator::And, Box::new(right_expr));
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, ParseErrorCause> {
        let mut expr = self.comparison()?;

        loop {
            match self.matches(&[TokenType::BangEqual, TokenType::EqualEqual]) {
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
            match self.matches(&[TokenType::Less,
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
            match self.matches(&[TokenType::Minus, TokenType::Plus]) {
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
            match self.matches(&[TokenType::Slash, TokenType::Star]) {
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
        match self.matches(&[TokenType::Bang, TokenType::Minus]) {
            None => self.call(),
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

    fn call(&mut self) -> Result<Expr, ParseErrorCause> {
        let mut expr = self.primary()?;

        loop {
            match self.matches(&[TokenType::Dot,
                                 TokenType::LeftParen,
                                 TokenType::LeftBracket]) {
                None => break,
                Some((TokenType::Dot, loc)) => {
                    let (id, _) = self.consume_identifier("Expect property name after '.'.")?;
                    expr = Expr::Get(Box::new(expr), id, loc);
                }
                Some((TokenType::LeftParen, loc)) => {
                    expr = self.finish_call(expr, loc)?;
                }
                Some((TokenType::LeftBracket, loc)) => {
                    expr = self.finish_get_index(expr, loc)?;
                }
                Some((token_type, loc)) => panic!("call: unexpected token type: {:?} loc={:?}", token_type, loc),
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, expr: Expr, loc: SourceLoc) -> Result<Expr, ParseErrorCause> {
        // LeftParen token already consumed.
        let mut args = Vec::new();
        if ! self.check(TokenType::RightParen) {
            loop {
                if args.len() >= 255 {
                    return Err(self.error_from_peek("Cannot have more than 255 arguments."));
                }
                args.push(self.expression()?);

                if ! self.match_token(TokenType::Comma) { break; }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after arguments.")?;

        Ok(Expr::Call(Box::new(expr), args, loc))
    }

    fn finish_get_index(&mut self, expr: Expr, loc: SourceLoc) -> Result<Expr, ParseErrorCause> {
        // LeftBracket token already consumed.
        let index = self.expression()?;
        self.consume(TokenType::RightBracket, "Expect ']' after index.")?;

        Ok(Expr::GetIndex(Box::new(expr), Box::new(index), loc))
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
            TokenType::Identifier | TokenType::This => {
                match self.peek() {
                    None => panic!("primary: identifier case: this shouldn't happen"),
                    Some(token) => {
                        let loc = SourceLoc::from(token);

                        Expr::Variable(token.lexeme.to_string(), Cell::new(VarLoc::placeholder()), loc)
                    }
                }
            }
            TokenType::Fun => {
                match self.peek() {
                    None => panic!("primary: fun case: this shouldn't happen"),
                    Some(token) => {
                        let loc = SourceLoc::from(token);

                        self.advance();
                        already_advanced = true;

                        if self.match_token(TokenType::Identifier) {
                            return Err(self.error_from_last("Expect function in expression context to not have a name."));
                        }

                        let fun_def = self.function_definition(&loc)?;

                        Expr::Function(Box::new(fun_def))
                    }
                }
            }
            TokenType::LeftParen => {
                self.advance();
                already_advanced = true;

                let expr = self.expression()?;
                self.consume(TokenType::RightParen, "Missing close parenthesis.")?;

                Expr::Grouping(Box::new(expr))
            }
            TokenType::LeftBracket => {
                self.advance();
                already_advanced = true;

                let mut elements = Vec::new();
                if ! self.check(TokenType::RightBracket) {
                    loop {
                        elements.push(self.expression()?);

                        if ! self.match_token(TokenType::Comma) { break; }
                    }
                }
                self.consume(TokenType::RightBracket, "Expect ']' after array elements.")?;

                Expr::LiteralArray(elements)
            }
            TokenType::Super => {
                match self.peek() {
                    None => panic!("primary: super case: this shouldn't happen"),
                    Some(token) => {
                        let loc = SourceLoc::from(token);

                        self.advance();
                        already_advanced = true;

                        self.consume(TokenType::Dot, "Expect '.' after 'super'.")?;
                        let (id, _) = self.consume_identifier("Expect superclass method name.")?;

                        Expr::Super(Cell::new(VarLoc::placeholder()), id, loc)
                    }
                }
            }
            _ => {
                // We need to advance here to prevent looping forever.
                self.advance();
                return Err(self.error_from_last("Expect expression."));
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

    // Like check(), but looks ahead further one more token.
    fn check_next(&self, token_type: TokenType) -> bool {
        ! self.is_at_end() && match self.peek_next() {
            None => false,
            Some(token) => token.token_type == token_type,
        }
    }

    // Checks the next token.  If it is the given type, consumes it.
    fn match_token(&mut self, token_type: TokenType) -> bool {
        let found = self.check(token_type);
        if found {
            self.advance();
        }

        found
    }

    // Like match_token(), but allows several token types.  The token type that
    // matched is returned so that the caller can tell which kind matched.
    fn matches(&mut self, token_types: &[TokenType]) -> Option<(TokenType, SourceLoc)> {
        let token_type: Option<_> = match self.peek() {
            None => None,
            Some(token) => {
                if token_types.contains(&token.token_type) {
                    let loc = SourceLoc::from(token);

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

    fn peek_next(&self) -> Option<&Token<'a>> {
        self.tokens.get(self.current + 1)
    }

    fn peek(&self) -> Option<&Token<'a>> {
        self.tokens.get(self.current)
    }

    fn previous(&self) -> Option<&Token<'a>> {
        self.tokens.get(self.current - 1)
    }

    fn previous_source_loc(&self) -> SourceLoc {
        match self.previous() {
            Some(token) => SourceLoc::from(token),
            // If there was no last token, this is an edge case where
            // there were no tokens.
            None => SourceLoc::default(),
        }
    }

    fn peek_source_loc(&self) -> SourceLoc {
        match self.peek() {
            Some(token) => SourceLoc::from(token),
            None => {
                // We're at the end of file, so use the line of the last token.
                match self.previous() {
                    Some(token) => SourceLoc::from(token),
                    // If there was no last token, this is an edge case where
                    // there were no tokens.
                    None => SourceLoc::default(),
                }
            }
        }
    }

    fn error_from_last(&self, message: &str) -> ParseErrorCause {
        match self.previous() {
            Some(token) => ParseErrorCause::new_with_location(self.previous_source_loc(), token.lexeme, message),
            None => ParseErrorCause::new(self.previous_source_loc(), message),
        }
    }

    fn error_from_peek(&self, message: &str) -> ParseErrorCause {
        match self.peek() {
            Some(token) => ParseErrorCause::new_with_location(self.peek_source_loc(), token.lexeme, message),
            None => ParseErrorCause::new(self.peek_source_loc(), message),
        }
    }

    fn is_at_end(&self) -> bool {
        match self.peek() {
            None => true,
            Some(token) => token.token_type == TokenType::Eof,
        }
    }

    fn consume(&mut self, token_type: TokenType, error_message: &str)
        -> Result<(), ParseErrorCause>
    {
        match self.matches(&[token_type]) {
            Some(_) => Ok(()), // Expected.
            None => Err(self.error_from_peek(error_message)),
        }
    }

    // Match an identifier and return everything needed to use it.  This is
    // different from most token types because most don't carry extra
    // information, like this string, in this case.
    fn consume_identifier(&mut self, error_message: &str)
        -> Result<(String, SourceLoc), ParseErrorCause>
    {
        let (id, loc) = match self.peek() {
            Some(token) => {
                if token.token_type == TokenType::Identifier {
                    (token.lexeme, SourceLoc::from(token))
                }
                else {
                    return Err(self.error_from_peek(error_message));
                }
            }
            None => {
                return Err(self.error_from_peek(error_message));
            }
        };
        // Consume the identifier.
        self.advance();

        Ok((id.to_string(), loc))
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if let Some(token) = self.previous() {
                match token.token_type {
                    TokenType::Class
                    | TokenType::Fun
                    | TokenType::Var
                    | TokenType::For
                    | TokenType::If
                    | TokenType::While
                    | TokenType::Print
                    | TokenType::Return => return,
                    _ => (),
                }
            }
            self.advance();
        }
    }
}
