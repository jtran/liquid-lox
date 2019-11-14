use std::collections::HashMap;
use std::iter::Peekable;
use std::mem;

use unicode_segmentation::{GraphemeIndices, UnicodeSegmentation};

use crate::error::*;
use crate::source_loc::*;
use crate::token::*;

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut m = HashMap::new();
        use crate::token::TokenType::*;
        m.insert("and", And);
        m.insert("break", Break);
        m.insert("class", Class);
        m.insert("continue", Continue);
        m.insert("else", Else);
        m.insert("false", False);
        m.insert("for", For);
        m.insert("fun", Fun);
        m.insert("if", If);
        m.insert("nil", Nil);
        m.insert("or", Or);
        m.insert("print", Print);
        m.insert("return", Return);
        m.insert("super", Super);
        m.insert("this", This);
        m.insert("true", True);
        m.insert("var", Var);
        m.insert("while", While);

        m
    };
}

#[derive(Clone)]
pub struct Scanner<'source, 'g> {
    source: &'source str,
    tokens: Vec<Token<'source>>,
    errors: Vec<ParseErrorCause>,
    grapheme_indices: Peekable<GraphemeIndices<'g>>,
    start: usize,
    current: usize,
    line: u32,
    column: u16,
    start_column: u16,
    eof: bool,
}

impl<'source, 'g> Scanner<'source, 'g> where 'source: 'g {
    pub fn new(source: &'source str) -> Scanner<'source, 'g> {
        Scanner {
            source,
            grapheme_indices: source.grapheme_indices(true).peekable(),
            tokens: Vec::new(),
            errors: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
            column: 1,
            start_column: 1,
            eof: false,
        }
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<Token>, ParseError> {
        while ! self.is_at_end() {
            // We are at the beginning of the next lexeme.
            self.start = self.peek_index();
            self.scan_token();
        }

        self.start_column = self.column.saturating_sub(1);
        self.add_token(TokenType::Eof);

        let tokens = mem::replace(&mut self.tokens, Vec::new());
        let errors = mem::replace(&mut self.errors, Vec::new());

        if errors.is_empty() {
            Ok(tokens)
        } else {
            Err(ParseError::new(errors))
        }
    }

    fn scan_token(&mut self) {
        self.start_column = self.column;
        match self.advance() {
            None => (),
            Some((_, grapheme_cluster)) => {
                use crate::token::TokenType::*;
                match grapheme_cluster {
                    "(" => self.add_token(LeftParen),
                    ")" => self.add_token(RightParen),
                    "{" => self.add_token(LeftBrace),
                    "}" => self.add_token(RightBrace),
                    "," => self.add_token(Comma),
                    "." => self.add_token(Dot),
                    "-" => self.add_token(Minus),
                    "+" => self.add_token(Plus),
                    ";" => self.add_token(Semicolon),
                    "*" => self.add_token(Star),
                    "!" => {
                        if self.matches("=") {
                            self.add_token(BangEqual);
                        } else {
                            self.add_token(Bang);
                        }
                    }
                    "=" => {
                        if self.matches("=") {
                            self.add_token(EqualEqual);
                        } else {
                            self.add_token(Equal);
                        }
                    }
                    "<" => {
                        if self.matches("=") {
                            self.add_token(LessEqual);
                        } else {
                            self.add_token(Less);
                        }
                    }
                    ">" => {
                        if self.matches("=") {
                            self.add_token(GreaterEqual);
                        } else {
                            self.add_token(Greater);
                        }
                    }
                    "/" => {
                        if self.matches("/") {
                            // A comment until the end of the line.
                            self.advance_to_eol();
                        } else {
                            self.add_token(Slash);
                        }
                    }
                    " " | "\r" | "\t" => (), // Ignore whitespace.
                    "\n" => {
                        self.line = self.line.saturating_add(1);
                        self.column = 1;
                    }
                    "\"" => self.scan_string(),
                    _ => {
                        if is_digit(grapheme_cluster) {
                            self.scan_number();
                        }
                        else if is_alphabetic(grapheme_cluster) {
                            self.scan_identifier();
                        }
                        else {
                            self.error(ParseErrorCause::new(SourceLoc::new(self.line, self.column), &format!("Unexpected token: {}", grapheme_cluster)));
                        }
                    }
                };
            }
        }
    }

    // Conditionally advance if the next grapheme cluster matches an expected
    // string.  Returns true if we matched.
    fn matches(&mut self, expected: &str) -> bool {
        if self.is_at_end() {
            return false;
        }

        match self.grapheme_indices.peek() {
            None => return false,
            Some((_, grapheme_cluster)) => {
                if *grapheme_cluster != expected {
                    return false;
                }
            }
        };

        // Consume this cluster when it's expected.
        self.advance();

        true
    }

    fn peek_index(&mut self) -> usize {
        match self.grapheme_indices.peek() {
            None => self.source.len(),
            Some((i, _)) => *i,
        }
    }

    // This is looking ahead 2 characters.
    fn peek_next_grapheme(&mut self) -> Option<&'g str> {
        match self.grapheme_indices.peek() {
            None => return None,
            Some(_) => {
                let mut cloned = self.grapheme_indices.clone();
                cloned.next();
                match cloned.peek() {
                    None => return None,
                    Some((_, grapheme_cluster)) => {
                        return Some(grapheme_cluster)
                    }
                }
            }
        }
    }

    fn is_match(&mut self, expected: &str) -> bool {
        match self.grapheme_indices.peek() {
            None => false,
            Some((_, grapheme_cluster)) => *grapheme_cluster == expected,
        }
    }

    // Advance the grapheme cluster iterator.
    fn advance(&mut self) -> Option<(usize, &'g str)> {
        self.column = self.column.saturating_add(1);

        match self.grapheme_indices.next() {
            None => {
                self.eof = true;

                None
            }
            Some((i, cluster)) => {
                self.current = i;

                Some((i, cluster))
            }
        }
    }

    fn advance_to_eol(&mut self) {
        loop {
            match self.grapheme_indices.peek() {
                None => break,
                Some((_, grapheme_cluster)) => {
                    if *grapheme_cluster == "\n" {
                        // Keep the newline next.
                        break;
                    }
                }
            };
            self.advance();
        }
    }

    fn scan_string(&mut self) {
        let start_index = self.peek_index();
        let start_line = self.line;

        while ! self.is_match("\"") && ! self.is_at_end() {
            match self.grapheme_indices.peek() {
                None => (),
                Some((_, grapheme_cluster)) => {
                    if *grapheme_cluster == "\n" {
                        self.line = self.line.saturating_add(1);
                        self.column = 0;
                    }
                }
            };
            self.advance();
        }

        // Unterminated string.
        if self.is_at_end() {
            let column = self.column.saturating_sub(1);
            self.error(ParseErrorCause::new(SourceLoc::new(self.line, column), "Unterminated string."));
            return;
        }

        // The closing quote.
        self.advance();

        // Trim the surrounding quotes.
        let value = &self.source[start_index..self.current];
        self.add_string_literal_token(value, start_line);
    }

    fn scan_number(&mut self) {
        loop {
            match self.grapheme_indices.peek() {
                None => break,
                Some((_, grapheme_cluster)) => {
                    if ! is_digit(grapheme_cluster) {
                        break;
                    }
                }
            };
            self.advance();
        }

        // Look for a fractional part.
        if self.is_match(".") {
            if let Some(c) = self.peek_next_grapheme() {
                if is_digit(c) {
                    // Consume the dot.
                    self.advance();
                }
            }

            loop {
                match self.grapheme_indices.peek() {
                    None => break,
                    Some((_, grapheme_cluster)) => {
                        if ! is_digit(grapheme_cluster) {
                            break;
                        }
                    }
                };
                self.advance();
            }
        }

        let value = &self.source[self.start..self.peek_index()];
        let number: f64 = value.parse().unwrap_or_else(|_| panic!("Unable to parse string as f64: {}", value));
        self.add_number_literal_token(number);
    }

    fn scan_identifier(&mut self) {
        loop {
            match self.grapheme_indices.peek() {
                None => break,
                Some((_, grapheme_cluster)) => {
                    if ! is_alphanumeric(grapheme_cluster) {
                        break;
                    }
                }
            };
            self.advance();
        }

        let text = &self.source[self.start..self.peek_index()];

        // See if the identifier is a reserved word.
        let token_type = match KEYWORDS.get(text) {
            None => TokenType::Identifier,
            Some(token_type) => *token_type,
        };

        self.add_token(token_type);
    }

    fn is_at_end(&self) -> bool {
        self.eof
    }

    fn error(&mut self, error: ParseErrorCause) {
        self.errors.push(error);
    }

    // Add a token to the output.
    fn add_token(&mut self, token_type: TokenType) {
        let text = &self.source[self.start..self.peek_index()];
        let token = Token::new(token_type, text, None, None, self.line, self.start_column);
        self.tokens.push(token);
    }

    fn add_string_literal_token(&mut self, value: &'source str, start_line: u32) {
        let text = &self.source[self.start..self.peek_index()];
        let token = Token::new(TokenType::String, text, Some(value), None, start_line, self.start_column);
        self.tokens.push(token);
    }

    fn add_number_literal_token(&mut self, value: f64) {
        let text = &self.source[self.start..self.peek_index()];
        let token = Token::new(TokenType::Number, text, None, Some(value), self.line, self.start_column);
        self.tokens.push(token);
    }
}

fn is_digit(grapheme: &str) -> bool {
    // Note: built-in is_numeric() uses a more complicated unicode definition of
    // numeric.
    match grapheme {
        "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" => true,
        _ => false,
    }
}

fn is_alphabetic(grapheme: &str) -> bool {
    // Only look at the first base character.
    match grapheme.chars().next() {
        None => true,
        Some(c) => c.is_alphabetic() || c == '_',
    }
}

fn is_alphanumeric(grapheme: &str) -> bool {
    // Only look at the first base character.
    match grapheme.chars().next() {
        None => true,
        Some(c) => c.is_alphanumeric() || c == '_',
    }
}
