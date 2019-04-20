use unicode_segmentation::{GraphemeIndices, UnicodeSegmentation};

use std::collections::HashMap;
use std::iter::Peekable;
use std::mem;
use crate::token::*;
use crate::util;

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut m = HashMap::new();
        use crate::token::TokenType::*;
        m.insert("and", And);
        m.insert("break", Break);
        m.insert("class", Class);
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
    grapheme_indices: Peekable<GraphemeIndices<'g>>,
    start: usize,
    current: usize,
    line: usize,
    eof: bool,
}

impl <'source, 'g> Scanner<'source, 'g> where 'source: 'g {
    pub fn new(source: &'source str) -> Scanner<'source, 'g> {
        Scanner {
            source,
            grapheme_indices: source.grapheme_indices(true).peekable(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
            eof: false,
        }
    }

    pub fn scan_tokens(&mut self) -> Vec<Token> {
        while ! self.is_at_end() {
            // We are at the beginning of the next lexeme.
            self.start = self.peek_index();
            self.scan_token();
        };

        self.add_token(TokenType::Eof);

        mem::replace(&mut self.tokens, Vec::new())
    }

    fn scan_token(&mut self) {
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
                        if self.matches("=") { self.add_token(BangEqual) }
                        else { self.add_token(Bang) };
                    }
                    "=" => {
                        if self.matches("=") { self.add_token(EqualEqual) }
                        else { self.add_token(Equal) };
                    }
                    "<" => {
                        if self.matches("=") { self.add_token(LessEqual) }
                        else { self.add_token(Less) };
                    }
                    ">" => {
                        if self.matches("=") { self.add_token(GreaterEqual) }
                        else { self.add_token(Greater) };
                    }
                    "/" => {
                        if self.matches("/") {
                            // A comment until the end of the line.
                            self.advance_to_eol();
                        }
                        else {
                            self.add_token(Slash);
                        }
                    }
                    " " | "\r" | "\t" => (), // Ignore whitespace.
                    "\n" => self.line += 1,
                    "\"" => self.scan_string(),
                    _ => {
                        if is_digit(grapheme_cluster) {
                            self.scan_number();
                        }
                        else if is_alphabetic(grapheme_cluster) {
                            self.scan_identifier();
                        }
                        else {
                            util::error(self.line, &format!("Unexpected token: {}", grapheme_cluster));
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
            Some((i,_)) => *i,
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
            Some((_,grapheme_cluster)) => *grapheme_cluster == expected,
        }
    }

    // Advance the grapheme cluster iterator.
    fn advance(&mut self) -> Option<(usize, &'g str)> {
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
                Some((_,grapheme_cluster)) => {
                    if *grapheme_cluster == "\n" {
                        self.line += 1;
                    }
                }
            };
            self.advance();
        }

        // Unterminated string.
        if self.is_at_end() {
            util::error(self.line, "Unterminated string");
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

    // Add a token to the output.
    fn add_token(&mut self, token_type: TokenType) {
        let text = &self.source[self.start..self.peek_index()];
        let token = Token::new(token_type, text, None, None, self.line);
        self.tokens.push(token);
    }

    fn add_string_literal_token(&mut self, value: &'source str, start_line: usize) {
        let text = &self.source[self.start..self.peek_index()];
        let token = Token::new(TokenType::String, text, Some(value), None, start_line);
        self.tokens.push(token);
    }

    fn add_number_literal_token(&mut self, value: f64) {
        let text = &self.source[self.start..self.peek_index()];
        let token = Token::new(TokenType::Number, text, None, Some(value), self.line);
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
        Some(c) => {
            c.is_alphabetic() || c == '_'
        }
    }
}

fn is_alphanumeric(grapheme: &str) -> bool {
    // Only look at the first base character.
    match grapheme.chars().next() {
        None => true,
        Some(c) => {
            c.is_alphanumeric() || c == '_'
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scan_single_tokens() {
        let mut s = Scanner::new("!");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::Bang, "!", None, None, 1),
                                         Token::new(TokenType::Eof, "", None, None, 1)]);
        let mut s = Scanner::new(".");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::Dot, ".", None, None, 1),
                                         Token::new(TokenType::Eof, "", None, None, 1)]);
        let mut s = Scanner::new("=");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::Equal, "=", None, None, 1),
                                         Token::new(TokenType::Eof, "", None, None, 1)]);
        let mut s = Scanner::new("<");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::Less, "<", None, None, 1),
                                         Token::new(TokenType::Eof, "", None, None, 1)]);
        let mut s = Scanner::new("()");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::LeftParen, "(", None, None, 1),
                                         Token::new(TokenType::RightParen, ")", None, None, 1),
                                         Token::new(TokenType::Eof, "", None, None, 1)]);
        let mut s = Scanner::new("{}");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::LeftBrace, "{", None, None, 1),
                                         Token::new(TokenType::RightBrace, "}", None, None, 1),
                                         Token::new(TokenType::Eof, "", None, None, 1)]);
        // Next line.
        let mut s = Scanner::new("\n-");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::Minus, "-", None, None, 2),
                                         Token::new(TokenType::Eof, "", None, None, 2)]);
    }

    #[test]
    fn test_scan_double_tokens() {
        let mut s = Scanner::new("==");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::EqualEqual, "==", None, None, 1),
                                         Token::new(TokenType::Eof, "", None, None, 1)]);
        let mut s = Scanner::new("!=");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::BangEqual, "!=", None, None, 1),
                                         Token::new(TokenType::Eof, "", None, None, 1)]);
        let mut s = Scanner::new("<=");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::LessEqual, "<=", None, None, 1),
                                         Token::new(TokenType::Eof, "", None, None, 1)]);
    }

    #[test]
    fn test_scan_string() {
        let mut s = Scanner::new("\"hello\"");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::String, "\"hello\"", Some("hello"), None, 1),
                                         Token::new(TokenType::Eof, "", None, None, 1)]);
        // Multi-line.
        let mut s = Scanner::new("\"hello\nthere\"");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::String, "\"hello\nthere\"", Some("hello\nthere"), None, 1),
                                         Token::new(TokenType::Eof, "", None, None, 2)]);
    }

    #[test]
    fn test_scan_number() {
        let mut s = Scanner::new("9.5");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::Number, "9.5", None, Some(9.5), 1),
                                         Token::new(TokenType::Eof, "", None, None, 1)]);
        let mut s = Scanner::new("7");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::Number, "7", None, Some(7.0), 1),
                                         Token::new(TokenType::Eof, "", None, None, 1)]);
        let mut s = Scanner::new("144.25.");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::Number, "144.25", None, Some(144.25), 1),
                                         Token::new(TokenType::Dot, ".", None, None, 1),
                                         Token::new(TokenType::Eof, "", None, None, 1)]);
    }

    #[test]
    fn test_scan_identifier() {
        let mut s = Scanner::new("foo");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::Identifier, "foo", None, None, 1),
                                         Token::new(TokenType::Eof, "", None, None, 1)]);
        let mut s = Scanner::new("foo_bar2");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::Identifier, "foo_bar2", None, None, 1),
                                         Token::new(TokenType::Eof, "", None, None, 1)]);
        let mut s = Scanner::new("π");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::Identifier, "π", None, None, 1),
                                         Token::new(TokenType::Eof, "", None, None, 1)]);
        // Multi-character grapheme cluster.
        let mut s = Scanner::new("y̆");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::Identifier, "y̆", None, None, 1),
                                         Token::new(TokenType::Eof, "", None, None, 1)]);
    }

    #[test]
    fn test_scan_keywords() {
        let mut s = Scanner::new("and");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::And, "and", None, None, 1),
                                         Token::new(TokenType::Eof, "", None, None, 1)]);
        let mut s = Scanner::new("break");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::Break, "break", None, None, 1),
                                         Token::new(TokenType::Eof, "", None, None, 1)]);
        let mut s = Scanner::new("class");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::Class, "class", None, None, 1),
                                         Token::new(TokenType::Eof, "", None, None, 1)]);
        let mut s = Scanner::new("else");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::Else, "else", None, None, 1),
                                         Token::new(TokenType::Eof, "", None, None, 1)]);
        let mut s = Scanner::new("false");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::False, "false", None, None, 1),
                                         Token::new(TokenType::Eof, "", None, None, 1)]);
        let mut s = Scanner::new("for");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::For, "for", None, None, 1),
                                         Token::new(TokenType::Eof, "", None, None, 1)]);
        let mut s = Scanner::new("fun");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::Fun, "fun", None, None, 1),
                                         Token::new(TokenType::Eof, "", None, None, 1)]);
        let mut s = Scanner::new("if");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::If, "if", None, None, 1),
                                         Token::new(TokenType::Eof, "", None, None, 1)]);
        let mut s = Scanner::new("nil");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::Nil, "nil", None, None, 1),
                                         Token::new(TokenType::Eof, "", None, None, 1)]);
        let mut s = Scanner::new("or");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::Or, "or", None, None, 1),
                                         Token::new(TokenType::Eof, "", None, None, 1)]);
        let mut s = Scanner::new("print");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::Print, "print", None, None, 1),
                                         Token::new(TokenType::Eof, "", None, None, 1)]);
        let mut s = Scanner::new("return");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::Return, "return", None, None, 1),
                                         Token::new(TokenType::Eof, "", None, None, 1)]);
        let mut s = Scanner::new("this");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::This, "this", None, None, 1),
                                         Token::new(TokenType::Eof, "", None, None, 1)]);
        let mut s = Scanner::new("true");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::True, "true", None, None, 1),
                                         Token::new(TokenType::Eof, "", None, None, 1)]);
        let mut s = Scanner::new("var");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::Var, "var", None, None, 1),
                                         Token::new(TokenType::Eof, "", None, None, 1)]);
        let mut s = Scanner::new("while");
        assert_eq!(s.scan_tokens(), vec![Token::new(TokenType::While, "while", None, None, 1),
                                         Token::new(TokenType::Eof, "", None, None, 1)]);
    }
}
