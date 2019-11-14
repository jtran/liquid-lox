use crate::scanner::*;
use crate::token::*;

#[test]
fn test_scan_single_tokens() {
    let mut s = Scanner::new("!");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::Bang, "!", None, None, 1, 1),
                                        Token::new(TokenType::Eof, "", None, None, 1, 2)]));
    let mut s = Scanner::new(".");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::Dot, ".", None, None, 1, 1),
                                        Token::new(TokenType::Eof, "", None, None, 1, 2)]));
    let mut s = Scanner::new("=");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::Equal, "=", None, None, 1, 1),
                                        Token::new(TokenType::Eof, "", None, None, 1, 2)]));
    let mut s = Scanner::new("<");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::Less, "<", None, None, 1, 1),
                                        Token::new(TokenType::Eof, "", None, None, 1, 2)]));
    let mut s = Scanner::new("()");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::LeftParen, "(", None, None, 1, 1),
                                        Token::new(TokenType::RightParen, ")", None, None, 1, 2),
                                        Token::new(TokenType::Eof, "", None, None, 1, 3)]));
    let mut s = Scanner::new("{}");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::LeftBrace, "{", None, None, 1, 1),
                                        Token::new(TokenType::RightBrace, "}", None, None, 1, 2),
                                        Token::new(TokenType::Eof, "", None, None, 1, 3)]));
    // Next line.
    let mut s = Scanner::new("\n-");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::Minus, "-", None, None, 2, 1),
                                        Token::new(TokenType::Eof, "", None, None, 2, 2)]));
}

#[test]
fn test_scan_double_tokens() {
    let mut s = Scanner::new("==");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::EqualEqual, "==", None, None, 1, 1),
                                        Token::new(TokenType::Eof, "", None, None, 1, 3)]));
    let mut s = Scanner::new("!=");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::BangEqual, "!=", None, None, 1, 1),
                                        Token::new(TokenType::Eof, "", None, None, 1, 3)]));
    let mut s = Scanner::new("<=");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::LessEqual, "<=", None, None, 1, 1),
                                        Token::new(TokenType::Eof, "", None, None, 1, 3)]));
}

#[test]
fn test_scan_string() {
    let mut s = Scanner::new("\"hello\"");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::String, "\"hello\"", Some("hello"), None, 1, 1),
                                        Token::new(TokenType::Eof, "", None, None, 1, 8)]));
}

#[test]
fn test_scan_multiline_string() {
    // Multi-line.
    let mut s = Scanner::new("\"hello\nthere\"");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::String, "\"hello\nthere\"", Some("hello\nthere"), None, 1, 1),
                                        Token::new(TokenType::Eof, "", None, None, 2, 7)]));
}

#[test]
fn test_scan_number() {
    let mut s = Scanner::new("9.5");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::Number, "9.5", None, Some(9.5), 1, 1),
                                        Token::new(TokenType::Eof, "", None, None, 1, 4)]));
    let mut s = Scanner::new("7");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::Number, "7", None, Some(7.0), 1, 1),
                                        Token::new(TokenType::Eof, "", None, None, 1, 2)]));
    let mut s = Scanner::new("144.25.");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::Number, "144.25", None, Some(144.25), 1, 1),
                                        Token::new(TokenType::Dot, ".", None, None, 1, 7),
                                        Token::new(TokenType::Eof, "", None, None, 1, 8)]));
}

#[test]
fn test_scan_identifier() {
    let mut s = Scanner::new("foo");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::Identifier, "foo", None, None, 1, 1),
                                        Token::new(TokenType::Eof, "", None, None, 1, 4)]));
    let mut s = Scanner::new("foo_bar2");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::Identifier, "foo_bar2", None, None, 1, 1),
                                        Token::new(TokenType::Eof, "", None, None, 1, 9)]));
    let mut s = Scanner::new("π");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::Identifier, "π", None, None, 1, 1),
                                        Token::new(TokenType::Eof, "", None, None, 1, 2)]));
    // Multi-character grapheme cluster.
    let mut s = Scanner::new("y̆");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::Identifier, "y̆", None, None, 1, 1),
                                        Token::new(TokenType::Eof, "", None, None, 1, 2)]));
}

#[test]
fn test_scan_keywords() {
    let mut s = Scanner::new("and");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::And, "and", None, None, 1, 1),
                                        Token::new(TokenType::Eof, "", None, None, 1, 4)]));
    let mut s = Scanner::new("break");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::Break, "break", None, None, 1, 1),
                                        Token::new(TokenType::Eof, "", None, None, 1, 6)]));
    let mut s = Scanner::new("class");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::Class, "class", None, None, 1, 1),
                                        Token::new(TokenType::Eof, "", None, None, 1, 6)]));
    let mut s = Scanner::new("continue");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::Continue, "continue", None, None, 1, 1),
                                        Token::new(TokenType::Eof, "", None, None, 1, 9)]));
    let mut s = Scanner::new("else");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::Else, "else", None, None, 1, 1),
                                        Token::new(TokenType::Eof, "", None, None, 1, 5)]));
    let mut s = Scanner::new("false");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::False, "false", None, None, 1, 1),
                                        Token::new(TokenType::Eof, "", None, None, 1, 6)]));
    let mut s = Scanner::new("for");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::For, "for", None, None, 1, 1),
                                        Token::new(TokenType::Eof, "", None, None, 1, 4)]));
    let mut s = Scanner::new("fun");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::Fun, "fun", None, None, 1, 1),
                                        Token::new(TokenType::Eof, "", None, None, 1, 4)]));
    let mut s = Scanner::new("if");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::If, "if", None, None, 1, 1),
                                        Token::new(TokenType::Eof, "", None, None, 1, 3)]));
    let mut s = Scanner::new("nil");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::Nil, "nil", None, None, 1, 1),
                                        Token::new(TokenType::Eof, "", None, None, 1, 4)]));
    let mut s = Scanner::new("or");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::Or, "or", None, None, 1, 1),
                                        Token::new(TokenType::Eof, "", None, None, 1, 3)]));
    let mut s = Scanner::new("print");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::Print, "print", None, None, 1, 1),
                                        Token::new(TokenType::Eof, "", None, None, 1, 6)]));
    let mut s = Scanner::new("return");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::Return, "return", None, None, 1, 1),
                                        Token::new(TokenType::Eof, "", None, None, 1, 7)]));
    let mut s = Scanner::new("this");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::This, "this", None, None, 1, 1),
                                        Token::new(TokenType::Eof, "", None, None, 1, 5)]));
    let mut s = Scanner::new("true");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::True, "true", None, None, 1, 1),
                                        Token::new(TokenType::Eof, "", None, None, 1, 5)]));
    let mut s = Scanner::new("var");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::Var, "var", None, None, 1, 1),
                                        Token::new(TokenType::Eof, "", None, None, 1, 4)]));
    let mut s = Scanner::new("while");
    assert_eq!(s.scan_tokens(), Ok(vec![Token::new(TokenType::While, "while", None, None, 1, 1),
                                        Token::new(TokenType::Eof, "", None, None, 1, 6)]));
}
