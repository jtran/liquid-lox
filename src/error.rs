use crate::source_loc::*;

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
    pub token: Option<String>,
    pub message: String,
}

impl ParseErrorCause {
    pub fn new(source_loc: SourceLoc, message: &str) -> ParseErrorCause {
        ParseErrorCause {
            source_loc,
            token: None,
            message: message.to_string(),
        }
    }

    pub fn new_with_location(source_loc: SourceLoc, token: &str, message: &str) -> ParseErrorCause {
        ParseErrorCause {
            source_loc,
            token: Some(token.to_string()),
            message: message.to_string(),
        }
    }
}

impl From<ParseErrorCause> for ParseError {
    fn from(error: ParseErrorCause) -> ParseError {
        ParseError { causes: vec![error] }
    }
}
