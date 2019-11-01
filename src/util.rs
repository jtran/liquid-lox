use crate::source_loc::*;

pub fn error(source_loc: &SourceLoc, message: &str) {
    eprintln!("[line {}:{}] Error: {}", source_loc.line, source_loc.column, message);
}

pub fn error_at_token(source_loc: &SourceLoc, token: &str, message: &str) {
    eprintln!("[line {}:{}] Error at '{}': {}", source_loc.line, source_loc.column, token, message);
}
