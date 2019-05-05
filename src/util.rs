use crate::source_loc::*;

pub fn error(source_loc: &SourceLoc, message: &str) {
    println!("line {}:{}: Error: {}", source_loc.line, source_loc.column, message);
}
