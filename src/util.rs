pub fn error(line: usize, message: &str) {
    println!("line {}: Error: {}", line, message);
}

pub fn is_digit(grapheme: &str) -> bool {
    match grapheme {
        "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" => true,
        _ => false,
    }
}

pub fn is_alphabetic(grapheme: &str) -> bool {
    grapheme.chars().all(|c| c.is_alphabetic())
}

pub fn is_alphanumeric(grapheme: &str) -> bool {
    grapheme.chars().all(|c| c.is_alphanumeric())
}
