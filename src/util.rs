pub fn error(line: usize, message: &str) {
    println!("line {}: Error: {}", line, message);
}

// Returns true if two pointers point to the same object.  You can call this
// with references, and they will be automatically converted.
pub fn same_object<T>(x: *const T, y: *const T) -> bool {
    x == y
}
