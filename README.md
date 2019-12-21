## Liquid Lox

This is an interpreter for the Lox language from
[Crafting Interpreters](https://www.craftinginterpreters.com/).  Lox is a
dynamic, object-oriented language loosely based on JavaScript.  See the
[language overview](https://www.craftinginterpreters.com/the-lox-language.html)
for features of the language.

A few notable highlights of this implementation:

- Passing the reference implementation's test suite
- Arrays
- Metaclasses
- Unicode variable names
- Garbage collection
- Written in 100% safe Rust
- Free software license

See below for the gory details.

### Differences from the Reference Implementation

Optional challenges and features implemented:

- Pass the reference implementation's test suite (see status for exact counts)
- Column number of source, in addition to line number, is displayed in error
  messages
- Plus operator coerces to string when one value is a string
- Division by zero is a runtime error
- REPL accepts expressions and statements
- `break` and `continue` statements inside loops
- anonymous function expressions
- Look up variables by index, instead of by name, which is much faster
- Static class methods that can access `this`
- Get and set properties on classes using metaclasses

Custom features not mentioned in the book:

- Unicode variable names using grapheme clusters, not just code points
- Arrays
  - literals `[1, 2, 3]`
  - get `arr[index]`
  - set `arr[index] = exp`
  - Native function: `array_create(length, fill_value)`
  - Native function: `array_length(arr)`
  - Native function: `array_push(arr, item)`
  - Native function: `array_pop(arr)`

Features that will probably never be implemented:

- Better runtime performance by using a bytecode virtual machine

### Reference Tests

To ensure correctness, the
[official test suite](https://github.com/munificent/craftinginterpreters/tree/master/test)
is run.

Current status:

- 224 tests passed
- 15 tests failed
- 8 failures are due to optional features and are false positives
- 7 failures are minor differences in parse error messages

Like the reference implementation, we skip limit tests that don't apply.

### Known Issues

- The garbage collector is very conservative.  It assumes that anything in scope
is reachable, even in enclosing scopes.  For example, if you create a closure in
a deeply nested scope and put it in a data structure, everything in all
enclosing scopes is not collected.

## Building

```shell
cargo build --release
```

The result will be `./target/release/lox`.

## Usage

Run a Lox script:

```shell
lox script.lox
```

Run a Lox REPL:

```shell
lox
```

## Running Tests

```shell
cargo test
```

Note: This runs the internal test suite.  The
[reference tests](https://github.com/munificent/craftinginterpreters/tree/master/test)
aren't included in this repo.
