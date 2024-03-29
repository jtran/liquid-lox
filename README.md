# Liquid Lox

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
- Written in 100% safe Rust
- Free software license

A second entire implementation using a byte-code virtual machine is under
development.

See below for the gory details.

## Tree-Walking Interpreter

This project contains two separate implementations of the language.

The first implementation, the tree-walking interpeter, is feature-complete and
is designed to be simple for experimentation.

### Differences from the Reference Implementation jlox

Optional challenges and features implemented in the tree-walking interpreter:

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

- Because the interpreter uses reference counting and doesn't (yet) implement a
  garbage collector, many input programs create reference cycles and leak
  memory.

## Bytecode VM

The bytecode virtual machine is the second implementation in this project and is
currently under development.  Major features that haven't been done yet are:

- functions
- closures
- classes
- garbage collection

### Differences from the Reference Implementation clox

Optional features implemented in the bytecode VM:

- `break` and `continue` statements inside loops

Custom features not mentioned in the book:

- Unicode variable names using grapheme clusters, not just code points

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

To use the byte-code virtual machine, which is still a work-in-progress, use:

```shell
lox --vm
```

## Running Tests

```shell
cargo test
```

Note: This runs the internal test suite.  The
[reference tests](https://github.com/munificent/craftinginterpreters/tree/master/test)
aren't included in this repo.
