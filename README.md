## Liquid Lox

This is an interpreter for the Lox language from
[Crafting Interpreters](https://www.craftinginterpreters.com/).  Lox is a
dynamic, object-oriented language loosely based on JavaScript.  See the
[language overview](https://www.craftinginterpreters.com/the-lox-language.html)
for features of the language.

A few notable highlights of this implementation:

- Passing the reference implementation's test suite
- Arrays
- Unicode variable names
- `break` and `continue`
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

- Unicode variable names
- Arrays
  - literals `[1, 2, 3]`
  - get `array[index]`
  - set `array[index] = exp`
  - `array_create(length, fill_value)`
  - `array_length(array)`
  - `array_push(array, item)`
  - `array_pop(array)`

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

- Because the interpreter uses reference counting and doesn't (yet) implement a
  garbage collector, many input programs create reference cycles and leak
  memory.

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
