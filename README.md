This is an interpreter for the Lox language from
[Crafting Interpreters](https://www.craftinginterpreters.com/).  See the
[language overview](https://www.craftinginterpreters.com/the-lox-language.html)
for features of the language.

The priorities for this project are:

1. Fun!
2. Learning

Optional challenges and features implemented:

- Column number in source, in addition to line, is displayed in error messages
- Plus operator coerces to string when one value is a string
- Division by zero is a runtime error
- REPL accepts expressions and statements
- `break` statements inside loops
- Look up variables by index, instead of by name, which is much faster
- Static class methods

Custom features not mentioned in the book:

- Unicode variable names

#### Reference Tests

To ensure correctness, I run against the [official test suite](https://github.com/munificent/craftinginterpreters/tree/master/test).

Current status:

- 227 tests passed
- 16 tests failed
- 2 failures are due to optional features and are false positives

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

Note: This runs the internal test suite.  I do not include the
[reference tests](https://github.com/munificent/craftinginterpreters/tree/master/test)
in this repo.
