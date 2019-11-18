This is an interpreter for the Lox language from
[Crafting Interpreters](https://www.craftinginterpreters.com/).  Lox is a
dynamic, object-oriented language loosely based on JavaScript.  See the
[language overview](https://www.craftinginterpreters.com/the-lox-language.html)
for features of the language.

The priorities for this project are:

1. Fun!
2. Learning

#### Reference Tests

To ensure correctness, I run this against the [official test suite](https://github.com/munificent/craftinginterpreters/tree/master/test).

Current status:

- 224 tests passed
- 15 tests failed
- 8 failures are due to optional features and are false positives
- 7 failures are minor differences in parse error messages

Like the reference implementation, we skip limit tests that don't apply.

#### Differences

Because of my priorities, I may implement some things and not others.

Optional challenges and features implemented:

- Pass the reference implementation's test suite (getting there, at least)
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

Features I will probably never implement:

- Better runtime performance by using a bytecode virtual machine

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
