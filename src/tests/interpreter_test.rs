use std::cell::RefCell;
use std::mem;
use std::rc::Rc;

use crate::error::*;
use crate::interpreter::*;
use crate::parser::*;
use crate::resolver;
use crate::source_loc::*;
use crate::value::{ExecutionInterrupt, RuntimeError, Value};
use crate::value::Value::*;

fn interpret(code: &str) -> Result<Value, RuntimeError> {
    let mut ast = parse(code)?;
    resolver::resolve(&mut ast).map_err(|e| ParseError::from(e))?;
    let mut interpreter = Interpreter::new();

    interpreter.interpret(ast)
}

fn interpret_repl_line(code: &str) -> Result<Value, RuntimeError> {
    let mut ast = parse_repl_line(code)?;
    resolver::resolve(&mut ast).map_err(|e| ParseError::from(e))?;
    let mut interpreter = Interpreter::new();

    interpreter.interpret(ast)
}

fn eval(code: &str) -> Result<Value, RuntimeError> {
    let mut ast = parse_expression(code)?;
    resolver::resolve_expression(&mut ast).map_err(|e| ParseError::from(e))?;
    let mut interpreter = Interpreter::new();

    interpreter.evaluate(&ast)
}

#[test]
fn test_interpreter_size_of_internal_result() {
    assert_eq!(mem::size_of::<Result<Value, RuntimeError>>(), 40);
    assert_eq!(mem::size_of::<Result<Value, ExecutionInterrupt>>(), 48);
}

#[test]
fn test_eval_literals() {
    assert_eq!(eval("42"), Ok(NumberVal(42.0)));
    assert_eq!(eval("\"hello\""), Ok(StringVal(Rc::new("hello".to_string()))));
    assert_eq!(eval("true"), Ok(BoolVal(true)));
    assert_eq!(eval("false"), Ok(BoolVal(false)));
    assert_eq!(eval("nil"), Ok(NilVal));
}

#[test]
fn test_eval_binary_ops() {
    assert_eq!(eval("40 + 2"), Ok(NumberVal(42.0)));
    assert_eq!(eval("\"foo\" + \"bar\""), Ok(StringVal(Rc::new("foobar".to_string()))));
    assert_eq!(eval("40 - 10"), Ok(NumberVal(30.0)));
    assert_eq!(eval("7 * 3"), Ok(NumberVal(21.0)));
    assert_eq!(eval("10 / 2"), Ok(NumberVal(5.0)));
}

#[test]
fn test_eval_divide_by_zero() {
    let loc = SourceLoc::new(1, 3);
    assert_eq!(eval("1 / 0"), Err(RuntimeError::new(loc, "attempted to divide by zero")));
}

#[test]
fn test_eval_comparison() {
    assert_eq!(eval("true == true"), Ok(BoolVal(true)));
    assert_eq!(eval("true == 32"), Ok(BoolVal(false)));
    assert_eq!(eval("2 < 3"), Ok(BoolVal(true)));
    assert_eq!(eval("2 > 3"), Ok(BoolVal(false)));
}

#[test]
fn test_eval_unary_ops() {
    assert_eq!(eval("-6"), Ok(NumberVal(-6.0)));
    assert_eq!(eval("! true"), Ok(BoolVal(false)));
    assert_eq!(eval("! false"), Ok(BoolVal(true)));
    assert_eq!(eval("! 1"), Ok(BoolVal(false)));
    assert_eq!(eval("! 0"), Ok(BoolVal(false)));
    assert_eq!(eval("! \"\""), Ok(BoolVal(false)));
    assert_eq!(eval("! nil"), Ok(BoolVal(true)));
}

#[test]
fn test_interpret_literals() {
    assert_eq!(interpret("42;"), Ok(NumberVal(42.0)));
    assert_eq!(interpret("nil;"), Ok(NilVal));
}

#[test]
fn test_interpret_array_literals() {
    assert_eq!(interpret("[];"), Ok(ArrayVal(Rc::new(RefCell::new(Vec::new())))));
    assert_eq!(interpret("[1, 2];"), Ok(ArrayVal(Rc::new(RefCell::new(vec![
                                                 NumberVal(1.0),
                                                 NumberVal(2.0)])))));
}

#[test]
fn test_interpret_literals_negative_zero() {
    assert_eq!(interpret("\"\" + (-0);"), Ok(StringVal(Rc::new("-0".into()))));
}

#[test]
fn test_interpret_unterminated_string_literal() {
    assert_eq!(interpret("\"foo"), Err(RuntimeError::new(SourceLoc::new(1, 5), "parse error: Unterminated string.")));
}

#[test]
fn test_interpret_operators() {
    assert_eq!(interpret("40 + 2;"), Ok(NumberVal(42.0)));
    assert_eq!(interpret("\"foo\" + \"bar\";"), Ok(StringVal(Rc::new("foobar".into()))));
}

#[test]
fn test_interpret_string_plus_number_coerces() {
    // https://www.craftinginterpreters.com/evaluating-expressions.html#challenges
    assert_eq!(interpret("\"scone\" + 4;"), Ok(StringVal(Rc::new("scone4".into()))));
    assert_eq!(interpret("4 + \"scone\";"), Ok(StringVal(Rc::new("4scone".into()))));
    assert_eq!(interpret("\"scone\" + true;"), Ok(StringVal(Rc::new("sconetrue".into()))));
    assert_eq!(interpret("true + \"scone\";"), Ok(StringVal(Rc::new("truescone".into()))));
    assert_eq!(interpret("\"scone\" + nil;"), Ok(StringVal(Rc::new("sconenil".into()))));
    assert_eq!(interpret("nil + \"scone\";"), Ok(StringVal(Rc::new("nilscone".into()))));
}

#[test]
fn test_interpret_array_get_index() {
    assert_eq!(interpret("[1][0];"), Ok(NumberVal(1.0)));
    assert_eq!(interpret("var a = [1, 2]; a[1];"), Ok(NumberVal(2.0)));
    assert_eq!(interpret("var a = [1, 2]; a[2];"), Err(RuntimeError::new(SourceLoc::new(1, 18), "Array index out of bounds.")));
    assert_eq!(interpret("var a = [1, 2]; a[-1];"), Err(RuntimeError::new(SourceLoc::new(1, 18), "Array index out of bounds.")));
}

#[test]
fn test_interpret_array_set_index() {
    // Return value of set index is the rhs value.
    assert_eq!(interpret("var a = [1, 2]; a[1] = 3;"), Ok(NumberVal(3.0)));
    // The rhs value after a set index is returned from get index.
    assert_eq!(interpret("var a = [1, 2]; a[1] = 3; a[1];"), Ok(NumberVal(3.0)));
    assert_eq!(interpret("var a = [1, 2]; a[2] = 3;"), Err(RuntimeError::new(SourceLoc::new(1, 22), "Array index out of bounds.")));
    assert_eq!(interpret("var a = [1, 2]; a[-1] = 3;"), Err(RuntimeError::new(SourceLoc::new(1, 23), "Array index out of bounds.")));
}

#[test]
fn test_interpret_array_create() {
    assert_eq!(interpret("array_create(0, nil) == [];"), Ok(BoolVal(true)));
    assert_eq!(interpret("var a = array_create(2, 3); a[1];"), Ok(NumberVal(3.0)));
    assert_eq!(interpret("var a = array_create(2, 3); array_length(a);"), Ok(NumberVal(2.0)));
    assert_eq!(interpret("array_create(nil, 1);"), Err(RuntimeError::new(SourceLoc::new(1, 13), "Array create expects number and value.")));
    assert_eq!(interpret("array_create(0.5, 1);"), Err(RuntimeError::new(SourceLoc::new(1, 13), "Array length must be a non-negative integer.")));
}

#[test]
fn test_interpret_array_length() {
    assert_eq!(interpret("array_length([]);"), Ok(NumberVal(0.0)));
    assert_eq!(interpret("array_length([10]);"), Ok(NumberVal(1.0)));
    assert_eq!(interpret("array_length(0);"), Err(RuntimeError::new(SourceLoc::new(1, 13), "Can only get length of an array.")));
}

#[test]
fn test_interpret_array_pop() {
    // Popping from an array returns the last element.
    assert_eq!(interpret("var a = [10, 20]; array_pop(a);"), Ok(NumberVal(20.0)));
    // Popping from an array does not modify other elements.
    assert_eq!(interpret("var a = [10, 20]; array_pop(a); a[0];"), Ok(NumberVal(10.0)));
    // Popping from an array reduces the length.
    assert_eq!(interpret("var a = [10, 20]; array_pop(a); array_length(a);"), Ok(NumberVal(1.0)));
    assert_eq!(interpret("var a = []; array_pop(a);"), Err(RuntimeError::new(SourceLoc::new(1, 22), "Cannot pop on an empty array.")));
    assert_eq!(interpret("array_pop(0);"), Err(RuntimeError::new(SourceLoc::new(1, 10), "Can only pop on an array.")));
}

#[test]
fn test_interpret_array_push() {
    assert_eq!(interpret("var a = []; array_push(a, 10); a[0];"), Ok(NumberVal(10.0)));
    // Pushing to an array appends an element.
    assert_eq!(interpret("var a = [10]; array_push(a, 20); a[1];"), Ok(NumberVal(20.0)));
    // Pushing to an array does not modify other elements
    assert_eq!(interpret("var a = [10]; array_push(a, 20); a[0];"), Ok(NumberVal(10.0)));
    // Pushing to an array increases the length.
    assert_eq!(interpret("var a = [10]; array_push(a, 20); array_length(a);"), Ok(NumberVal(2.0)));
    assert_eq!(interpret("array_push(0, 1);"), Err(RuntimeError::new(SourceLoc::new(1, 11), "Can only push on an array.")));
}

#[test]
fn test_interpret_array_sum() {
    assert_eq!(interpret("
var a = [1, 2, 3];
var sum = 0;
for (var i = 0; i < array_length(a); i = i + 1) {
    sum = sum + a[i];
}
sum;
    "), Ok(NumberVal(6.0)));
}

#[test]
fn test_interpret_print() {
    assert_eq!(interpret("print \"print test\";"), Ok(NilVal));
}

#[test]
fn test_interpret_var() {
    assert_eq!(interpret("var x;"), Ok(NilVal));
    assert_eq!(interpret("var x = 1;"), Ok(NilVal));
    assert_eq!(interpret("var x = x;"), Err(RuntimeError::new(SourceLoc::new(1, 9), "parse error: Cannot read local variable in its own initializer.")));
}

#[test]
fn test_interpret_unicode_var_name() {
    assert_eq!(interpret("var λ = 2; λ;"), Ok(NumberVal(2.0)));
}

#[test]
fn test_interpret_var_use() {
    assert_eq!(interpret("var x = 1; x;"), Ok(NumberVal(1.0)));
    assert_eq!(interpret("var x = 1; var y = 3; x = y = 5; x;"), Ok(NumberVal(5.0)));
    assert_eq!(interpret("x;"), Err(RuntimeError::new(SourceLoc::new(1, 1), "Undefined variable 'x'.")));
    assert_eq!(interpret("var x = 1; y;"), Err(RuntimeError::new(SourceLoc::new(1, 12), "Undefined variable 'y'.")));
}

#[test]
fn test_interpret_var_assign() {
    assert_eq!(interpret("var x = 1; x = 2; x;"), Ok(NumberVal(2.0)));
    assert_eq!(interpret("x = 1;"), Err(RuntimeError::new(SourceLoc::new(1, 3), "Undefined variable 'x'.")));
}

#[test]
fn test_interpret_assign_to_this() {
    assert_eq!(interpret("var this = 1;"), Err(RuntimeError::new(SourceLoc::new(1, 5), "parse error: Expect variable name.")));
    assert_eq!(interpret("this = 1;"), Err(RuntimeError::new(SourceLoc::new(1, 6), "parse error: Invalid assignment target.")));
}

#[test]
fn test_interpret_blocks() {
    assert_eq!(interpret("var x = 1; { var x = 2; x; }"), Ok(NumberVal(2.0)));
    assert_eq!(interpret("var x = 1; { var x = 2; } x;"), Ok(NumberVal(1.0)));
    // Assignment.
    assert_eq!(interpret("var x = 1; { var x = 2; x = 3; x; }"), Ok(NumberVal(3.0)));
    assert_eq!(interpret("var x = 1; { var x = 2; x = 3; } x;"), Ok(NumberVal(1.0)));
}

#[test]
fn test_interpret_repl_line() {
    assert_eq!(interpret_repl_line("1 + 2"), Ok(NumberVal(3.0)));
    assert_eq!(interpret_repl_line("1 + 2;"), Ok(NumberVal(3.0)));
    assert_eq!(interpret_repl_line("1 + 2; 10"), Ok(NumberVal(10.0)));
}

#[test]
fn test_interpret_if() {
    assert_eq!(interpret("var x = 1; if (0 < 1) x = 2; else x = 3; x;"), Ok(NumberVal(2.0)));
    assert_eq!(interpret("var x = 1; if (0 > 1) x = 2; else x = 3; x;"), Ok(NumberVal(3.0)));
    assert_eq!(interpret("var x = 1; if (0 > 1) { x = 2; } else { x = 3; } x;"), Ok(NumberVal(3.0)));
    // Dangling else ambiguity.
    assert_eq!(interpret("var x = 1; if (true) if (false) x = 2; else x = 3; x;"), Ok(NumberVal(3.0)));
}

#[test]
fn test_interpret_declaration_in_if_then_body() {
    assert_eq!(interpret("if (true) var x = 1;"), Err(RuntimeError::new(SourceLoc::new(1, 11), "parse error: Expect expression.")));
}

#[test]
fn test_interpret_declaration_in_if_else_body() {
    assert_eq!(interpret("if (true) {} else var x = 1;"), Err(RuntimeError::new(SourceLoc::new(1, 19), "parse error: Expect expression.")));
}

#[test]
fn test_interpret_and_or() {
    // TODO: test short-circuiting.
    assert_eq!(interpret("1 and 2;"), Ok(NumberVal(2.0)));
    assert_eq!(interpret("false and 2;"), Ok(BoolVal(false)));
    assert_eq!(interpret("1 or 2;"), Ok(NumberVal(1.0)));
    assert_eq!(interpret("nil or 2;"), Ok(NumberVal(2.0)));
}

#[test]
fn test_interpret_while() {
    assert_eq!(interpret("var x = 0; while (x < 3) x = x + 1; x;"), Ok(NumberVal(3.0)));
}

#[test]
fn test_interpret_while_break() {
    assert_eq!(interpret("var x = 0; while (true) { if (x > 3) break; x = x + 1; } x;"), Ok(NumberVal(4.0)));
}

#[test]
fn test_interpret_top_level_break() {
    assert_eq!(interpret("1 + 2;\nbreak;"), Err(RuntimeError::new(SourceLoc::new(2, 1), "parse error: Found break statement outside of loop body")));
}

#[test]
fn test_interpret_top_level_continue() {
    assert_eq!(interpret("1 + 2;\ncontinue;"), Err(RuntimeError::new(SourceLoc::new(2, 1), "parse error: Found continue statement outside of loop body")));
}

#[test]
fn test_interpret_while_continue() {
    assert_eq!(interpret("
        var x = 0;
        var y = 0;
        while (x < 3) {
            x = x + 1;
            if (x < 3) continue;
            y = y + 1;
        }
        y;"), Ok(NumberVal(1.0)));
}

#[test]
fn test_interpret_declaration_in_while_body() {
    assert_eq!(interpret("while (true) var y = 1;"), Err(RuntimeError::new(SourceLoc::new(1, 14), "parse error: Expect expression.")));
}

#[test]
fn test_interpret_for_loop() {
    assert_eq!(interpret("var x = 1;\nfor (var i = 0; i < 3; i = i + 1)\nx = x * 2;\nx;"), Ok(NumberVal(8.0)));
    assert_eq!(interpret("var x = 1;\nfor (var i = 0; i < 3; i = i + 1)\n{ x = x * 2; }\nx;"), Ok(NumberVal(8.0)));
}

#[test]
fn test_interpret_for_loop_break() {
    assert_eq!(interpret("
        var x = 1;
        for (;; x = x + 1) {
            if (x > 3) break;
        }
        x;"), Ok(NumberVal(4.0)));
}

#[test]
fn test_interpret_for_loop_continue() {
    assert_eq!(interpret("
        var x = 0;
        var y = 0;
        var n = 0;
        for (; x < 3 and n < 20; x = x + 1) {
            n = n + 1;
            if (x >= 1) continue;
            y = y + 1;
        }
        y;"), Ok(NumberVal(1.0)));
}

#[test]
fn test_interpret_for_loop_continue_still_increments() {
    assert_eq!(interpret("
        var x = 0;
        var y = 0;
        var n = 0;
        for (; x < 3 and n < 20; x = x + 1) {
            n = n + 1;
            if (x >= 1) continue;
            y = y + 1;
        }
        n == 3;"), Ok(BoolVal(true)));
}

#[test]
fn test_interpret_declaration_in_for_body() {
    assert_eq!(interpret("for (;;) var x = 1;"), Err(RuntimeError::new(SourceLoc::new(1, 10), "parse error: Expect expression.")));
}

#[test]
fn test_interpret_native_function_call() {
    assert_eq!(interpret("
        var t = clock();
        t > 0;"), Ok(BoolVal(true)));
}

#[test]
fn test_interpret_function_call() {
    assert_eq!(interpret("
        fun succ(x) {
            return x + 1;
        }
        succ(1);"), Ok(NumberVal(2.0)));
}

#[test]
fn test_interpret_no_implicit_return_value() {
    assert_eq!(interpret("
        fun do(x) {
            x + 1;
        }
        do(1);"), Ok(NilVal));
}

#[test]
fn test_interpret_function_expression() {
    assert_eq!(interpret("
        var succ = fun(x) {
            return x + 1;
        };
        succ(1);"), Ok(NumberVal(2.0)));
}

#[test]
fn test_interpret_duplicate_variable_names() {
    assert_eq!(interpret("
        {
            var x = 1;
            var x = 2;
        }"), Err(RuntimeError::new(SourceLoc::new(4, 17), "parse error: Variable with this name already declared in this scope.")));
}

#[test]
fn test_interpret_duplicate_function_parameters() {
    assert_eq!(interpret("
        fun foo(x, x) {
            return x;
        }"), Err(RuntimeError::new(SourceLoc::new(2, 20), "parse error: Variable with this name already declared in this scope.")));
}

#[test]
fn test_interpret_max_function_parameters() {
    let mut source = "fun f(".to_string();
    for i in 1..=256 {
        source += &format!("x{},\n", i);
    }
    source += ") {}";
    assert_eq!(interpret(&source), Err(RuntimeError::new(SourceLoc::new(256, 1), "parse error: Cannot have more than 255 parameters.")));
}

#[test]
fn test_interpret_max_call_arguments() {
    let mut source = "f(".to_string();
    for i in 1..=256 {
        source += &format!("{},\n", i);
    }
    source += ");";
    assert_eq!(interpret(&source), Err(RuntimeError::new(SourceLoc::new(256, 1), "parse error: Cannot have more than 255 arguments.")));
}

#[test]
fn test_interpret_max_local_variables() {
    let mut source = "fun f() {\n".to_string();
    for i in 1..=256 {
        source += &format!("var x{};\n", i);
    }
    source += "}";
    assert_eq!(interpret(&source), Err(RuntimeError::new(SourceLoc::new(257, 5), "parse error: Too many local variables in function.")));
}

#[test]
fn test_interpret_redefine_global_variable() {
    assert_eq!(interpret("
        var x = 1;
        var x = 2;
        x;"), Ok(NumberVal(2.0)));
}

#[test]
fn test_interpret_scope_resolved() {
    assert_eq!(interpret("
        var x = \"global\";
        {
            fun show() {
                return x;
            }
            show();
            var x = \"local\";
            show();
        }"), Ok(StringVal(Rc::new("global".to_string()))));
}

#[test]
fn test_interpret_use_global_variable_when_not_resolvable() {
    // See https://www.craftinginterpreters.com/global-variables.html
    assert_eq!(interpret("
        fun showVariable() {
            return global;
        }

        var global = \"after\";
        showVariable();
        "), Ok(StringVal(Rc::new("after".to_string()))));
}

#[test]
fn test_interpret_use_global_variable_when_not_resolvable_and_not_defined() {
    // See https://www.craftinginterpreters.com/global-variables.html
    assert_eq!(interpret("
        fun showVariable() {
            return global;
        }

        showVariable();
        "), Err(RuntimeError::new(SourceLoc::new(3, 20), "Undefined variable 'global'.")));
}

#[test]
fn test_interpret_assign_to_global_variable_when_not_resolvable() {
    // See https://www.craftinginterpreters.com/global-variables.html
    assert_eq!(interpret("
        fun changeVariable() {
            global = \"changed\";
        }

        var global = \"original\";
        changeVariable();
        global;
        "), Ok(StringVal(Rc::new("changed".to_string()))));
}

#[test]
fn test_interpret_assign_other_global_variable_when_not_resolvable_is_forward_declared() {
    assert_eq!(interpret("
        fun showVariable() {
            return global;
        }

        var otherGlobal = 1;
        var global = \"after\";
        showVariable();
        "), Ok(StringVal(Rc::new("after".to_string()))));
}

#[test]
fn test_interpret_top_level_return() {
    assert_eq!(interpret("1 + 2;\nreturn;"), Err(RuntimeError::new(SourceLoc::new(2, 1), "parse error: Cannot return from top-level code.")));
}

#[test]
fn test_instance_fields_set_get() {
    assert_eq!(interpret("
        class Point {
        }
        var p = Point();
        p.x = 1;
        p.y = 2;
        p.x + p.y;"), Ok(NumberVal(3.0)));
}

#[test]
fn test_instance_set_evaluation_order() {
    assert_eq!(interpret("p.x = bar;"), Err(RuntimeError::new(SourceLoc::new(1, 1), "Undefined variable 'p'.")));
}

#[test]
fn test_instance_get_undefined_field() {
    assert_eq!(interpret("
        class Point {
        }
        var p = Point();
        p.x;"), Err(RuntimeError::new(SourceLoc::new(5, 10), "Undefined property 'x'.")));
}

#[test]
fn test_instance_method_call() {
    assert_eq!(interpret("
        class Computer {
            answer() {
                return 42;
            }
        }
        var c = Computer();
        c.answer();"), Ok(NumberVal(42.0)));
}

#[test]
fn test_instance_access_field_with_this() {
    assert_eq!(interpret("
        class Box {
            result() {
                return this.value;
            }
        }
        var b = Box();
        b.value = 42;
        b.result();"), Ok(NumberVal(42.0)));
}

#[test]
fn test_this_outside_method_body() {
    assert_eq!(interpret("print this;"), Err(RuntimeError::new(SourceLoc::new(1, 7), "parse error: Cannot use 'this' outside of a class.")));
    assert_eq!(interpret("fun foo() { return this; }"), Err(RuntimeError::new(SourceLoc::new(1, 20), "parse error: Cannot use 'this' outside of a class.")));
}

#[test]
fn test_class_constructor() {
    assert_eq!(interpret("
        class Point {
            init() {
                this.x = 0;
                this.y = 2;
            }
        }
        var p = Point();
        p.y;"), Ok(NumberVal(2.0)));
}

#[test]
fn test_class_constructor_with_one_parameter() {
    assert_eq!(interpret("
        class Box {
            init(value) {
                this.value = value;
            }
        }
        var box = Box(42);
        box.value;"), Ok(NumberVal(42.0)));
}

#[test]
fn test_class_constructor_with_parameters() {
    assert_eq!(interpret("
        class Point {
            init(x, y) {
                this.x = x;
                this.y = y;
            }
        }
        var p = Point(5, 10);
        p.x;"), Ok(NumberVal(5.0)));
}

#[test]
fn test_class_constructor_checks_zero_arity_when_no_init_defined() {
    assert_eq!(interpret("
        class Box {
        }
        Box(1000000);"), Err(RuntimeError::new(SourceLoc::new(4, 12), "Expected 0 arguments but got 1.")));
}

#[test]
fn test_class_constructor_checks_non_zero_arity() {
    assert_eq!(interpret("
        class Point {
            init(x, y) {
                this.x = x;
                this.y = y;
            }
        }
        Point(5, 10, 1000000);"), Err(RuntimeError::new(SourceLoc::new(8, 14), "Expected 2 arguments but got 3.")));
}

#[test]
fn test_class_constructor_disallows_return_with_expression() {
    assert_eq!(interpret("
        class Box {
            init() {
                return 42;
            }
        }"), Err(RuntimeError::new(SourceLoc::new(4, 17), "parse error: Cannot return a value from an initializer.")));
}

#[test]
fn test_class_constructor_allows_return_with_no_expression() {
    assert_eq!(interpret("
        class Box {
            init() {
                return;
            }
        }
        nil;"), Ok(NilVal));
}

#[test]
fn test_can_use_this_in_function_nested_in_method() {
    assert_eq!(interpret("
        class Box {
            foo() {
                fun nested() {
                    print this;
                    return 1;
                }
                return nested();
            }
        }
        Box().foo();"), Ok(NumberVal(1.0)));
}

#[test]
fn test_can_return_value_from_function_nested_in_class_initializer() {
    assert_eq!(interpret("
        class Box {
            init() {
                fun nested() {
                    return 1.0;
                }
                this.value = nested();
            }
        }
        Box().value;"), Ok(NumberVal(1.0)));
}

#[test]
fn test_calling_initializer_returns_this() {
    assert_eq!(interpret("
        class Box {
            init() {
            }
        }
        var x = Box();
        x == x.init();"), Ok(BoolVal(true)));
}

#[test]
fn test_early_return_in_initializer_still_returns_this() {
    assert_eq!(interpret("
        class Box {
            init() {
                return;
                this.x = 7;
            }
        }
        var x = Box();
        x == x.init();"), Ok(BoolVal(true)));
}

#[test]
fn test_static_class_method() {
    assert_eq!(interpret("
        class Math {
            class square(x) {
                return x * x;
            }
        }
        Math.square(2);"), Ok(NumberVal(4.0)));
}

#[test]
fn test_interpret_property_set_on_class() {
    assert_eq!(interpret("
        class Math {}
        Math.pi = 3.14159;
        Math.pi == 3.14159;"), Ok(BoolVal(true)));
}

#[test]
fn test_static_class_method_can_access_this() {
    assert_eq!(interpret("
        class Math {
            class square(x) {
                return this.impl(x);
            }
            class impl(x) {
                return x * x;
            }
        }
        Math.square(2);"), Ok(NumberVal(4.0)));
}

#[test]
fn test_static_class_method_can_access_super() {
    assert_eq!(interpret("
        class Impl {
            class impl(x) {
                return x * x;
            }
        }
        class Math < Impl {
            class square(x) {
                return super.impl(x);
            }
        }
        Math.square(2);"), Ok(NumberVal(4.0)));
}

#[test]
fn test_static_class_method_is_not_accessible_from_instance() {
    assert_eq!(interpret("
        class Math {
            class square(x) {
                return x * x;
            }
        }
        var m = Math();
        m.square(2);"), Err(RuntimeError::new(SourceLoc::new(8, 10), "Undefined property 'square'.")));
}


#[test]
fn test_class_with_superclass_can_call_super_methods() {
    assert_eq!(interpret("
        class Point2 {
            first() {
                return this.x;
            }
        }
        class Point3 < Point2 {
        }
        var p = Point3();
        p.x = 2;
        p.first();"), Ok(NumberVal(2.0)));
}

#[test]
fn test_class_calling_super_from_subclass_method() {
    assert_eq!(interpret("
        class Point2 {
            first() {
                return this.x;
            }
        }
        class Point3 < Point2 {
            second() {
                return super.first() + 1;
            }
        }
        var p = Point3();
        p.x = 2;
        p.second();"), Ok(NumberVal(3.0)));
}

#[test]
fn test_class_inheriting_from_itself() {
    assert_eq!(interpret("
        class Box < Box {}
        "), Err(RuntimeError::new(SourceLoc::new(2, 21), "parse error: A class cannot inherit from itself.")));
}

#[test]
fn test_class_inheriting_from_non_identifier() {
    // In the future, we could make arbitrary expressions work.
    assert_eq!(interpret("
        class Box < 2 {}
        "), Err(RuntimeError::new(SourceLoc::new(2, 21), "parse error: Expect superclass name.")));
}

#[test]
fn test_class_inheriting_from_non_class() {
    assert_eq!(interpret("
        var x = \"not a class\";
        class Box < x {}
        "), Err(RuntimeError::new(SourceLoc::new(3, 15), "Superclass must be a class.")));
}

#[test]
fn test_super_outside_method_body() {
    assert_eq!(interpret("super.x;"), Err(RuntimeError::new(SourceLoc::new(1, 1), "parse error: Cannot use 'super' outside of a class.")));
    assert_eq!(interpret("fun foo() { super.x; }"), Err(RuntimeError::new(SourceLoc::new(1, 13), "parse error: Cannot use 'super' outside of a class.")));
    assert_eq!(interpret("
        class Box {}
        fun foo() { super.x; }
        "), Err(RuntimeError::new(SourceLoc::new(3, 21), "parse error: Cannot use 'super' outside of a class.")));
}

#[test]
fn test_super_outside_subclass_method() {
    assert_eq!(interpret("
        class Box {
            foo() {
                super.x;
            }
        }"), Err(RuntimeError::new(SourceLoc::new(4, 17), "parse error: Cannot use 'super' in a class with no superclass.")));
}
