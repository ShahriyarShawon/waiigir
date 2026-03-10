use crate::ast::{
    BlockStatement, Expression, IdentifierExpression, IfExpression, Program, Statement,
};
use crate::environment::Environment;
use crate::object::{
    BooleanObject, FunctionObject, IntegerObject, NullObject, Object, ReturnValue, StringObject,
};
use crate::builtins::get_builtin;

const CONST_TRUE: BooleanObject = BooleanObject { value: true };
const CONST_FALSE: BooleanObject = BooleanObject { value: false };
const CONST_NULL: NullObject = NullObject {};

const TRUE_OBJ: Object = Object::Boolean(CONST_TRUE);
const FALSE_OBJ: Object = Object::Boolean(CONST_FALSE);
const NULL_OBJ: Object = Object::Null(CONST_NULL);

fn is_error(obj: &Object) -> bool {
    matches!(obj, Object::Error(_))
}

pub fn eval(program: Program, env: &mut Environment) -> Option<Object> {
    let mut result: Object = NULL_OBJ;

    for s in program.statements {
        result = eval_statement(s, env);
        match result {
            Object::Return(rs) => return Some(*rs.value),
            Object::Error(eo) => return Some(Object::Error(eo)),
            _ => {}
        }
    }

    Some(result)
}

fn eval_statement(statement: Statement, env: &mut Environment) -> Object {
    let result = match statement {
        Statement::Let(ls) => {
            let val = eval_expression(ls.value.unwrap(), env);
            if is_error(&val) {
                return val;
            }

            env.set(&ls.name.value, val);
            match env.get(&ls.name.value) {
                Some(v) => v,
                None => Object::new_error(format!(
                    "Supposedely added ikdentifier to environment, but retrieving failed: {}",
                    ls.name.value
                )),
            }
        }
        Statement::Expression(es) => eval_expression(es.expression.unwrap(), env),
        Statement::Block(bs) => eval_block_statement(bs, env),
        Statement::Return(rs) => {
            let val = eval_expression(rs.return_value.expect("return value missing"), env);
            if is_error(&val) {
                return val;
            }
            return Object::Return(ReturnValue {
                value: Box::new(val),
            });
        }
        _ => return NULL_OBJ,
    };

    result
}

fn eval_block_statement(block: BlockStatement, env: &mut Environment) -> Object {
    let mut result: Object = NULL_OBJ;
    for s in block.statements {
        result = eval_statement(s, env);
        if let Object::Return(_) | Object::Error(_) = &result {
            return result;
        }
    }

    result
}

fn eval_identifier(ie: IdentifierExpression, env: &mut Environment) -> Object {
    if let Some(v) = env.get(&ie.value) {
        return v
    }

    let func = get_builtin(&ie.value);
    match func {
        Some(bif) => bif,
        None => Object::new_error(format!("identifier not found: {}", ie.value))
    }
}

fn eval_expressions(exps: Vec<Expression>, env: &mut Environment) -> Vec<Object> {
    let mut results: Vec<Object> = Vec::new();
    for e in exps {
        let evaluated = eval_expression(e, env);
        if is_error(&evaluated) {
            return vec![evaluated];
        }
        results.push(evaluated);
    }

    results
}

fn apply_function(func: Object, args: Vec<Object>) -> Object {
    match func {
        Object::Function(fo) => {
            let mut extended_env = extend_function_env(&fo, args);
            let evaluated = eval_block_statement(fo.body, &mut extended_env);
            return unwrap_return_value(evaluated);
        },
        Object::BuiltInFunction(bif) => {
            return (bif.function)(args)
        },
        _ => Object::new_error(format!("not a function: {:?}", func)),
    }
}

fn extend_function_env(func: &FunctionObject, args: Vec<Object>) -> Environment {
    let mut env = Environment::new_enclosed_environment(Box::new(func.env.clone()));
    for (index, ident) in func.parameters.iter().enumerate() {
        env.set(&ident.value, args[index].clone());
    }

    env
}

fn unwrap_return_value(obj: Object) -> Object {
    match obj {
        Object::Return(ro) => *ro.value,
        _ => obj,
    }
}

fn eval_expression(e: Expression, env: &mut Environment) -> Object {
    match e {
        Expression::Call(ce) => {
            let function = eval_expression(*ce.function, env);
            if is_error(&function) {
                return function;
            }
            let args = eval_expressions(ce.arguments, env);
            if args.len() == 1 && is_error(&args[0]) {
                return args[0].clone();
            }

            return apply_function(function, args);
        }
        Expression::Function(fl) => {
            let params = fl.parameters;
            let body = fl.body;
            Object::Function(FunctionObject {
                parameters: params,
                body,
                env: env.clone(),
            })
        }
        Expression::Identifier(ie) => eval_identifier(ie, env),
        Expression::Integer(il) => {
            let io = IntegerObject { value: il.value };
            Object::Integer(io)
        }
        Expression::Boolean(be) => match be.value {
            true => TRUE_OBJ,
            false => FALSE_OBJ,
        },
        Expression::Prefix(pe) => {
            let right = eval_expression(*pe.right, env);
            if is_error(&right) {
                return right;
            }
            eval_prefix_expression(&pe.operator, right)
        }
        Expression::Infix(ie) => {
            let left = eval_expression(*ie.left, env);
            if is_error(&left) {
                return left;
            }

            let right = eval_expression(*ie.right, env);
            if is_error(&right) {
                return right;
            }
            eval_infix_expression(&ie.operator, &left, &right)
        }
        Expression::If(ie) => eval_if_expression(ie, env),
        Expression::String(se) => Object::String(StringObject { value: se.value }),
        _ => NULL_OBJ,
    }
}

fn is_truthy(o: Object) -> bool {
    match o {
        Object::Null(_) => false,
        Object::Boolean(bo) => bo.value,
        _ => true,
    }
}

fn eval_if_expression(ie: IfExpression, env: &mut Environment) -> Object {
    let condition = eval_expression(*ie.condition, env);
    if is_error(&condition) {
        return condition;
    }

    if is_truthy(condition) {
        eval_block_statement(*ie.consequence, env)
    } else if let Some(a) = ie.alternative {
        eval_block_statement(*a, env)
    } else {
        NULL_OBJ
    }
}

fn eval_infix_expression(operator: &str, left: &Object, right: &Object) -> Object {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => {
            eval_integer_infix_expression(operator, l.value, r.value)
        }
        (Object::Boolean(l), Object::Boolean(r)) => match operator {
            "==" => Object::Boolean(BooleanObject {
                value: l.value == r.value,
            }),
            "!=" => Object::Boolean(BooleanObject {
                value: l.value != r.value,
            }),
            _ => Object::new_error(format!(
                "unknown operator: {} {} {}",
                left.type_name(),
                operator,
                right.type_name()
            )),
        },
        (Object::String(l), Object::String(r)) => eval_string_infix_expression(operator, l, r),
        _ => Object::new_error(format!(
            "type mismatch: {} {} {}",
            left.type_name(),
            operator,
            right.type_name()
        )),
    }
}

fn eval_integer_infix_expression(operator: &str, left: i64, right: i64) -> Object {
    match operator {
        "+" => Object::Integer(IntegerObject {
            value: left + right,
        }),
        "-" => Object::Integer(IntegerObject {
            value: left - right,
        }),
        "*" => Object::Integer(IntegerObject {
            value: left * right,
        }),
        "/" => Object::Integer(IntegerObject {
            value: left / right,
        }),

        ">" => Object::Boolean(BooleanObject {
            value: left > right,
        }),
        "<" => Object::Boolean(BooleanObject {
            value: left < right,
        }),
        "==" => Object::Boolean(BooleanObject {
            value: left == right,
        }),
        "!=" => Object::Boolean(BooleanObject {
            value: left != right,
        }),

        _ => Object::new_error(format!("unknown operator: int {} int", operator)),
    }
}
fn eval_prefix_expression(operator: &str, right: Object) -> Object {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator(right),
        _ => Object::new_error(format!("unknown operator: {}{:?}", operator, right)),
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        Object::Boolean(be) => match be.value {
            true => FALSE_OBJ,
            false => TRUE_OBJ,
        },
        Object::Null(_) => TRUE_OBJ,
        _ => FALSE_OBJ,
    }
}

fn eval_minus_prefix_operator(right: Object) -> Object {
    match right {
        Object::Integer(io) => {
            let value = io.value;
            Object::Integer(IntegerObject { value: -value })
        }
        _ => Object::new_error(format!("unknown operator: -{}", right.type_name())),
    }
}

fn eval_string_infix_expression(
    operator: &str,
    left: &StringObject,
    right: &StringObject,
) -> Object {
    match operator {
        "+" => Object::String(StringObject {
            value: format!("{}{}", left.value, right.value),
        }),

        _ => Object::new_error(format!("unknown operator STRING {} STRING", operator)),
    }
}

#[cfg(test)]
mod tests {
    use crate::environment::Environment;
    use crate::evaluator::eval;
    use crate::lexer::Lexer;
    use crate::object::Object;
    use crate::parser::{Parser, check_parser_errors, ExpectedLiteral};

    fn test_eval(input: &str) -> Option<Object> {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        let mut env = Environment::new();
        eval(program, &mut env)
    }

    fn test_integer_object(
        obj: &Option<Object>,
        expected: i64,
        input: &str,
        failures: &mut Vec<String>,
    ) {
        match obj {
            Some(Object::Integer(io)) => {
                if io.value != expected {
                    failures.push(format!(
                        "input='{}': object has wrong value, got={}, want={}",
                        input, io.value, expected
                    ));
                }
            }
            Some(other) => {
                failures.push(format!(
                    "input='{}': object is not Integer, got={:?}",
                    input, other
                ));
            }
            None => {
                failures.push(format!(
                    "input='{}': object is not Integer, got=<nil>",
                    input
                ));
            }
        }
    }

    fn test_boolean_object(obj: Option<Object>, expected: bool) {
        match obj {
            Some(Object::Boolean(b)) => {
                assert_eq!(
                    b.value, expected,
                    "object has wrong value, got={}, want={}",
                    b.value, expected
                );
            }
            _ => panic!("object is not Boolean, got={:?}", obj),
        }
    }

    #[test]
    fn test_eval_integer_expressions() {
        let tests = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        let mut failures: Vec<String> = Vec::new();

        for (input, expected) in &tests {
            let evaluated = test_eval(input);
            test_integer_object(&evaluated, *expected, input, &mut failures);
        }

        assert!(failures.is_empty(), "\n{}", failures.join("\n"));
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_boolean_object(evaluated, expected);
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests = vec![
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_boolean_object(evaluated, expected);
        }
    }

    fn test_null_object(obj: &Option<Object>, input: &str, failures: &mut Vec<String>) {
        match obj {
            Some(Object::Null(_)) | None => {}
            Some(other) => {
                failures.push(format!(
                    "input='{}': object is not NULL, got={:?}",
                    input, other
                ));
            }
        }
    }
    #[test]
    fn test_if_else_expressions() {
        enum Expected {
            Int(i64),
            Null,
        }

        let tests = vec![
            ("if (true) { 10 }", Expected::Int(10)),
            ("if (false) { 10 }", Expected::Null),
            ("if (1) { 10 }", Expected::Int(10)),
            ("if (1 < 2) { 10 }", Expected::Int(10)),
            ("if (1 > 2) { 10 }", Expected::Null),
            ("if (1 > 2) { 10 } else { 20 }", Expected::Int(20)),
            ("if (1 < 2) { 10 } else { 20 }", Expected::Int(10)),
        ];

        let mut failures: Vec<String> = Vec::new();

        for (input, expected) in &tests {
            let evaluated = test_eval(input);
            match expected {
                Expected::Int(i) => test_integer_object(&evaluated, *i, input, &mut failures),
                Expected::Null => test_null_object(&evaluated, input, &mut failures),
            }
        }

        assert!(failures.is_empty(), "\n{}", failures.join("\n"));
    }

    #[test]
    fn test_return_statements() {
        let tests = vec![
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            ("if (10 > 1) { if (10 > 1) { return 10; } return 1; }", 10),
        ];

        let mut failures: Vec<String> = Vec::new();

        for (input, expected) in &tests {
            let evaluated = test_eval(input);
            test_integer_object(&evaluated, *expected, input, &mut failures);
        }

        assert!(failures.is_empty(), "\n{}", failures.join("\n"));
    }
    #[test]
    fn test_error_handling() {
        let tests = vec![
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                "if (10 > 1) { if (10 > 1) { return true + false; } return 1; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            ("foobar", "identifier not found: foobar"),
            ("\"Hello\" - \"World\"", "unknown operator STRING - STRING"),
        ];

        let mut failures: Vec<String> = Vec::new();

        for (input, expected_message) in &tests {
            let evaluated = test_eval(input);
            match &evaluated {
                Some(Object::Error(err)) => {
                    if &err.message != expected_message {
                        failures.push(format!(
                            "input='{}': wrong error message, expected={}, got={}",
                            input, expected_message, err.message
                        ));
                    }
                }
                Some(other) => {
                    failures.push(format!(
                        "input='{}': no error object returned, got={:?}",
                        input, other
                    ));
                }
                None => {
                    failures.push(format!(
                        "input='{}': no error object returned, got=<nil>",
                        input
                    ));
                }
            }
        }

        assert!(failures.is_empty(), "\n{}", failures.join("\n"));
    }

    #[test]
    fn test_let_statements() {
        let tests = vec![
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        let mut failures: Vec<String> = Vec::new();

        for (input, expected) in &tests {
            let evaluated = test_eval(input);
            test_integer_object(&evaluated, *expected, input, &mut failures);
        }

        assert!(failures.is_empty(), "\n{}", failures.join("\n"));
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; };";
        let mut failures: Vec<String> = Vec::new();

        let evaluated = test_eval(input);

        let function = match &evaluated {
            Some(Object::Function(f)) => f,
            _ => {
                failures.push(format!("object is not Function, got={:?}", evaluated));
                assert!(failures.is_empty(), "\n{}", failures.join("\n"));
                return;
            }
        };

        if function.parameters.len() != 1 {
            failures.push(format!(
                "function has wrong number of parameters, got={}",
                function.parameters.len()
            ));
        }

        if function.parameters.len() >= 1 && function.parameters[0].value != "x" {
            failures.push(format!(
                "parameter is not 'x', got={}",
                function.parameters[0].value
            ));
        }

        let expected_body = "(x + 2)";
        if function.body.to_string() != expected_body {
            failures.push(format!(
                "body is not '{}', got={}",
                expected_body,
                function.body.to_string()
            ));
        }

        assert!(failures.is_empty(), "\n{}", failures.join("\n"));
    }

    #[test]
    fn test_function_application() {
        let tests = vec![
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];

        let mut failures: Vec<String> = Vec::new();

        for (input, expected) in &tests {
            let evaluated = test_eval(input);
            test_integer_object(&evaluated, *expected, input, &mut failures);
        }

        assert!(failures.is_empty(), "\n{}", failures.join("\n"));
    }

    #[test]
    fn test_closures() {
        let input = "
let newAdder = fn(x) {
    fn(y) { x + y };
};
let addTwo = newAdder(2);
addTwo(2);";

        let mut failures: Vec<String> = Vec::new();
        let evaluated = test_eval(input);
        test_integer_object(&evaluated, 4, input, &mut failures);
        assert!(failures.is_empty(), "\n{}", failures.join("\n"));
    }

    #[test]
    fn test_string_literal() {
        let input = "\"Hello World!\"";
        let mut failures: Vec<String> = Vec::new();

        let evaluated = test_eval(input);

        match &evaluated {
            Some(Object::String(s)) => {
                if s.value != "Hello World!" {
                    failures.push(format!("string has wrong value, got={}", s.value));
                }
            }
            _ => failures.push(format!("object is not String, got={:?}", evaluated)),
        }

        assert!(failures.is_empty(), "\n{}", failures.join("\n"));
    }

    #[test]
    fn test_string_concatenation() {
        let input = "\"Hello\" + \" \" + \"World!\"";
        let mut failures: Vec<String> = Vec::new();

        let evaluated = test_eval(input);

        match &evaluated {
            Some(Object::String(s)) => {
                if s.value != "Hello World!" {
                    failures.push(format!("string has wrong value, got={}", s.value));
                }
            }
            _ => failures.push(format!("object is not String, got={:?}", evaluated)),
        }

        assert!(failures.is_empty(), "\n{}", failures.join("\n"));
    }

    #[test]
    fn test_builtin_functions() {
        let tests = vec![
            (r#"len("")"#, ExpectedLiteral::Int(0)),
            (r#"len("four")"#, ExpectedLiteral::Int(4)),
            (r#"len("hello world")"#, ExpectedLiteral::Int(11)),
            (
                r#"len(1)"#,
                ExpectedLiteral::Str("argument to `len` not supported, got INTEGER".to_string()),
            ),
            (
                r#"len("one", "two")"#,
                ExpectedLiteral::Str("wrong number of arguments. got=2, want=1".to_string()),
            ),
        ];

        let mut failures: Vec<String> = Vec::new();

        for (input, expected) in &tests {
            let evaluated = test_eval(input);
            match expected {
                ExpectedLiteral::Int(i) => {
                    test_integer_object(&evaluated, *i, input, &mut failures);
                }
                ExpectedLiteral::Str(s) => match &evaluated {
                    Some(Object::Error(err)) => {
                        if &err.message != s {
                            failures.push(format!(
                                "input='{}': wrong error message, expected={}, got={}",
                                input, s, err.message
                            ));
                        }
                    }
                    Some(other) => {
                        failures.push(format!(
                            "input='{}': object is not Error, got={:?}",
                            input, other
                        ));
                    }
                    None => {
                        failures.push(format!("input='{}': object is not Error, got=<nil>", input));
                    }
                },
                _ => {}
            }
        }

        assert!(failures.is_empty(), "\n{}", failures.join("\n"));
    }
}
