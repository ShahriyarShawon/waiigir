use crate::ast::{Expression, IfExpression, Program, Statement};
use crate::object::{BooleanObject, IntegerObject, NullObject, Object, ReturnValue};

const CONST_TRUE: BooleanObject = BooleanObject { value: true };
const CONST_FALSE: BooleanObject = BooleanObject { value: false };
const CONST_NULL: NullObject = NullObject {};

const TRUE_OBJ: Object = Object::Boolean(CONST_TRUE);
const FALSE_OBJ: Object = Object::Boolean(CONST_FALSE);
const NULL_OBJ: Object = Object::Null(CONST_NULL);

fn is_error(obj: &Object) -> bool {
    matches!(obj, Object::Error(_))
}

pub fn eval(program: Program) -> Option<Object> {
    let mut result: Object = Object::Integer(IntegerObject { value: 0 });

    for s in program.statements {
        result = eval_statement(s);
        match result {
            Object::Return(rs) => return Some(*rs.value),
            Object::Error(eo) => return Some(Object::Error(eo)),
            _ => {}
        }
    }

    Some(result)
}

fn eval_statement(statement: Statement) -> Object {
    let result = match statement {
        Statement::Expression(es) => eval_expression(es.expression.unwrap()),
        Statement::Block(bs) => eval_block_statement(Statement::Block(bs)),
        Statement::Return(rs) => {
            let val = eval_expression(rs.return_value.expect("return value missing"));
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

fn eval_block_statement(statement: Statement) -> Object {
    let mut result: Object = Object::Integer(IntegerObject { value: 0 });
    if let Statement::Block(bs) = statement {
        for s in bs.statements {
            result = eval_statement(s);
            if let Object::Return(_) | Object::Error(_) = &result {
                return result;
            }
        }
    }

    result
}

fn eval_expression(e: Expression) -> Object {
    match e {
        Expression::Integer(il) => {
            let io = IntegerObject { value: il.value };
            Object::Integer(io)
        }
        Expression::Boolean(be) => match be.value {
            true => TRUE_OBJ,
            false => FALSE_OBJ,
        },
        Expression::Prefix(pe) => {
            let right = eval_expression(*pe.right);
            if is_error(&right) {
                return right;
            }
            eval_prefix_expression(&pe.operator, right)
        }
        Expression::Infix(ie) => {
            let left = eval_expression(*ie.left);
            if is_error(&left) {
                return left;
            }

            let right = eval_expression(*ie.right);
            if is_error(&right) {
                return right;
            }
            eval_infix_expression(&ie.operator, &left, &right)
        }
        Expression::If(ie) => eval_if_expression(ie),
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

fn eval_if_expression(ie: IfExpression) -> Object {
    let condition = eval_expression(*ie.condition);
    if is_error(&condition) {
        return condition;
    }

    if is_truthy(condition) {
        eval_block_statement(Statement::Block(*ie.consequence))
    } else if let Some(a) = ie.alternative {
        eval_block_statement(Statement::Block(*a))
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
#[cfg(test)]
mod tests {
    use crate::evaluator::eval;
    use crate::lexer::Lexer;
    use crate::object::Object;
    use crate::parser::Parser;

    fn test_eval(input: &str) -> Option<Object> {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        eval(program)
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
}
