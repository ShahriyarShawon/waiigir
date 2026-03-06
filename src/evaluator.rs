use crate::ast::{Expression, IfExpression, Program, Statement};
use crate::object::{BooleanObject, IntegerObject, NullObject, Object};

const CONST_TRUE: BooleanObject = BooleanObject { value: true };
const CONST_FALSE: BooleanObject = BooleanObject { value: false };
const CONST_NULL: NullObject = NullObject {};

const TRUE_OBJ: Object = Object::Boolean(CONST_TRUE);
const FALSE_OBJ: Object = Object::Boolean(CONST_FALSE);
const NULL_OBJ: Object = Object::Null(CONST_NULL);

pub fn eval(program: Program) -> Option<Object> {
    Some(eval_statements(program.statements))
}

fn eval_statements(statements: Vec<Statement>) -> Object {
    let mut o: Object = Object::Integer(IntegerObject { value: 0 });

    for s in statements {
        match s {
            Statement::Expression(es) => o = eval_expression(es.expression.unwrap()),
            Statement::Block(bs) => o = eval_statements(bs.statements),
            _ => return NULL_OBJ
        }
    }

    o
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
            eval_prefix_expression(&pe.operator, right)
        }
        Expression::Infix(ie) => {
            let left = eval_expression(*ie.left);
            let right = eval_expression(*ie.right);
            eval_infix_expression(&ie.operator, left, right)
        }
        Expression::If(ie) => {
            eval_if_expression(ie)
        }
        _ => NULL_OBJ,
    }
}

fn is_truthy(o: Object) -> bool {
    match o {
        Object::Null(_) => false,
        Object::Boolean(bo) => {
            bo.value
        },
        _ => true
    }
}

fn eval_if_expression(ie: IfExpression) -> Object {
    let condition = eval_expression(*ie.condition);
    if is_truthy(condition) {
        eval_statements(ie.consequence.statements)
    } else if let Some(a) = ie.alternative {
        eval_statements(a.statements)
    } else {
        NULL_OBJ
    }
}

fn eval_infix_expression(operator: &str, left: Object, right: Object) -> Object {
    match (left, right, operator) {
        (Object::Integer(lio), Object::Integer(rio), _) => {
            eval_integer_infix_expression(operator, lio.value, rio.value)
        }
        (Object::Boolean(lio), Object::Boolean(rio), "==") => Object::Boolean(BooleanObject {
            value: lio.value == rio.value,
        }),
        (Object::Boolean(lio), Object::Boolean(rio), "!=") => Object::Boolean(BooleanObject {
            value: lio.value != rio.value,
        }),
        _ => NULL_OBJ,
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

        _ => NULL_OBJ,
    }
}
fn eval_prefix_expression(operator: &str, right: Object) -> Object {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator(right),
        _ => Object::Null(CONST_NULL),
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
        _ => NULL_OBJ,
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
}
