use crate::ast::{Expression, ExpressionStatement, Program, Statement};
use crate::object::{BooleanObject, IntegerObject, NullObject, Object, ObjectType};

const CONST_TRUE: BooleanObject = BooleanObject { value: true };
const CONST_FALSE: BooleanObject = BooleanObject { value: false };
const CONST_NULL: NullObject = NullObject {};

const TRUE_OBJ: Object = Object::Boolean(CONST_TRUE);
const FALSE_OBJ: Object = Object::Boolean(CONST_FALSE);
const NULL_OBJ: Object = Object::Null(CONST_NULL);

pub fn eval(program: Program) -> Option<Object> {
    let mut o: Object = Object::Integer(IntegerObject { value: 0 });

    for s in program.statements {
        match s {
            Statement::Expression(es) => o = eval_expression_statement(es),
            _ => return None,
        }
    }

    Some(o)
}

fn eval_expression_statement(es: ExpressionStatement) -> Object {
    eval_expression(es.expression.unwrap())
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
        _ => todo!(),
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

        _ => return NULL_OBJ,
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

    fn test_integer_object(obj: &Object, expected: i64) {
        match obj {
            Object::Integer(io) => {
                assert_eq!(
                    io.value, expected,
                    "object has wrong value, got={}, want={}",
                    io.value, expected
                );
            }
            _ => panic!("object is not Integer, got={:?}", obj),
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

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            match evaluated {
                Some(e) => test_integer_object(&e, expected),
                None => panic!("eval returned None for input: {}", input),
            }
        }
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
}
