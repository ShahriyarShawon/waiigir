use crate::ast::{Expression, ExpressionStatement, Program, Statement};
use crate::object::{BooleanObject, IntegerObject, NullObject, Object};

const CONST_TRUE: BooleanObject = BooleanObject { value: true };
const CONST_FALSE: BooleanObject = BooleanObject { value: false };
const CONST_NULL: NullObject = NullObject {};

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
    match es.expression.unwrap() {
        Expression::Integer(il) => {
            let io = IntegerObject { value: il.value };
            Object::Integer(io)
        }
        Expression::Boolean(be) => {
            let v = if be.value { CONST_TRUE } else { CONST_FALSE };
            Object::Boolean(v)
        }
        _ => todo!(),
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

    fn test_integer_object(obj: Object, expected: i64) -> bool {
        let result = match obj {
            Object::Integer(io) => io,
            _ => {
                eprintln!("object is not Integer. got={:?}", obj);
                return false;
            }
        };

        if result.value != expected {
            eprintln!(
                "object has wrong value. got={}, want={}",
                result.value, expected
            );
            return false;
        }
        return true;
    }

    fn test_boolean_object(obj: Option<Object>, expected: bool) -> bool {
        match obj {
            Some(Object::Boolean(b)) => {
                if b.value != expected {
                    eprintln!("object has wrong value, got={}, want={}", b.value, expected);
                    return false;
                }
                true
            }
            _ => {
                eprintln!("object is not Boolean, got={:?}", obj);
                false
            }
        }
    }

    #[test]
    fn test_eval_integer_expressions() {
        let tests = vec![("5", 5), ("10", 10)];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            match evaluated {
                Some(e) => test_integer_object(e, expected),
                None => panic!("DID NOT GET BACK EVALUATED INTEGER EXPRESSION"),
            };
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests = vec![("true", true), ("false", false)];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_boolean_object(evaluated, expected);
        }
    }
}
