use crate::ast::{
    BlockStatement, Expression, HashLiteral, IdentifierExpression, IfExpression, Program, Statement,
};
use crate::builtins::get_builtin;
use crate::environment::Environment;
use crate::object::{FunctionObject, HashKey, HashPair, Object};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Evaluator {
    pub env: Rc<RefCell<Environment>>,
}

impl Evaluator {
    pub const TRUE_OBJ: Object = Object::Boolean(true);
    pub const FALSE_OBJ: Object = Object::Boolean(false);
    pub const NULL_OBJ: Object = Object::Null;

    pub fn new() -> Self {
        Self {
            env: Environment::new(),
        }
    }

    fn is_truthy(o: Object) -> bool {
        match o {
            Object::Null => false,
            Object::Boolean(bo) => bo,
            _ => true,
        }
    }

    fn is_error(obj: &Object) -> bool {
        matches!(obj, Object::Error(_))
    }

    fn unwrap_return_value(obj: Object) -> Object {
        match obj {
            Object::Return(ro) => *ro,
            _ => obj,
        }
    }

    pub fn eval(&mut self, program: Program) -> Option<Object> {
        let mut result: Object = Self::NULL_OBJ;
        for s in program.statements {
            result = self.eval_statement(s);
            match result {
                Object::Return(rs) => return Some(*rs),
                Object::Error(eo) => return Some(Object::Error(eo)),
                _ => {}
            }
        }

        Some(result)
    }

    fn eval_statement(&mut self, statement: Statement) -> Object {
        match statement {
            Statement::Let(ls) => {
                let val = self.eval_expression(ls.value.unwrap());
                if Self::is_error(&val) {
                    return val;
                }

                self.env.borrow_mut().set(&ls.name.value, val.clone());
                Self::NULL_OBJ
            }
            Statement::Expression(es) => self.eval_expression(es.expression.unwrap()),
            Statement::Return(rs) => {
                let val = self.eval_expression(rs.return_value.expect("return value missing"));
                if Self::is_error(&val) {
                    return val;
                }
                Object::Return(Box::new(val))
            }
        }
    }

    fn eval_block_statement(&mut self, block: BlockStatement) -> Object {
        let mut result: Object = Self::NULL_OBJ;
        for s in block.statements {
            result = self.eval_statement(s);
            if let Object::Return(_) | Object::Error(_) = &result {
                return result;
            }
        }

        result
    }

    fn eval_identifier(&mut self, ie: IdentifierExpression) -> Object {
        if let Some(v) = self.env.borrow().get(&ie.value) {
            return v;
        }

        let func = get_builtin(&ie.value);
        match func {
            Some(bif) => bif,
            None => Object::new_error(format!("identifier not found: {}", ie.value)),
        }
    }

    fn eval_expressions(&mut self, exps: Vec<Expression>) -> Vec<Object> {
        let mut results: Vec<Object> = Vec::new();
        for e in exps {
            let evaluated = self.eval_expression(e);
            if Self::is_error(&evaluated) {
                return vec![evaluated];
            }
            results.push(evaluated);
        }

        results
    }

    fn apply_function(&mut self, func: Object, args: Vec<Object>) -> Object {
        match func {
            Object::Function(fo) => {
                let old_env = self.env.clone();
                self.env = Environment::new_enclosed_environment(&fo.env);
                for (ident, arg) in fo.parameters.iter().zip(args) {
                    self.env.borrow_mut().set(&ident.value, arg)
                }

                let result = self.eval_block_statement(fo.body);
                self.env = old_env;
                Self::unwrap_return_value(result)
            }
            Object::BuiltInFunction(bif) => bif(args),
            _ => Object::new_error(format!("not a function: {:?}", func)),
        }
    }

    // might need this lowkey later
    // fn extend_function_env(
    //     &mut self,
    //     func: &FunctionObject,
    //     args: Vec<Object>,
    // ) -> Rc<RefCell<Environment>> {
    //     let env = Environment::new_enclosed_environment(&func.env);
    //     for (index, ident) in func.parameters.iter().enumerate() {
    //         env.borrow_mut().set(&ident.value, args[index].clone());
    //     }
    //
    //     env
    // }

    fn eval_expression(&mut self, e: Expression) -> Object {
        match e {
            Expression::Call(ce) => {
                let function = self.eval_expression(*ce.function);
                if Self::is_error(&function) {
                    return function;
                }
                let args = self.eval_expressions(ce.arguments);
                if args.len() == 1 && Self::is_error(&args[0]) {
                    return args[0].clone();
                }

                self.apply_function(function, args)
            }
            Expression::Function(fl) => {
                let params = fl.parameters;
                let body = fl.body;
                Object::Function(FunctionObject {
                    parameters: params,
                    body,
                    env: self.env.clone(),
                })
            }
            Expression::Identifier(ie) => self.eval_identifier(ie),
            Expression::Integer(il) => Object::Integer(il.value),
            Expression::Boolean(be) => match be.value {
                true => Self::TRUE_OBJ,
                false => Self::FALSE_OBJ,
            },
            Expression::Prefix(pe) => {
                let right = self.eval_expression(*pe.right);
                if Self::is_error(&right) {
                    return right;
                }
                self.eval_prefix_expression(&pe.operator, right)
            }
            Expression::Infix(ie) => {
                let left = self.eval_expression(*ie.left);
                if Self::is_error(&left) {
                    return left;
                }

                let right = self.eval_expression(*ie.right);
                if Self::is_error(&right) {
                    return right;
                }
                self.eval_infix_expression(&ie.operator, &left, &right)
            }
            Expression::If(ie) => self.eval_if_expression(ie),
            Expression::String(se) => Object::String(se.value),
            Expression::Array(al) => {
                let elements = self.eval_expressions(al.elements);
                if elements.len() == 1 && Self::is_error(&elements[0]) {
                    return elements[0].clone();
                }

                Object::Array(elements)
            }
            Expression::Index(ie) => {
                let left = self.eval_expression(*ie.left);
                if Self::is_error(&left) {
                    return left;
                }
                let index = self.eval_expression(*ie.index);
                if Self::is_error(&index) {
                    return index;
                }

                self.eval_index_expression(left, index)
            }
            Expression::Hash(hl) => self.eval_hash_literal(hl),
            _ => Self::NULL_OBJ,
        }
    }

    fn eval_if_expression(&mut self, ie: IfExpression) -> Object {
        let condition = self.eval_expression(*ie.condition);
        if Self::is_error(&condition) {
            return condition;
        }

        if Self::is_truthy(condition) {
            self.eval_block_statement(*ie.consequence)
        } else if let Some(a) = ie.alternative {
            self.eval_block_statement(*a)
        } else {
            Self::NULL_OBJ
        }
    }

    fn eval_infix_expression(&mut self, operator: &str, left: &Object, right: &Object) -> Object {
        match (left, right) {
            (Object::Integer(l), Object::Integer(r)) => {
                self.eval_integer_infix_expression(operator, *l, *r)
            }
            (Object::Boolean(l), Object::Boolean(r)) => match operator {
                "==" => Object::Boolean(l == r),
                "!=" => Object::Boolean(l != r),
                _ => Object::new_error(format!(
                    "unknown operator: {} {} {}",
                    left.type_name(),
                    operator,
                    right.type_name()
                )),
            },
            (Object::String(l), Object::String(r)) => {
                self.eval_string_infix_expression(operator, l, r)
            }
            _ => Object::new_error(format!(
                "type mismatch: {} {} {}",
                left.type_name(),
                operator,
                right.type_name()
            )),
        }
    }

    fn eval_integer_infix_expression(&mut self, operator: &str, left: i64, right: i64) -> Object {
        match operator {
            "+" => Object::Integer(left + right),
            "-" => Object::Integer(left - right),
            "*" => Object::Integer(left * right),
            "/" => Object::Integer(left / right),

            ">" => Object::Boolean(left > right),
            "<" => Object::Boolean(left < right),
            "==" => Object::Boolean(left == right),
            "!=" => Object::Boolean(left != right),

            _ => Object::new_error(format!("unknown operator: int {} int", operator)),
        }
    }

    fn eval_prefix_expression(&mut self, operator: &str, right: Object) -> Object {
        match operator {
            "!" => self.eval_bang_operator_expression(right),
            "-" => self.eval_minus_prefix_operator(right),
            _ => Object::new_error(format!("unknown operator: {}{:?}", operator, right)),
        }
    }

    fn eval_bang_operator_expression(&mut self, right: Object) -> Object {
        match right {
            Object::Boolean(true) => Self::FALSE_OBJ,
            Object::Boolean(false) => Self::TRUE_OBJ,
            Object::Null => Self::TRUE_OBJ,
            _ => Self::FALSE_OBJ,
        }
    }

    fn eval_minus_prefix_operator(&mut self, right: Object) -> Object {
        match right {
            Object::Integer(io) => Object::Integer(-io),
            _ => Object::new_error(format!("unknown operator: -{}", right.type_name())),
        }
    }

    fn eval_string_infix_expression(&mut self, operator: &str, left: &str, right: &str) -> Object {
        match operator {
            "+" => Object::String(format!("{}{}", left, right)),

            _ => Object::new_error(format!("unknown operator STRING {} STRING", operator)),
        }
    }

    fn eval_index_expression(&mut self, left: Object, index: Object) -> Object {
        match (&left, &index) {
            (Object::Array(ao), Object::Integer(io)) => self.eval_array_index_expression(ao, *io),
            (Object::Hash(ho), _) => self.eval_hash_index_expression(ho, index),
            _ => Object::new_error(format!(
                "index operator not supported: {}",
                left.type_name()
            )),
        }
    }

    fn eval_hash_literal(&mut self, hl: HashLiteral) -> Object {
        let mut pairs: HashMap<HashKey, HashPair> = HashMap::new();

        for (key_exp, value_exp) in hl.pairs {
            let key = self.eval_expression(key_exp);
            if Self::is_error(&key) {
                return key;
            }

            let hashed = match key.hash_key() {
                Some(h) => h,
                None => {
                    return Object::new_error(format!(
                        "unusable as hash key: {:?}",
                        key.type_name()
                    ));
                }
            };

            let val = self.eval_expression(value_exp);
            if Self::is_error(&val) {
                return val;
            }

            pairs.insert(hashed, HashPair { key, value: val });
        }

        Object::Hash(pairs)
    }

    fn eval_array_index_expression(&mut self, arr: &[Object], index: i64) -> Object {
        let idx = index;

        if arr.is_empty() || idx < 0 || idx as usize >= arr.len() {
            return Self::NULL_OBJ;
        }

        arr[idx as usize].clone()
    }

    fn eval_hash_index_expression(
        &mut self,
        hash: &HashMap<HashKey, HashPair>,
        index: Object,
    ) -> Object {
        let key = match index.hash_key() {
            Some(h) => h,
            None => {
                return Object::new_error(format!("unusable as hash key: {}", index.type_name()));
            }
        };
        let res = hash.get(&key);
        match res {
            Some(v) => v.value.clone(),
            None => Self::NULL_OBJ,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::evaluator::Evaluator;
    use crate::lexer::Lexer;
    use crate::object::{HashKey, Object};
    use crate::parser::{ExpectedLiteral, Parser};

    fn test_eval(input: &str) -> Option<Object> {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        let mut evaluator = Evaluator::new();
        evaluator.eval(program)
    }

    fn test_integer_object(
        obj: &Option<Object>,
        expected: i64,
        input: &str,
        failures: &mut Vec<String>,
    ) {
        match obj {
            Some(Object::Integer(io)) => {
                if *io != expected {
                    failures.push(format!(
                        "input='{}': object has wrong value, got={}, want={}",
                        input, io, expected
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
                    b, expected,
                    "object has wrong value, got={}, want={}",
                    b, expected
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
            Some(Object::Null) | None => {}
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
            (
                "{\"name\": \"Monkey\"}[fn(x) { x }];",
                "unusable as hash key: FUNCTION",
            ),
        ];

        let mut failures: Vec<String> = Vec::new();

        for (input, expected_message) in &tests {
            let evaluated = test_eval(input);
            match &evaluated {
                Some(Object::Error(err)) => {
                    if &err != expected_message {
                        failures.push(format!(
                            "input='{}': wrong error message, expected={}, got={}",
                            input, expected_message, err
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
                    failures.push(format!("input='{}': no object returned, got=<nil>", input));
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
                if s != "Hello World!" {
                    failures.push(format!("string has wrong value, got={}", s));
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
                if s != "Hello World!" {
                    failures.push(format!("string has wrong value, got={}", s));
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
            ("len([])", ExpectedLiteral::Int(0)),
            ("len([1])", ExpectedLiteral::Int(1)),
            ("len([1,2])", ExpectedLiteral::Int(2)),
            ("len([-1, 3, 5])", ExpectedLiteral::Int(3)),
            ("first([1])", ExpectedLiteral::Int(1)),
            ("first([])", ExpectedLiteral::Null()),
            ("first([3,4,5])", ExpectedLiteral::Int(3)),
            ("last([1])", ExpectedLiteral::Int(1)),
            ("last([])", ExpectedLiteral::Null()),
            ("last([3,4,5])", ExpectedLiteral::Int(5)),
            ("rest([])", ExpectedLiteral::Null()),
            ("rest([3])", ExpectedLiteral::IntArray(vec![])),
            ("rest([3,4,5])", ExpectedLiteral::IntArray(vec![4, 5])),
            ("push([], 3)", ExpectedLiteral::IntArray(vec![3])),
            ("push([3], 4)", ExpectedLiteral::IntArray(vec![3, 4])),
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
                        if err != s {
                            failures.push(format!(
                                "input='{}': wrong error message, expected={}, got={}",
                                input, s, err
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
                ExpectedLiteral::IntArray(ia) => match evaluated {
                    Some(Object::Array(io)) => {
                        let mut elements: Vec<i64> = Vec::new();
                        io.iter().for_each(|eo| match &eo {
                            &Object::Integer(i) => elements.push(*i),
                            _ => {}
                        });
                        if !ia.iter().eq(&elements) {
                            failures.push(format!(
                                "input='{}' should result in {:?}, got={:?}",
                                input, ia, elements
                            ));
                        }
                    }
                    _ => failures.push(format!(
                        "input='{}', should result in {:?}, got={:?}",
                        input, expected, evaluated
                    )),
                },
                ExpectedLiteral::Null() => match evaluated {
                    Some(Object::Null) => {}
                    _ => failures.push(format!(
                        "input='{}', should result in null, got={:?}",
                        input, evaluated
                    )),
                },
                _ => failures.push(format!("unhandled ExpectedLiteral {:?}", expected)),
            }
        }

        assert!(failures.is_empty(), "\n{}", failures.join("\n"));
    }

    #[test]
    fn test_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";
        let mut failures: Vec<String> = Vec::new();

        let evaluated = test_eval(input);

        let result = match &evaluated {
            Some(Object::Array(a)) => a,
            _ => {
                failures.push(format!("object is not Array, got={:?}", evaluated));
                assert!(failures.is_empty(), "\n{}", failures.join("\n"));
                return;
            }
        };

        if result.len() != 3 {
            failures.push(format!(
                "array has wrong num of elements, got={}",
                result.len()
            ));
            assert!(failures.is_empty(), "\n{}", failures.join("\n"));
            return;
        }

        test_integer_object(&Some(result[0].clone()), 1, input, &mut failures);
        test_integer_object(&Some(result[1].clone()), 4, input, &mut failures);
        test_integer_object(&Some(result[2].clone()), 6, input, &mut failures);

        assert!(failures.is_empty(), "\n{}", failures.join("\n"));
    }

    #[test]
    fn test_array_index_expressions() {
        enum Expected {
            Int(i64),
            Null,
        }

        let tests = vec![
            ("[1, 2, 3][0]", Expected::Int(1)),
            ("[1, 2, 3][1]", Expected::Int(2)),
            ("[1, 2, 3][2]", Expected::Int(3)),
            ("let i = 0; [1][i];", Expected::Int(1)),
            ("[1, 2, 3][1 + 1];", Expected::Int(3)),
            ("let myArray = [1, 2, 3]; myArray[2];", Expected::Int(3)),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                Expected::Int(6),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                Expected::Int(2),
            ),
            ("[1, 2, 3][3]", Expected::Null),
            ("[1, 2, 3][-1]", Expected::Null),
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
    fn test_map_function() {
        let input = "
let map = fn(arr, f) {
    let iter = fn(arr, accumulated) {
        if (len(arr) == 0) {
            accumulated
        } else {
            iter(rest(arr), push(accumulated, f(first(arr))));
        }
    };
    iter(arr, []);
};
let a = [1, 2, 3, 4];
let double = fn(x) { x * 2 };
map(a, double);
";

        let mut failures: Vec<String> = Vec::new();
        let evaluated = test_eval(input);

        let result = match &evaluated {
            Some(Object::Array(a)) => a,
            _ => {
                failures.push(format!("object is not Array, got={:?}", evaluated));
                assert!(failures.is_empty(), "\n{}", failures.join("\n"));
                return;
            }
        };

        assert_eq!(
            result.len(),
            4,
            "array has wrong num of elements, got={}",
            result.len()
        );

        let expected = vec![2, 4, 6, 8];
        for (i, exp) in expected.iter().enumerate() {
            test_integer_object(&Some(result[i].clone()), *exp, input, &mut failures);
        }

        assert!(failures.is_empty(), "\n{}", failures.join("\n"));
    }

    #[test]
    fn test_hash_literals() {
        let input = r#"let two = "two";
{
    "one": 10 - 9,
    two: 1 + 1,
    "thr" + "ee": 6 / 2,
    4: 4,
    true: 5,
    false: 6
}"#;

        let mut failures: Vec<String> = Vec::new();
        let evaluated = test_eval(input);

        let result = match &evaluated {
            Some(Object::Hash(h)) => h,
            _ => {
                failures.push(format!("eval didn't return Hash, got={:?}", evaluated));
                assert!(failures.is_empty(), "\n{}", failures.join("\n"));
                return;
            }
        };

        let expected: Vec<(HashKey, i64)> = vec![
            (Object::String("one".to_string()).hash_key().unwrap(), 1),
            (Object::String("two".to_string()).hash_key().unwrap(), 2),
            (Object::String("three".to_string()).hash_key().unwrap(), 3),
            (Object::Integer(4).hash_key().unwrap(), 4),
            (Object::Boolean(true).hash_key().unwrap(), 5),
            (Object::Boolean(false).hash_key().unwrap(), 6),
        ];

        if result.len() != expected.len() {
            failures.push(format!("hash has wrong num of pairs, got={}", result.len()));
            assert!(failures.is_empty(), "\n{}", failures.join("\n"));
            return;
        }

        for (expected_key, expected_value) in &expected {
            match result.get(expected_key) {
                Some(pair) => {
                    test_integer_object(
                        &Some(pair.value.clone()),
                        *expected_value,
                        input,
                        &mut failures,
                    );
                }
                None => {
                    failures.push(format!(
                        "no pair for given key in pairs, key={:?}",
                        expected_key
                    ));
                }
            }
        }

        assert!(failures.is_empty(), "\n{}", failures.join("\n"));
    }

    #[test]
    fn test_hash_index_expressions() {
        enum Expected {
            Int(i64),
            Null,
        }

        let tests = vec![
            (r#"{"foo": 5}["foo"]"#, Expected::Int(5)),
            (r#"{"foo": 5}["bar"]"#, Expected::Null),
            (r#"let key = "foo"; {"foo": 5}[key]"#, Expected::Int(5)),
            (r#"{}["foo"]"#, Expected::Null),
            (r#"{5: 5}[5]"#, Expected::Int(5)),
            (r#"{true: 5}[true]"#, Expected::Int(5)),
            (r#"{false: 5}[false]"#, Expected::Int(5)),
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
