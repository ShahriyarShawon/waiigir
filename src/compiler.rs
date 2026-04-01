use crate::ast::Program;
use crate::ast::{self, Expression};
use crate::code;
use crate::code::{Instructions, Opcode};
use crate::object::Object;

#[derive(Debug)]
pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

#[derive(Debug)]
pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            instructions: Instructions::new(),
            constants: Vec::new(),
        }
    }

    // fn compile_let(ls: LetStatement) -> Option<String> {
    //     None
    // }
    //
    // fn compile_return(ls: ReturnStatement) -> Option<String> {
    //     None
    // }
    //

    fn add_constant(&mut self, obj: Object) -> i32 {
        self.constants.push(obj);
        (self.constants.len() - 1) as i32
    }

    fn add_instruction(&mut self, ins: &[u8]) -> usize {
        let pos_new_instruction = self.instructions.len();
        self.instructions.extend_from_slice(ins);
        pos_new_instruction
    }

    fn emit(&mut self, op: Opcode, operands: &[i32]) -> usize {
        let ins = code::make(&op, operands);
        self.add_instruction(&ins)
    }

    fn compile_expression(&mut self, es: &Expression) -> Result<(), String> {
        match es {
            Expression::Infix(ie) => {
                let lres = self.compile_expression(&ie.left);
                match lres {
                    Ok(_) => {}
                    Err(e) => return Err(e),
                }

                let rres = self.compile_expression(&ie.right);
                match rres {
                    Ok(_) => {}
                    Err(e) => return Err(e),
                }

                match ie.operator.as_str() {
                    "+" => {
                        self.emit(Opcode::OpAdd, &[]);
                        Ok(())
                    }
                    _ => Err(format!("unknown operator {}", ie.operator)),
                }
            }
            Expression::Integer(ie) => {
                let integer = Object::Integer(ie.value);
                let const_pos = self.add_constant(integer);
                self.emit(Opcode::OpConstant, &[const_pos]);
                Ok(())
            }
            _ => todo!(),
        }
    }

    pub fn compile(&mut self, p: &Program) -> Result<(), String> {
        for s in &p.statements {
            match s {
                ast::Statement::Let(_ls) => {
                    todo!()
                }
                ast::Statement::Return(_rs) => {
                    todo!()
                }
                ast::Statement::Expression(es) => match &es.expression {
                    Some(e) => self.compile_expression(e)?,
                    None => todo!(),
                },
            }
        }
        Ok(())
    }

    pub fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.instructions.clone(),
            constants: self.constants.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::ExpectedLiteral;
    use crate::{ast::Program, lexer::Lexer, parser::Parser};

    use super::*;

    struct CompilerTestCase {
        input: String,
        expected_constants: Vec<ExpectedLiteral>,
        expected_instructions: Vec<Instructions>,
    }

    fn parse(input: String) -> Program {
        let l = Lexer::new(&input);
        let mut p = Parser::new(l);
        p.parse_program()
    }

    fn concat_instructions(instructions: &[Instructions]) -> Instructions {
        let mut out = Instructions::new();

        for ins in instructions {
            for i in ins.iter() {
                out.push(*i);
            }
        }

        out
    }

    fn test_integer_object(expected: i64, actual: &Object) -> Option<String> {
        let res = match actual {
            Object::Integer(i) => i,
            _ => return Some(format!("object is not Integer. got={}", actual)),
        };

        if *res != expected {
            return Some(format!(
                "object has wrong value. got={}, want={}",
                res, expected
            ));
        }

        return None;
    }

    fn test_constants(expected: Vec<ExpectedLiteral>, actual: Vec<Object>) -> Option<String> {
        if expected.len() != actual.len() {
            return Some(format!(
                "wrong number of constants. got={}, want={}",
                expected.len(),
                actual.len()
            ));
        }

        for (idx, constant) in expected.iter().enumerate() {
            match constant {
                &ExpectedLiteral::Int(i) => {
                    let err = test_integer_object(i, &actual[idx]);
                    match err {
                        Some(e) => {
                            return Some(format!(
                                "constant {} - testIntegerObject failed: {}",
                                idx, e
                            ));
                        }
                        _ => {}
                    }
                }

                _ => {}
            }
        }

        return None;
    }

    fn test_instructions(expected: &[Instructions], actual: Instructions) -> Option<String> {
        let concatted = concat_instructions(expected);
        if actual.len() != concatted.len() {
            return Some(format!(
                "wrong instructions length.\nwant={}\ngot={}",
                concatted, actual
            ));
        }

        for (idx, ins) in concatted.iter().enumerate() {
            if &actual[idx] != ins {
                return Some(format!(
                    "wrong instructions at {}.\nwant={:?}\ngot={:?}",
                    idx, concatted, actual
                ));
            }
        }

        return None;
    }

    fn run_compiler_tests(tests: &[CompilerTestCase]) {
        let mut errors: Vec<String> = Vec::new();

        for t in tests {
            let program = parse(t.input.clone());
            let mut compiler = Compiler::new();
            let res = compiler.compile(&program);
            match res {
                // compiled succesfully
                Ok(_) => {}
                // error returned
                Err(e) => errors.push(format!("compiler error: {}", e)),
            }

            let bytecode = compiler.bytecode();
            let mut err = test_instructions(&t.expected_instructions, bytecode.instructions);
            match err {
                // error returned
                Some(e) => errors.push(format!("testInstructions failed: {}", e)),
                // compiled succesfully
                None => {}
            }

            err = test_constants(t.expected_constants.clone(), bytecode.constants);
            match err {
                // error returned
                Some(e) => errors.push(format!("testConstants failed: {}", e)),
                // compiled succesfully
                None => {}
            }
        }

        assert!(errors.is_empty(), "\n{}", errors.join("\n"));
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![CompilerTestCase {
            input: "1 + 2".to_string(),
            expected_constants: vec![ExpectedLiteral::Int(1), ExpectedLiteral::Int(2)],
            expected_instructions: vec![
                Instructions(code::make(&Opcode::OpConstant, &vec![0])),
                Instructions(code::make(&Opcode::OpConstant, &vec![1])),
                Instructions(code::make(&Opcode::OpAdd, &vec![])),
            ],
        }];

        run_compiler_tests(&tests);
    }
}
