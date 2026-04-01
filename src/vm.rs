use crate::code::{Instructions, Opcode, read_uint16};
use crate::compiler::Bytecode;
use crate::object::Object;

const STACK_SIZE: usize = 2048;
const TRUE_OBJ: Object = Object::Boolean(true);
const FALSE_OBJ: Object = Object::Boolean(false);

#[derive(Debug)]
pub struct VM {
    pub constants: Vec<Object>,
    pub instructions: Instructions,
    stack: Vec<Object>,
    last_popped: Option<Object>,
}

impl VM {
    pub fn new(bytecode: Bytecode) -> Self {
        VM {
            constants: bytecode.constants,
            instructions: bytecode.instructions,
            stack: Vec::with_capacity(STACK_SIZE),
            last_popped: None,
        }
    }

    pub fn stack_top(&mut self) -> Option<Object> {
        if self.stack.is_empty() {
            return None;
        }
        self.stack.pop()
    }

    fn push(&mut self, o: Object) -> Result<(), String> {
        if self.stack.len() >= STACK_SIZE {
            return Err(String::from("stack overflow"));
        }

        self.stack.push(o);
        Ok(())
    }

    fn pop(&mut self) -> Result<Object, String> {
        match self.stack.pop() {
            Some(o) => {
                self.last_popped = Some(o.clone());
                Ok(o)
            }
            None => Err(String::from("stack underflow")),
        }
        // self.stack.pop().ok_or(String::from("stack underflow"))
    }

    fn execute_binary_operation(&mut self, op: &Opcode) -> Result<(), String> {
        let right_value = match self.pop() {
            Ok(o) => match o {
                Object::Integer(i) => i,
                _ => return Err(format!("unsupported types for binary operation {}", o)),
            },
            Err(e) => return Err(e),
        };

        let left_value = match self.pop() {
            Ok(o) => match o {
                Object::Integer(i) => i,
                _ => return Err(format!("unsupported types for binary operation {}", o)),
            },
            Err(e) => return Err(e),
        };

        self.execute_binary_integer_operation(op, left_value, right_value)
    }

    fn execute_binary_integer_operation(
        &mut self,
        op: &Opcode,
        left: i64,
        right: i64,
    ) -> Result<(), String> {
        let res = match op {
            Opcode::OpAdd => left + right,
            Opcode::OpSub => left - right,
            Opcode::OpMul => left * right,
            Opcode::OpDiv => left / right,
            _ => return Err(format!("unknown integer operator: {}", op)),
        };
        self.push(Object::Integer(res))
    }

    fn run_op_constant(&mut self, ip: &mut usize) -> Result<(), String> {
        let const_index = read_uint16(&self.instructions[*ip + 1..]) as usize;
        *ip += 2;
        let res = self.push(self.constants[const_index].clone());
        res
    }

    pub fn run(&mut self) -> Result<(), String> {
        let mut ip = 0;
        loop {
            if ip >= self.instructions.len() {
                break;
            }

            let op = unsafe { std::mem::transmute::<u8, Opcode>(self.instructions[ip]) };
            match op {
                Opcode::OpAdd | Opcode::OpSub | Opcode::OpMul | Opcode::OpDiv => {
                    self.execute_binary_operation(&op)?
                }
                Opcode::OpConstant => self.run_op_constant(&mut ip)?,
                Opcode::OpPop => {
                    self.pop()?;
                }
                Opcode::OpTrue => {
                    self.push(TRUE_OBJ)?
                },
                Opcode::OpFalse => {
                    self.push(FALSE_OBJ)?
                },
            }

            ip += 1;
        }

        Ok(())
    }

    #[allow(dead_code)]
    pub fn last_popped_stack_elem(&self) -> Option<Object> {
        match &self.last_popped {
            Some(l) => Some(l.clone()),
            None => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Program;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::{compiler::Compiler, object::Object, parser::ExpectedLiteral};

    struct VmTestCase {
        input: String,
        expected: ExpectedLiteral,
    }

    impl VmTestCase {
        fn new(input: &str, expected: ExpectedLiteral) -> Self {
            VmTestCase {
                input: String::from(input),
                expected,
            }
        }
    }

    fn parse(input: &String) -> Program {
        let l = Lexer::new(&input);
        let mut p = Parser::new(l);
        return p.parse_program();
    }

    fn test_integer_object(expected: i64, actual: Object) -> Result<(), String> {
        match actual {
            Object::Integer(io) => {
                if io != expected {
                    return Err(format!(
                        "object hast wrong value. got={}, want={}",
                        io, expected
                    ));
                }
                Ok(())
            }
            _ => Err(format!("object is not integer. got={}", actual)),
        }
    }

    fn test_boolean_object(expected: bool, actual: Object) -> Result<(), String> {
        match actual {
            Object::Boolean(bo) => {
                if bo != expected {
                    return Err(format!(
                        "object hast wrong value. got={}, want={}",
                        bo, expected
                    ));
                }
                Ok(())
            }
            _ => Err(format!("object is not boolean. got={}", actual)),
        }
    }

    fn test_expected_object(expected: &ExpectedLiteral, actual: Object) -> Result<(), String> {
        match expected {
            ExpectedLiteral::Int(i) => {
                let res = test_integer_object(*i, actual);
                match res {
                    Ok(_) => {}
                    Err(e) => return Err(e),
                }
            },
            ExpectedLiteral::Boolean(b) => {
                let res = test_boolean_object(*b, actual);
                match res {
                    Ok(_) => {},
                    Err(e) => return Err(e)
                }
            }
            _ => todo!(),
        }
        Ok(())
    }

    fn run_vm_tests(tests: &[VmTestCase]) -> Result<(), String> {
        for t in tests {
            let program = parse(&t.input);
            let mut comp = Compiler::new();
            let res = comp.compile(&program);
            match res {
                Err(e) => panic!("compiler error: {}", e),
                Ok(_) => {}
            }

            let mut vm = VM::new(comp.bytecode());
            eprintln!("{:?}", comp.bytecode());
            let res = vm.run();
            match res {
                Err(e) => panic!("vm error: {}", e),
                Ok(_) => {}
            }

            let stack_elem = vm.last_popped_stack_elem();
            match stack_elem {
                Some(s) => test_expected_object(&t.expected, s)?,
                None => return Err(format!("tried to pop stack while stack empty")),
            }
        }
        Ok(())
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![
            VmTestCase::new("1", ExpectedLiteral::Int(1)),
            VmTestCase::new("2", ExpectedLiteral::Int(2)),
            VmTestCase::new("1+2", ExpectedLiteral::Int(3)),
            VmTestCase::new("1 - 2", ExpectedLiteral::Int(-1)),
            VmTestCase::new("1 * 2", ExpectedLiteral::Int(2)),
            VmTestCase::new("4 / 2", ExpectedLiteral::Int(2)),
            VmTestCase::new("50 / 2 * 2 + 10 - 5", ExpectedLiteral::Int(55)),
            VmTestCase::new("5 + 5 + 5 + 5 - 10", ExpectedLiteral::Int(10)),
            VmTestCase::new("2 * 2 * 2 * 2 * 2", ExpectedLiteral::Int(32)),
            VmTestCase::new("5 * 2 + 10", ExpectedLiteral::Int(20)),
            VmTestCase::new("5 + 2 * 10", ExpectedLiteral::Int(25)),
            VmTestCase::new("5 * (2 + 10)", ExpectedLiteral::Int(60)),
        ];

        match run_vm_tests(&tests) {
            Ok(()) => {}
            Err(e) => panic!("test_integer_arithmetic error: {}", e),
        }
    }


    #[test]
    fn test_boolean_expressions() {
        let tests = vec![
            VmTestCase::new("true", ExpectedLiteral::Boolean(true)),
            VmTestCase::new("false", ExpectedLiteral::Boolean(false)),
        ];

        match run_vm_tests(&tests) {
            Ok(()) => {}
            Err(e) => panic!("test_boolean_expressions error: {}", e),
        }
    }
}
