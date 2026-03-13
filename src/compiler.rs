use crate::ast::{Program};
use crate::code::{Instructions, make};
use crate::object::Object;

pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

pub struct Compiler {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            instructions: Instructions::new(),
            constants: Vec::new(),
        }
    }

    pub fn compile(&self, p: Program) -> Result<bool, String>{
        todo!()
    }
    pub fn bytecode(&self) -> Bytecode {
        todo!()
    }
}

