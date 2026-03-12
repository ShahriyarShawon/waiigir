use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{BlockStatement, IdentifierExpression},
    environment::Environment,
};

type BIFO = fn(Vec<Object>) -> Object;

#[derive(Debug, Clone)]
pub enum Object {
    Function(FunctionObject),
    Integer(IntegerObject),
    Boolean(BooleanObject),
    Return(ReturnValue),
    String(StringObject),
    BuiltInFunction(BuiltInFunctionObject),
    Array(ArrayObject),
    Null(NullObject),
    Error(ErrorObject),
}

#[allow(non_snake_case)]
impl Object {
    pub fn Inspect(&self) -> String {
        match self {
            Object::Function(fo) => format!(
                "fn ({}) {{\n{}\n}}",
                fo.parameters
                    .iter()
                    .map(|f| f.value.clone())
                    .collect::<Vec<String>>()
                    .join(", "),
                fo.body.to_string()
            ),
            Object::Integer(io) => format!("{}", io.value),
            Object::Boolean(bo) => format!("{}", bo.value),
            Object::String(so) => format!("{}", so.value),
            Object::BuiltInFunction(_bif) => String::from("builtin function"),
            Object::Array(ao) => {
                let mut out = String::new();
                let e = ao.elements.iter().map(|o| o.Inspect()).collect::<Vec<String>>().join(", ");
                out += &format!("[{}]", e);

                out

            },
            Object::Return(ro) => format!("{:?}", ro.value),
            Object::Error(eo) => format!("{}", eo.message),
            Object::Null(_) => String::from("null"),
        }
    }

    pub fn new_integer(val: i64) -> Object {
        Object::Integer(IntegerObject { value: val })
    }

    pub fn new_error(message: String) -> Object {
        Object::Error(ErrorObject { message })
    }

    pub fn type_name(&self) -> &str {
        match self {
            Object::Function(_) => "FUNCTION",
            Object::Integer(_) => "INTEGER",
            Object::Boolean(_) => "BOOLEAN",
            Object::String(_) => "STRING",
            Object::Return(_) => "RETURN",
            Object::BuiltInFunction(_) => "BUILTIN",
            Object::Array(_) => "ARRAY",
            Object::Null(_) => "NULL",
            Object::Error(_) => "ERROR",
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionObject {
    pub parameters: Vec<IdentifierExpression>,
    pub body: BlockStatement,
    pub env: Rc<RefCell<Environment>>,
}

#[derive(Debug, Clone)]
pub struct IntegerObject {
    pub value: i64,
}

#[derive(Debug, Clone)]
pub struct BooleanObject {
    pub value: bool,
}

#[derive(Debug, Clone)]
pub struct ReturnValue {
    pub value: Box<Object>,
}

#[derive(Debug, Clone)]
pub struct StringObject {
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct BuiltInFunctionObject {
    pub function: BIFO,
}

#[derive(Debug, Clone)]
pub struct ArrayObject {
    pub elements: Vec<Object>,
}

#[derive(Debug, Clone)]
pub struct ErrorObject {
    pub message: String,
}

#[derive(Debug, Clone)]
pub struct NullObject {}
