use std::{
    cell::RefCell,
    collections::HashMap,
    hash::{DefaultHasher, Hash, Hasher},
    rc::Rc,
};

use crate::{
    ast::{BlockStatement, IdentifierExpression},
    environment::Environment,
};

#[allow(clippy::upper_case_acronyms)]
type BIFO = fn(Vec<Object>) -> Object;

#[derive(Hash, PartialEq, Eq, Debug, Clone)]
pub struct HashKey {
    pub type_name: &'static str,
    pub value: u64,
}

pub trait Hashable {
    fn hash_key(&self) -> HashKey;
}

#[derive(Debug, Clone)]
pub enum Object {
    Function(FunctionObject),
    Integer(IntegerObject),
    Boolean(BooleanObject),
    Return(ReturnValue),
    String(StringObject),
    BuiltInFunction(BuiltInFunctionObject),
    Array(ArrayObject),
    Hash(HashObject),
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
                fo.body
            ),
            Object::Integer(io) => io.value.to_string(),
            Object::Boolean(bo) => bo.value.to_string(),
            Object::String(so) => so.value.to_string(),
            Object::BuiltInFunction(_bif) => String::from("builtin function"),
            Object::Hash(ho) => {
                let mut pairs: Vec<String> = Vec::new();

                for (_, pair) in ho.pairs.iter() {
                    pairs.push(format!("{}: {}", pair.key.Inspect(), pair.value.Inspect()))
                }

                let out = format!("{{{}}}", pairs.join(", "));
                out
            }
            Object::Array(ao) => {
                let mut out = String::new();
                let e = ao
                    .elements
                    .iter()
                    .map(|o| o.Inspect())
                    .collect::<Vec<String>>()
                    .join(", ");
                out += &format!("[{}]", e);

                out
            }
            Object::Return(ro) => format!("{:?}", ro.value),
            Object::Error(eo) => eo.message.to_string(),
            Object::Null(_) => String::from("null"),
        }
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
            Object::Hash(_) => "HASH",
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

impl Hashable for IntegerObject {
    fn hash_key(&self) -> HashKey {
        HashKey {
            type_name: "INTEGER",
            value: self.value as u64,
        }
    }
}

#[derive(Debug, Clone)]
pub struct BooleanObject {
    pub value: bool,
}

impl Hashable for BooleanObject {
    fn hash_key(&self) -> HashKey {
        HashKey {
            type_name: "BOOLEAN",
            value: self.value as u64,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ReturnValue {
    pub value: Box<Object>,
}

#[derive(Debug, Clone)]
pub struct StringObject {
    pub value: String,
}

impl Hashable for StringObject {
    fn hash_key(&self) -> HashKey {
        let mut hasher = DefaultHasher::new();
        self.value.hash(&mut hasher);
        HashKey {
            type_name: "STRING",
            value: hasher.finish(),
        }
    }
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
pub struct HashPair {
    pub key: Object,
    pub value: Object,
}

#[derive(Debug, Clone)]
pub struct HashObject {
    pub pairs: HashMap<HashKey, HashPair>,
}

#[derive(Debug, Clone)]
pub struct ErrorObject {
    pub message: String,
}

#[derive(Debug, Clone)]
pub struct NullObject {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_hash_key() {
        let hello1 = StringObject {
            value: "Hello World".to_string(),
        };
        let hello2 = StringObject {
            value: "Hello World".to_string(),
        };
        let diff1 = StringObject {
            value: "My name is johnny".to_string(),
        };
        let diff2 = StringObject {
            value: "My name is johnny".to_string(),
        };

        assert_eq!(
            hello1.hash_key(),
            hello2.hash_key(),
            "strings with same content should be equal"
        );
        assert_eq!(
            diff1.hash_key(),
            diff2.hash_key(),
            "strings with same content should be equal"
        );
        assert_ne!(
            hello1.hash_key(),
            diff1.hash_key(),
            "strings with different content should not be equal"
        );
    }
}
