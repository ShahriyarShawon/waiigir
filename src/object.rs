use std::fmt;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    ast::{BlockStatement, IdentifierExpression},
    environment::Environment,
};

#[allow(clippy::upper_case_acronyms)]
type BIFO = fn(Vec<Object>) -> Object;

#[derive(Hash, PartialEq, Eq, Debug, Clone)]
pub enum HashKey {
    Integer(i64),
    Boolean(bool),
    String(String),
}

#[derive(Debug, Clone)]
pub enum Object {
    Function(FunctionObject),
    Integer(i64),
    Boolean(bool),
    Return(Box<Object>),
    String(String),
    BuiltInFunction(BIFO),
    Array(Vec<Object>),
    Hash(HashMap<HashKey, HashPair>),
    Null,
    Error(String),
}

impl Object {
    pub fn hash_key(&self) -> Option<HashKey> {
        match self {
            Object::Integer(i) => Some(HashKey::Integer(*i)),
            Object::Boolean(b) => Some(HashKey::Boolean(*b)),
            Object::String(s) => Some(HashKey::String(s.clone())),
            _ => None,
        }
    }

    pub fn new_error(message: impl Into<String>) -> Object {
        Object::Error(message.into())
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
            Object::Null => "NULL",
            Object::Error(_) => "ERROR",
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Function(fo) => write!(
                f,
                "fn ({}) {{\n{}\n}}",
                fo.parameters
                    .iter()
                    .map(|f| f.value.clone())
                    .collect::<Vec<String>>()
                    .join(", "),
                fo.body
            ),
            Object::Integer(io) => write!(f, "{}", io),
            Object::Boolean(bo) => write!(f, "{}", bo),
            Object::String(so) => write!(f, "{}", so),
            Object::BuiltInFunction(_bif) => write!(f, "builtin function"),
            Object::Hash(ho) => {
                let mut pairs: Vec<String> = Vec::new();

                for (_, pair) in ho.iter() {
                    pairs.push(format!("{}: {}", pair.key, pair.value))
                }

                write!(f, "{{{}}}", pairs.join(", "))
            }
            Object::Array(ao) => {
                let e = ao
                    .iter()
                    .map(|o| o.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "[{}]", e)
            }
            Object::Return(ro) => write!(f, "{}", ro),
            Object::Error(eo) => write!(f, "{}", eo),
            Object::Null => write!(f, "null"),
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
pub struct HashPair {
    pub key: Object,
    pub value: Object,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_hash_key() {
        let hello1 = Object::String("Hello World".to_string());
        let hello2 = Object::String("Hello World".to_string());
        let diff1 = Object::String("My name is johnny".to_string());
        let diff2 = Object::String("My name is johnny".to_string());

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
