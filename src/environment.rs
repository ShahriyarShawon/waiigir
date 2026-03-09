use std::collections::HashMap;

use crate::object::Object;

#[derive(Debug, Default, Clone)]
pub struct Environment {
    pub store: HashMap<String, Object>,
    pub outer: Option<Box<Environment>>
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            outer: None
        }
    }

    pub fn new_enclosed_environment(outer: Box<Environment>) -> Environment {
        let mut env = Environment::new();
        env.outer = Some(outer); 
        env
    }
    pub fn get(&self, key: &str) -> Option<Object> {
        match self.store.get(key) {
            Some(o) => Some(o.clone()),
            None => match &self.outer {
                Some(outer) => return outer.get(key) ,
                None => None
            }
        }
    }
    pub fn set(&mut self, key: &str, val: Object) -> &mut Object {
        self.store.entry(String::from(key)).or_insert(val)
    }
}
