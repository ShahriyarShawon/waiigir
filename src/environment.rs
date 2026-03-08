use std::collections::HashMap;

use crate::object::Object;

#[derive(Debug, Default)]
pub struct Environment {
    pub store: HashMap<String, Object>
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            ..Default::default()
        }
    }

    pub fn get(&self, key: &str) -> Option<Object> {
        match self.store.get(key) {
            Some(o) => Some(o.clone()),
            None => None
        }
    }
    pub fn set(&mut self, key: &str, val: Object) -> &mut Object {
        self.store.entry(String::from(key)).or_insert(val)
    }
}
