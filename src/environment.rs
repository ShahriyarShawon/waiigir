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

    pub fn get(&self, key: String) -> Option<&Object> {
        self.store.get(&key)
    }
    pub fn set(&mut self, key: String, val: Object) -> &mut Object {
        self.store.entry(key).or_insert(val)
    }
}
