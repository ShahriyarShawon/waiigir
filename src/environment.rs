use std::cell::RefCell;
use std::rc::Rc;

use std::collections::HashMap;

use crate::object::Object;

#[derive(Debug, Clone)]
pub struct Environment {
    pub store: HashMap<String, Object>,
    pub outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Rc<RefCell<Environment>> {
        Rc::new(RefCell::new(Environment {
            store: HashMap::new(),
            outer: None,
        }))
    }

    pub fn new_enclosed_environment(outer: &Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
        Rc::new(RefCell::new(Environment {
            store: HashMap::new(),
            outer: Some(outer.clone()),
        }))
    }
    pub fn get(&self, key: &str) -> Option<Object> {
        match self.store.get(key) {
            Some(o) => Some(o.clone()),
            None => match &self.outer {
                Some(outer) => return outer.borrow().get(key),
                None => None,
            },
        }
    }
    pub fn set(&mut self, key: &str, val: Object) {
        self.store.insert(String::from(key), val);
    }
}
