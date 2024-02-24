use crate::object::ObjectType;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Environment {
    store: HashMap<String, ObjectType>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            store: HashMap::new(),
        }
    }

    pub fn get(&self, key: &str) -> Option<&ObjectType> {
        self.store.get(key)
    }

    pub fn set(&mut self, key: String, value: ObjectType) {
        self.store.insert(key, value);
    }
}
