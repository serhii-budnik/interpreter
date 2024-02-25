use crate::object::ObjectType;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    store: HashMap<String, ObjectType>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn extend_env(outer: Rc<RefCell<Environment>>) -> Self {
        Environment {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }

    pub fn get(& self, key: &str) -> Option<&ObjectType> {
        let res = self.store.get(key);

        if res.is_none() && self.outer.is_some() {
            if let Some(out) = &self.outer {
                let out: *mut Environment = out.as_ptr();
                return unsafe { (&*out).get(key) };
            }

            None
        } else {
            res
        }
    }

    pub fn set(&mut self, key: String, value: ObjectType) {
        self.store.insert(key, value);
    }
}

impl Into<Rc<RefCell<Environment>>> for Environment {
    fn into(self) -> Rc<RefCell<Environment>> {
        Rc::new(RefCell::new(self))
    }
}
