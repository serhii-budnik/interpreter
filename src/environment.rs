use crate::{object::ObjectType, ast::Expr};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    store: HashMap<Rc<Expr>, ObjectType>,
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

    pub fn get(&self, key: &Rc<Expr>) -> Option<&ObjectType> {
        let res = self.store.get(key);

        if res.is_none() && self.outer.is_some() {
            let env_pointer: *mut Environment = self.outer.as_ref().unwrap().as_ptr();
            unsafe { (*env_pointer).get(key) }
        } else {
            res
        }
    }

    pub fn set(&mut self, key: Rc<Expr>, value: ObjectType) {
        self.store.insert(key, value);
    }
}

impl Into<Rc<RefCell<Environment>>> for Environment {
    fn into(self) -> Rc<RefCell<Environment>> {
        Rc::new(RefCell::new(self))
    }
}
