use std::cell::RefCell;
use std::rc::Rc;
use crate::ast::{Expr, Statement};
use crate::environment::Environment;

#[derive(Debug, PartialEq, Clone)]
pub enum ObjectType {
    Int(isize),
    Bool(bool),
    Null,
    Function(Rc<(Vec<Rc<Expr>>, Rc<Statement>, Rc<RefCell<Environment>>)>),
    Array(Rc<Vec<ObjectType>>),
    OString(Rc<RefCell<String>>),
    Error(String),
}

pub const FALSE_OBJ: ObjectType = ObjectType::Bool(false);
pub const NULL_OBJ: ObjectType = ObjectType::Null;
pub const TRUE_OBJ: ObjectType = ObjectType::Bool(true);

impl From<Option<ObjectType>> for ObjectType {
    fn from(value: Option<ObjectType>) -> Self {
        match value {
            Some(o) => o,
            None => NULL_OBJ,
        }
    }
}

impl std::fmt::Display for ObjectType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Int(value) => write!(f, "{}", value),
            Self::OString(value) => write!(f, "\"{}\"", value.borrow()),
            Self::Bool(value) => write!(f, "{}", value),
            Self::Null => write!(f, "null"),
            Self::Function(_) => write!(f, "FUNCTION OBJECT"),
            Self::Array(value) => {
                let mut arr = Vec::with_capacity(value.len());

                for val in value.iter() {
                    arr.push(val.to_string());
                };

                write!(f, "[{}]", arr.join(", "))
            }
            Self::Error(str) => write!(f, "Error: {}", str),
        }
    }
}

impl ObjectType {
    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Bool(boolean) => *boolean,
            Self::Null => false,
            Self::Error(_) => panic!("Can not convert Error object to truthy"),
            _ => true,
        }
    }

    pub fn new_error(msg: String) -> ObjectType {
        ObjectType::Error(msg)
    }

    pub fn type_name(&self) -> &str {
        match self {
            Self::Int(_) => "INTEGER",
            Self::OString(_) => "STRING",
            Self::Bool(_) => "BOOLEAN",
            Self::Null => "NULL",
            Self::Function(_) => "FUNCTION",
            Self::Array(_) => "ARRAY",
            Self::Error(_) => "ERROR",
        }
    }

    pub fn new_function(
        parameters: Vec<Rc<Expr>>,
        body: Rc<Statement>,
        env: Rc<RefCell<Environment>>,
    ) -> ObjectType {
        ObjectType::Function(Rc::new((parameters, body, env)))
    }
}

// tests
#[cfg(test)]
mod test {
    use super::ObjectType;

    #[test]
    fn test_type_is_truthy() {
        assert_eq!(ObjectType::Int(0).is_truthy(), true);
        assert_eq!(ObjectType::Int(1).is_truthy(), true);
        assert_eq!(ObjectType::Bool(true).is_truthy(), true);
        assert_eq!(ObjectType::Bool(false).is_truthy(), false);
        assert_eq!(ObjectType::Null.is_truthy(), false);
    }
}
