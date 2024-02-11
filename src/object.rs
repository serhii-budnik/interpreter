#[derive(Debug, PartialEq)]
pub enum ObjectType {
    Int(isize),
    Bool(bool),
    Null,
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
            Self::Bool(value) => write!(f, "{}", value),
            Self::Null => write!(f, "null"),
        }
    }
}

impl ObjectType {
    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Bool(boolean) => *boolean,
            Self::Null => false,
            _ => true,
        }
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
