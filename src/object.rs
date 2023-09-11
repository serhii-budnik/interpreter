use std::fmt::Display;

pub enum ObjectType {
    INTEGER,
    BOOLEAN,
    NULL,
}

pub trait Object: Display {
    fn obj_type(&self) -> ObjectType;
}

pub struct Integer {
    pub value: isize,
}

pub struct Boolean {
    pub value: bool,
}

pub struct Null {}

impl Object for Integer {
    fn obj_type(&self) -> ObjectType {
        ObjectType::INTEGER
    }
}

impl Display for Integer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Object for Boolean {
    fn obj_type(&self) -> ObjectType {
        ObjectType::BOOLEAN
    }
}

impl Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Object for Null {
    fn obj_type(&self) -> ObjectType {
        ObjectType::NULL
    }
}

impl Display for Null {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "null")
    }
}
