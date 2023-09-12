use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub enum ObjectType {
    Int(Integer),
    Bool(Boolean),
    Null(Null),
}

pub struct Integer {
    pub value: isize,
}

pub struct Boolean {
    pub value: bool,
}

pub struct Null {}

impl Display for Integer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Display for Null {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "null")
    }
}
