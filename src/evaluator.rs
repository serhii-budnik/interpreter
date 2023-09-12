use crate::ast::Node;
use crate::object::{Object, Integer} ;

pub fn eval<T>(node: T) -> Box<dyn Object>
where T: Node {
    // I think it should be enum instead of structs :/
    Box::new(Integer { value: 5 })
}
