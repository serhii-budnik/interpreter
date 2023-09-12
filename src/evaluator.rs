use crate::object::{ObjectType, Integer} ;

pub fn eval<T>(node: T) -> ObjectType {
    // I think it should be enum instead of structs :/
    ObjectType::Int(Integer { value: 5 })
}

#[cfg(test)]
mod test {
    use super::eval;
    use crate::ast::IntegerLiteral;
    use crate::token::Token;

    #[test]
    fn test_eval_of_integer() {
        let five = eval(IntegerLiteral { token: Token::Int("5".into()), value: 5 });
    }
}
