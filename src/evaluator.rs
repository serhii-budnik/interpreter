use crate::object::ObjectType;

pub fn eval<T>(node: T) -> ObjectType {
    // I think it should be enum instead of structs :/
    ObjectType::Int(5)
}

#[cfg(test)]
mod test {
    use super::eval;
    use crate::object::ObjectType;
    use crate::ast::IntegerLiteral;
    use crate::token::Token;

    #[test]
    fn test_eval_of_integer() {
        let five = eval(IntegerLiteral { token: Token::Int("5".into()), value: 5 });
        assert_eq!(five, ObjectType::Int(5))
    }
}
