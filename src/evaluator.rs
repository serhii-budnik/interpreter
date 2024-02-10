use crate::object::ObjectType;
use crate::ast::Expr;

pub fn eval(node: Expr) -> ObjectType {
    // I think it should be enum instead of structs :/
    ObjectType::Int(5)
}

#[cfg(test)]
mod test {
    use super::eval;
    use crate::object::ObjectType;
    use crate::token::Token;
    use crate::ast::Expr;

    #[test]
    fn test_eval_integer_expression() {
        let five = eval(Expr::Int(Token::Int("5".into())));

        assert_eq!(five, ObjectType::Int(5));
    }
}
