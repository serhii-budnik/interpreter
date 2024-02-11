use crate::ast::{Expr, Program, Statement};
use crate::object::{ObjectType, FALSE_OBJ, NULL_OBJ, TRUE_OBJ};
use crate::token::Token;

pub trait Evaluator {
    fn eval(&self) -> ObjectType;
}

impl Evaluator for Expr {
    fn eval(&self) -> ObjectType {
        match self {
            Self::Int(token) => {
                let int_val: isize = token.value().parse().unwrap();

                ObjectType::Int(int_val)
            },
            Self::Bool(token) => {
                match token {
                    Token::True => TRUE_OBJ,
                    Token::False => FALSE_OBJ,
                    _ => panic!("expected Expr::Bool, got {:?}", token),
                }
            },
            Self::Prefix(op, right) => {
                match op {
                    Token::Bang => {
                        if right.eval().is_truthy() { FALSE_OBJ } else { TRUE_OBJ }
                    },
                    Token::Minus => {
                        match right.eval() {
                            ObjectType::Int(val) => ObjectType::Int(-val),
                            _ => panic!("expected Expr::Int, got {:?}", right),
                        }
                    }
                    _ => panic!("eval for prefix operator not implemented"),
                }
            },
            Self::Infix(left, op, right) => {
                match op {
                    Token::Plus | Token::Minus | Token::Asterisk | Token::Slash => {
                        let left_val = left.eval();
                        let right_val = right.eval();

                        match (left_val, right_val) {
                            (ObjectType::Int(l), ObjectType::Int(r)) => {
                                match op {
                                    Token::Plus => ObjectType::Int(l + r),
                                    Token::Minus => ObjectType::Int(l - r),
                                    Token::Asterisk => ObjectType::Int(l * r),
                                    Token::Slash => ObjectType::Int(l / r),
                                    _ => panic!("expected infix operator, got {:?}", op),
                                }
                            },
                            (l, _) => panic!("expected Expr::Int, got {:?}", l),
                        }
                    },
                    Token::Eq | Token::NotEq => {
                        let left_val = left.eval();
                        let right_val = right.eval();

                        match (left_val, right_val) {
                            (ObjectType::Int(l), ObjectType::Int(r)) => {
                                match op {
                                    Token::Eq => ObjectType::Bool(l == r),
                                    Token::NotEq => ObjectType::Bool(l != r),
                                    _ => panic!("expected infix operator, got {:?}", op),
                                }
                            },
                            (ObjectType::Bool(l), ObjectType::Bool(r)) => {
                                match op {
                                    Token::Eq => ObjectType::Bool(l == r),
                                    Token::NotEq => ObjectType::Bool(l != r),
                                    _ => panic!("expected infix operator, got {:?}", op),
                                }
                            },
                            (l, _) => panic!("expected Expr::Int or Expr::Bool for left and right nodes, got {:?}", l),
                        }
                    },
                    _ => panic!("eval for infix operator not implemented"),
                }
            },
            _ => NULL_OBJ,
        }
    }
}

impl Evaluator for Statement {
    fn eval(&self) -> ObjectType {
        match self {
            Statement::ExprStatement(expr) => expr.eval(),
            _ => todo!(),
        }
    }
}

impl Evaluator for Program {
    fn eval(&self) -> ObjectType {
        let mut result = NULL_OBJ;

        for stmt in &self.statements {
            result = stmt.eval();
        }

        result
    }
}


#[cfg(test)]
mod test {
    use super::Evaluator;
    use crate::object::{ObjectType, TRUE_OBJ, FALSE_OBJ};
    use crate::token::Token;
    use crate::ast::{Expr, Statement};

    #[test]
    fn test_eval_integer_expression() {
        let five = Expr::Int(Token::Int("5".into())).eval();
        let one_o_nine = Expr::Int(Token::Int("0109".into())).eval();

        assert_eq!(five, ObjectType::Int(5));
        assert_eq!(one_o_nine, ObjectType::Int(109));
    }

    #[test]
    fn test_eval_bool_expression() {
        let true_val = Expr::Bool(Token::True).eval();
        let false_val = Expr::Bool(Token::False).eval();

        assert_eq!(true_val, TRUE_OBJ);
        assert_eq!(false_val, FALSE_OBJ);
    }

    #[test]
    fn test_eval_expr_statement() {
        let five = Box::new(Expr::Int(Token::Int("5".into())));
        let expr_stmt = Statement::ExprStatement(five);

        assert_eq!(expr_stmt.eval(), ObjectType::Int(5));
    }

    #[test]
    fn test_bang_operator() {
        let not_true = Expr::Prefix(Token::Bang, Box::new(Expr::Bool(Token::True))).eval();
        let not_false = Expr::Prefix(Token::Bang, Box::new(Expr::Bool(Token::False))).eval();
        let not_six = Expr::Prefix(Token::Bang, Box::new(Expr::Int(Token::Int("6".into())))).eval();
        let not_zero = Expr::Prefix(Token::Bang, Box::new(Expr::Int(Token::Int("0".into())))).eval();
        let not_one = Expr::Prefix(Token::Bang, Box::new(Expr::Int(Token::Int("1".into())))).eval();


        assert_eq!(not_true, FALSE_OBJ);
        assert_eq!(not_false, TRUE_OBJ);
        assert_eq!(not_six, FALSE_OBJ);
        assert_eq!(not_zero, FALSE_OBJ);
        assert_eq!(not_one, FALSE_OBJ);
    }

    #[test]
    fn test_minus_prefix_operator() {
        let minus_five = Expr::Prefix(Token::Minus, Box::new(Expr::Int(Token::Int("5".into())))).eval();
        let minus_ten = Expr::Prefix(Token::Minus, Box::new(Expr::Int(Token::Int("10".into())))).eval();
        let minus_minus_five = Expr::Prefix(
            Token::Minus,
            Box::new(Expr::Prefix(Token::Minus, Box::new(Expr::Int(Token::Int("5".into()))))),
        ).eval();

        assert_eq!(minus_five, ObjectType::Int(-5));
        assert_eq!(minus_ten, ObjectType::Int(-10));
        assert_eq!(minus_minus_five, ObjectType::Int(5));
    }

    #[test]
    fn test_infix_operator() {
        let five_plus_five = Expr::Infix(
            Box::new(Expr::Int(Token::Int("5".into()))),
            Token::Plus,
            Box::new(Expr::Int(Token::Int("5".into()))),
        ).eval();
        let five_minus_five = Expr::Infix(
            Box::new(Expr::Int(Token::Int("5".into()))),
            Token::Minus,
            Box::new(Expr::Int(Token::Int("5".into()))),
        ).eval();
        let five_times_five = Expr::Infix(
            Box::new(Expr::Int(Token::Int("5".into()))),
            Token::Asterisk,
            Box::new(Expr::Int(Token::Int("5".into()))),
        ).eval();
        let five_divided_by_five = Expr::Infix(
            Box::new(Expr::Int(Token::Int("5".into()))),
            Token::Slash,
            Box::new(Expr::Int(Token::Int("5".into()))),
        ).eval();
        let five_eq_five = Expr::Infix(
            Box::new(Expr::Int(Token::Int("5".into()))),
            Token::Eq,
            Box::new(Expr::Int(Token::Int("5".into()))),
        ).eval();
        let five_eq_six = Expr::Infix(
            Box::new(Expr::Int(Token::Int("5".into()))),
            Token::Eq,
            Box::new(Expr::Int(Token::Int("6".into()))),
        ).eval();
        let five_neq_six = Expr::Infix(
            Box::new(Expr::Int(Token::Int("5".into()))),
            Token::NotEq,
            Box::new(Expr::Int(Token::Int("6".into()))),
        ).eval();
        let five_neq_five = Expr::Infix(
            Box::new(Expr::Int(Token::Int("5".into()))),
            Token::NotEq,
            Box::new(Expr::Int(Token::Int("5".into()))),
        ).eval();

        assert_eq!(five_plus_five, ObjectType::Int(10));
        assert_eq!(five_minus_five, ObjectType::Int(0));
        assert_eq!(five_times_five, ObjectType::Int(25));
        assert_eq!(five_divided_by_five, ObjectType::Int(1));
        assert_eq!(five_eq_five, TRUE_OBJ);
        assert_eq!(five_eq_six, FALSE_OBJ);
        assert_eq!(five_neq_six, TRUE_OBJ);
        assert_eq!(five_neq_five, FALSE_OBJ);
    }
}
