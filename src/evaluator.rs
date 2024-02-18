use crate::ast::{Expr, Program, Statement, ChildrenStatements};
use crate::object::{ObjectType, FALSE_OBJ, NULL_OBJ, TRUE_OBJ};
use crate::token::Token;

pub trait Evaluator {
    fn eval(self) -> ObjectType;
}

impl Evaluator for Expr {
    fn eval(self) -> ObjectType {
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
                            ObjectType::Bool(_) => ObjectType::new_error("unknown operator: -BOOLEAN".to_string()),
                            obj => panic!("expected Expr::Int, got {:?}", obj),
                        }
                    }
                    op => panic!("eval for prefix operator not implemented {}", op),
                }
            },
            Self::Infix(left, op, right) => {
                match op {
                    Token::Plus | Token::Minus | Token::Asterisk | Token::Slash | Token::LessThen |
                    Token::GreaterThen => {
                        let left_val = left.eval();
                        let right_val = right.eval();

                        match (left_val, right_val) {
                            (ObjectType::Int(l), ObjectType::Int(r)) => {
                                match op {
                                    Token::Plus => ObjectType::Int(l + r),
                                    Token::Minus => ObjectType::Int(l - r),
                                    Token::Asterisk => ObjectType::Int(l * r),
                                    Token::Slash => ObjectType::Int(l / r),
                                    Token::LessThen => ObjectType::Bool(l < r),
                                    Token::GreaterThen => ObjectType::Bool(l > r),
                                    _ => panic!("expected infix operator, got {:?}", op),
                                }
                            },
                            (l, r) => ObjectType::new_error(
                                format!("unknown operator: {} {} {}", l.type_name(), op, r.type_name())
                            ),
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
                            (l, r) => ObjectType::new_error(
                                format!("type mismatch: {} {} {}", l.type_name(), op, r.type_name())
                            ),
                        }
                    },
                    op => panic!("eval for infix operator ({}) not implemented", op),
                }
            },
            Self::If(condition, consequence, alternative) => {
                let cond_value = condition.eval();

                if let ObjectType::Error(_) = cond_value {
                    return cond_value;
                }

                if cond_value.is_truthy() {
                    consequence.eval()
                } else {
                    if let Some(alt) = alternative {
                        return alt.eval();
                    }

                    NULL_OBJ
                }
            },
            expr => ObjectType::new_error(format!("eval is not impemented for {}", expr)),
        }
    }
}

impl Evaluator for Statement {
    fn eval(self) -> ObjectType {
        match self {
            Statement::ExprStatement(expr) => expr.eval(),
            Statement::Block(stmts) => eval_statements(stmts),
            _ => panic!("given Statement {} is not implemented yet", self),
        }
    }
}

impl Evaluator for Program {
    fn eval(self) -> ObjectType {
        eval_statements(self.children())
    }
}

fn eval_statements(statements: Vec<Box<Statement>>) -> ObjectType
{
    let mut result = NULL_OBJ;

    for stmt in statements {
        result = stmt.eval();

        if let ObjectType::Error(_) = result {
            return result;
        }
    }

    result
}

#[cfg(test)]
mod test {
    use super::Evaluator;
    use crate::object::{ObjectType, TRUE_OBJ, FALSE_OBJ};
    use crate::token::Token;
    use crate::ast::{Expr, Statement};
    use crate::lexer::Lexer;
    use crate::parser::Parser;

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
        let examples = [
            ("!true", FALSE_OBJ),
            ("!false", TRUE_OBJ),
            ("!6", FALSE_OBJ),
            ("!0", FALSE_OBJ),
            ("!1", FALSE_OBJ),
        ];

        for (input, expected) in examples.iter() {
            let result = Parser::new(Lexer::new(input)).parse_program().eval();

            assert_eq!(result, *expected);
        }
    }

    #[test]
    fn test_minus_prefix_operator() {
        let examples = [
            ("5", ObjectType::Int(5)),
            ("-5", ObjectType::Int(-5)),
            ("-10", ObjectType::Int(-10)),
            ("--5", ObjectType::Int(5)),
        ];

        for (input, expected) in examples.iter() {
            let result = Parser::new(Lexer::new(input)).parse_program().eval();

            assert_eq!(result, *expected);
        }
    }

    #[test]
    fn test_infix_operator() {
        let examples = [
            ("5 + 5", ObjectType::Int(10)),
            ("5 - 5", ObjectType::Int(0)),
            ("5 * 5", ObjectType::Int(25)),
            ("5 / 5", ObjectType::Int(1)),
            ("5 == 5", TRUE_OBJ),
            ("5 == 6", FALSE_OBJ),
            ("5 != 6", TRUE_OBJ),
            ("5 != 5", FALSE_OBJ),
            ("5 > 6", FALSE_OBJ),
            ("5 < 6", TRUE_OBJ),
        ];

        for (input, expected) in examples.iter() {
            let result = Parser::new(Lexer::new(input)).parse_program().eval();

            assert_eq!(result, *expected);
        }
    }

    #[test]
    fn test_if_statements() {
        let examples = [
            ("if (true) { 10 }", ObjectType::Int(10)),
            ("if (false) { 10 }", ObjectType::Null),
            ("if (1) { 10 }", ObjectType::Int(10)),
            ("if (1 < 2) { 10 }", ObjectType::Int(10)),
            ("if (1 > 2) { 10 }", ObjectType::Null),
            ("if (1 > 2) { 10 } else { 20 }", ObjectType::Int(20)),
            ("if (1 < 2) { 10 } else { 20 }", ObjectType::Int(10)),
        ];

        for (input, expected) in examples.iter() {
            let result = Parser::new(Lexer::new(input)).parse_program().eval();

            assert_eq!(result, *expected);
        }
    }

    #[test]
    fn test_error_handling() {
        let examples = [
            ("5 + true", "unknown operator: INTEGER + BOOLEAN"),
            ("5 + true; 5", "unknown operator: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; false + true; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            ("if (10 > 1) { true + false; }", "unknown operator: BOOLEAN + BOOLEAN"),
            ("if (-false) { 1 } else { 2 };", "unknown operator: -BOOLEAN"),
        ];

        for (input, expected) in examples.iter() {
            let result = Parser::new(Lexer::new(input)).parse_program().eval();

            assert_eq!(result, ObjectType::Error(expected.to_string()));
        }
    }
}
