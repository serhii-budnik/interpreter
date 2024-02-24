use crate::ast::{Expr, Program, Statement, ChildrenStatements};
use crate::environment::Environment;
use crate::object::{ObjectType, FALSE_OBJ, NULL_OBJ, TRUE_OBJ};
use crate::token::Token;

pub trait Evaluator {
    // maybe change it to take pointer to self
    fn eval(self, environment: &mut Environment) -> ObjectType;
}

impl Evaluator for Expr {
    fn eval(self, environment: &mut Environment) -> ObjectType {
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
                        if right.eval(environment).is_truthy() { FALSE_OBJ } else { TRUE_OBJ }
                    },
                    Token::Minus => {
                        match right.eval(environment) {
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
                        let left_val = left.eval(environment);
                        let right_val = right.eval(environment);

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
                        let left_val = left.eval(environment);
                        let right_val = right.eval(environment);

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
                let cond_value = condition.eval(environment);

                if let ObjectType::Error(_) = cond_value {
                    return cond_value;
                }

                if cond_value.is_truthy() {
                    consequence.eval(environment)
                } else {
                    if let Some(alt) = alternative {
                        return alt.eval(environment);
                    }

                    NULL_OBJ
                }
            },
            Self::Ident(token) => {
                let ident_val = token.value();

                match environment.get(&ident_val) {
                    Some(val) => val.clone(),
                    None => ObjectType::new_error(format!("identifier not found: {}", &ident_val)),
                }
            },
            expr => ObjectType::new_error(format!("eval is not impemented for {}", expr)),
        }
    }
}

impl Evaluator for Statement {
    fn eval(self, environment: &mut Environment) -> ObjectType {
        match self {
            Statement::ExprStatement(expr) => expr.eval(environment),
            Statement::Block(stmts) => eval_statements(stmts, environment),
            Self::Let(ident, expr) => {
                let ident_val = match *ident {
                    Expr::Ident(token) => token.value(),
                    expr => panic!("expected to be Expr::Ident got {}", expr),
                };

                match expr {
                    Some(expr) => {
                        let object_res = expr.eval(environment);

                        if let ObjectType::Error(_) = object_res {
                            return object_res;
                        }

                        environment.set(ident_val, object_res);
                    },
                    None => {
                        environment.set(ident_val, NULL_OBJ);
                    },
                };

                NULL_OBJ
            },
            _ => panic!("given Statement {} is not implemented yet", self),
        }
    }
}

impl Evaluator for Program {
    fn eval(self, environment: &mut Environment) -> ObjectType {
        eval_statements(self.children(), environment)
    }
}

fn eval_statements(statements: Vec<Box<Statement>>, environment: &mut Environment) -> ObjectType
{
    let mut result = NULL_OBJ;

    for stmt in statements {
        result = stmt.eval(environment);

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
    use crate::environment::Environment;

    #[test]
    fn test_eval_integer_expression() {
        let environment = &mut Environment::new();
        let five = Expr::Int(Token::Int("5".into())).eval(environment);
        let one_o_nine = Expr::Int(Token::Int("0109".into())).eval(environment);

        assert_eq!(five, ObjectType::Int(5));
        assert_eq!(one_o_nine, ObjectType::Int(109));
    }

    #[test]
    fn test_eval_bool_expression() {
        let environment = &mut Environment::new();
        let true_val = Expr::Bool(Token::True).eval(environment);
        let false_val = Expr::Bool(Token::False).eval(environment);

        assert_eq!(true_val, TRUE_OBJ);
        assert_eq!(false_val, FALSE_OBJ);
    }

    #[test]
    fn test_eval_expr_statement() {
        let five = Box::new(Expr::Int(Token::Int("5".into())));
        let expr_stmt = Statement::ExprStatement(five);
        let environment = &mut Environment::new();

        assert_eq!(expr_stmt.eval(environment), ObjectType::Int(5));
    }

    #[test]
    fn test_bang_operator() {
        let environment = &mut Environment::new();
        let examples = [
            ("!true", FALSE_OBJ),
            ("!false", TRUE_OBJ),
            ("!6", FALSE_OBJ),
            ("!0", FALSE_OBJ),
            ("!1", FALSE_OBJ),
        ];

        for (input, expected) in examples.iter() {
            let result = Parser::new(Lexer::new(input)).parse_program().eval(environment);

            assert_eq!(result, *expected);
        }
    }

    #[test]
    fn test_minus_prefix_operator() {
        let environment = &mut Environment::new();
        let examples = [
            ("5", ObjectType::Int(5)),
            ("-5", ObjectType::Int(-5)),
            ("-10", ObjectType::Int(-10)),
            ("--5", ObjectType::Int(5)),
        ];

        for (input, expected) in examples.iter() {
            let result = Parser::new(Lexer::new(input)).parse_program().eval(environment);

            assert_eq!(result, *expected);
        }
    }

    #[test]
    fn test_infix_operator() {
        let environment = &mut Environment::new();
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
            let result = Parser::new(Lexer::new(input)).parse_program().eval(environment);

            assert_eq!(result, *expected);
        }
    }

    #[test]
    fn test_if_statements() {
        let environment = &mut Environment::new();
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
            let result = Parser::new(Lexer::new(input)).parse_program().eval(environment);

            assert_eq!(result, *expected);
        }
    }

    #[test]
    fn test_error_handling() {
        let environment = &mut Environment::new();
        let examples = [
            ("5 + true", "unknown operator: INTEGER + BOOLEAN"),
            ("5 + true; 5", "unknown operator: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; false + true; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            ("if (10 > 1) { true + false; }", "unknown operator: BOOLEAN + BOOLEAN"),
            ("if (-false) { 1 } else { 2 };", "unknown operator: -BOOLEAN"),
            ("foobar;", "identifier not found: foobar"),
        ];

        for (input, expected) in examples.iter() {
            let result = Parser::new(Lexer::new(input)).parse_program().eval(environment);

            assert_eq!(result, ObjectType::Error(expected.to_string()));
        }
    }

    #[test]
    fn test_let_statements() {
        let environment = &mut Environment::new();
        let examples = [
            ("let a = 5; a;", ObjectType::Int(5)),
            ("let a = 5 * 5; a;", ObjectType::Int(25)),
            ("let a = 5; let b = a; b;", ObjectType::Int(5)),
            ("let a = 5; let b = a; let c = a + b + 5; c;", ObjectType::Int(15)),
        ];

        for (input, expected) in examples.iter() {
            let result = Parser::new(Lexer::new(input)).parse_program().eval(environment);

            assert_eq!(result, *expected);
        }
    }
}
