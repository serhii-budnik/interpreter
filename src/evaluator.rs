use std::cell::RefCell;
use std::rc::Rc;
use crate::ast::{Expr, Program, Statement, ChildrenStatements};
use crate::environment::Environment;
use crate::object::{ObjectType, FALSE_OBJ, NULL_OBJ, TRUE_OBJ};
use crate::token::Token;

pub trait Evaluator {
    // maybe change it to take pointer to self
    fn eval(self, environment: Rc<RefCell<Environment>>) -> ObjectType;
}

impl Evaluator for Expr {
    fn eval(self, environment: Rc<RefCell<Environment>>) -> ObjectType {
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
                        let left_val = left.eval(Rc::clone(&environment));
                        let right_val = right.eval(Rc::clone(&environment));

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
                        let left_val = left.eval(Rc::clone(&environment));
                        let right_val = right.eval(Rc::clone(&environment));

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
                let cond_value = condition.eval(Rc::clone(&environment));

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

                match (*environment).borrow().get(&ident_val) {
                    Some(val) => val.clone(),
                    None => ObjectType::new_error(format!("identifier not found: {}", &ident_val)),
                }
            },
            Self::Fn(params, body) => {
                ObjectType::Function(params, body, Rc::clone(&environment))
            },
            Self::Call(func, args) => {
                let func_def = func.eval(environment.clone());

                match func_def {
                    ObjectType::Function(params, body, env) => {
                        let extended_env = Rc::new(RefCell::new(Environment::extend_env(env.clone())));

                        if let Err(obj_type) = map_fn_env_params(params, args, extended_env.clone()) {
                            return obj_type;
                        }

                        body.eval(extended_env)
                    },
                    ObjectType::Error(_) => func_def,
                    ob_type => panic!("expected ObjectType::Function, got {:?}", ob_type),
                }
            },
        }
    }
}

impl Evaluator for Statement {
    fn eval(self, environment: Rc<RefCell<Environment>>) -> ObjectType {
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
                        let object_res = expr.eval(Rc::clone(&environment));

                        if let ObjectType::Error(_) = object_res {
                            return object_res;
                        }

                        (*environment).borrow_mut().set(ident_val, object_res);
                    },
                    None => {
                        (*environment).borrow_mut().set(ident_val, NULL_OBJ);
                    },
                };

                NULL_OBJ
            },
            _ => panic!("given Statement {} is not implemented yet", self),
        }
    }
}

impl Evaluator for Program {
    fn eval(self, environment: Rc<RefCell<Environment>>) -> ObjectType {
        eval_statements(self.children(), environment)
    }
}

fn eval_statements(statements: Vec<Box<Statement>>, environment: Rc<RefCell<Environment>>) -> ObjectType {
    let mut result = NULL_OBJ;

    for stmt in statements {
        result = stmt.eval(Rc::clone(&environment));

        if let ObjectType::Error(_) = result {
            return result;
        }
    }

    result
}

fn map_fn_env_params(mut params: Vec<Box<Expr>>, mut args: Vec<Box<Expr>>, env: Rc<RefCell<Environment>>)
-> Result<(), ObjectType>
{
    if params.len() != args.len() {
        return Err(ObjectType::new_error(format!(
            "wrong number of arguments: expected {}, got {}",
            params.len(),
            args.len()
        )));
    }

    for _ in 0..params.len() {
        let param = params.swap_remove(0);
        let arg = args.swap_remove(0);

        let param_name = match *param {
            Expr::Ident(token) => token.value(),
            expr => panic!("expected to be Expr::Ident got {}", expr),
        };

        let arg_v = arg.eval(env.clone());
        (*env).borrow_mut().set(param_name, arg_v);
    }

    Ok(())
}

#[cfg(test)]
mod test {
    use std::rc::Rc;
    use std::cell::RefCell;
    use super::Evaluator;
    use crate::object::{ObjectType, TRUE_OBJ, FALSE_OBJ};
    use crate::token::Token;
    use crate::ast::{Expr, Statement};
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::environment::Environment;

    #[test]
    fn test_eval_integer_expression() {
        let environment = Rc::new(RefCell::new(Environment::new()));
        let five = Expr::Int(Token::Int("5".into())).eval(environment.clone());
        let one_o_nine = Expr::Int(Token::Int("0109".into())).eval(environment.clone());

        assert_eq!(five, ObjectType::Int(5));
        assert_eq!(one_o_nine, ObjectType::Int(109));
    }

    #[test]
    fn test_eval_bool_expression() {
        let environment = Rc::new(RefCell::new(Environment::new()));
        let true_val = Expr::Bool(Token::True).eval(environment.clone());
        let false_val = Expr::Bool(Token::False).eval(environment.clone());

        assert_eq!(true_val, TRUE_OBJ);
        assert_eq!(false_val, FALSE_OBJ);
    }

    #[test]
    fn test_eval_expr_statement() {
        let five = Box::new(Expr::Int(Token::Int("5".into())));
        let expr_stmt = Statement::ExprStatement(five);
        let environment = Rc::new(RefCell::new(Environment::new()));

        assert_eq!(expr_stmt.eval(environment.clone()), ObjectType::Int(5));
    }

    #[test]
    fn test_bang_operator() {
        let environment = Rc::new(RefCell::new(Environment::new()));
        let examples = [
            ("!true", FALSE_OBJ),
            ("!false", TRUE_OBJ),
            ("!6", FALSE_OBJ),
            ("!0", FALSE_OBJ),
            ("!1", FALSE_OBJ),
        ];

        for (input, expected) in examples.iter() {
            let result = Parser::new(Lexer::new(input)).parse_program().eval(environment.clone());

            assert_eq!(result, *expected);
        }
    }

    #[test]
    fn test_minus_prefix_operator() {
        let environment = Rc::new(RefCell::new(Environment::new()));
        let examples = [
            ("5", ObjectType::Int(5)),
            ("-5", ObjectType::Int(-5)),
            ("-10", ObjectType::Int(-10)),
            ("--5", ObjectType::Int(5)),
        ];

        for (input, expected) in examples.iter() {
            let result = Parser::new(Lexer::new(input)).parse_program().eval(environment.clone());

            assert_eq!(result, *expected);
        }
    }

    #[test]
    fn test_infix_operator() {
        let environment = Rc::new(RefCell::new(Environment::new()));
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
            let result = Parser::new(Lexer::new(input)).parse_program().eval(environment.clone());

            assert_eq!(result, *expected);
        }
    }

    #[test]
    fn test_if_statements() {
        let environment = Rc::new(RefCell::new(Environment::new()));
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
            let result = Parser::new(Lexer::new(input)).parse_program().eval(environment.clone());

            assert_eq!(result, *expected);
        }
    }

    #[test]
    fn test_error_handling() {
        let environment = Rc::new(RefCell::new(Environment::new()));
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
            let result = Parser::new(Lexer::new(input)).parse_program().eval(environment.clone());

            assert_eq!(result, ObjectType::Error(expected.to_string()));
        }
    }

    #[test]
    fn test_let_statements() {
        let environment = Rc::new(RefCell::new(Environment::new()));
        let examples = [
            ("let a = 5; a;", ObjectType::Int(5)),
            ("let a = 5 * 5; a;", ObjectType::Int(25)),
            ("let a = 5; let b = a; b;", ObjectType::Int(5)),
            ("let a = 5; let b = a; let c = a + b + 5; c;", ObjectType::Int(15)),
        ];

        for (input, expected) in examples.iter() {
            let result = Parser::new(Lexer::new(input)).parse_program().eval(environment.clone());

            assert_eq!(result, *expected);
        }
    }

    #[test]
    fn test_fn_expression() {
        let environment = Rc::new(RefCell::new(Environment::new()));
        let examples = [
            ("let a = fn () { 1 + 1 }; a();", ObjectType::Int(2)),
            ("let a = fn (a, b) { a + b }; a(5, 10);", ObjectType::Int(15)),
            ("let a = fn () { 1 + 1 }; let b = fn () { a() + 1 }; b();", ObjectType::Int(3)),
            ("let a = fn () { 1 + 1 }; let b = fn (fun) { fun() + 5 }; b(a);", ObjectType::Int(7)),
            ("let newAdder = fn(x) { fn(y) { x + y }; }; let addTwo = newAdder(2); addTwo(2); ", ObjectType::Int(4)),
        ];

        for (input, expected) in examples.iter() {
            let result = Parser::new(Lexer::new(input)).parse_program().eval(environment.clone());

            assert_eq!(result, *expected);
        }
    }
}
