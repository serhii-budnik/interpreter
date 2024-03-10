use crate::ast::{Expr, Program, Statement};
use crate::builtins::BUILTIN_FNS;
use crate::environment::Environment;
use crate::object::{ObjectType, FALSE_OBJ, NULL_OBJ, TRUE_OBJ};
use crate::token::Token;
use std::cell::RefCell;
use std::rc::Rc;

pub trait Evaluator {
    // TODO: check if instead of using ObjectType::Error we can use Result<ObjType, String>
    fn eval(&self, environment: Rc<RefCell<Environment>>) -> ObjectType;
}

impl Evaluator for Expr {
    fn eval(&self, environment: Rc<RefCell<Environment>>) -> ObjectType {
        match self {
            Self::Int(token) => {
                let int_val: isize = token.as_ref().parse().unwrap();

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
                            obj => ObjectType::new_error(format!("unknown operator: -{}", obj.type_name())),
                        }
                    }
                    op => panic!("eval for prefix operator not implemented {}", op),
                }
            },
            Self::Infix(left, op, right) => {
                match op {
                    Token::Plus | Token::Minus | Token::Asterisk | Token::Slash | Token::LessThen |
                    Token::GreaterThen | Token::Eq | Token::NotEq => {
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
                                    Token::Eq => ObjectType::Bool(l == r),
                                    Token::NotEq => ObjectType::Bool(l != r),
                                    _ => panic!("expected infix operator, got {:?}", op),
                                }
                            },
                            (ObjectType::Bool(l), ObjectType::Bool(r)) => {
                                match op {
                                    Token::Eq => ObjectType::Bool(l == r),
                                    Token::NotEq => ObjectType::Bool(l != r),
                                    _ => ObjectType::new_error(format!("unknown operator: BOOLEAN {} BOOLEAN", op)),
                                }
                            },
                            (ObjectType::OString(l), ObjectType::OString(r)) => {
                                    match op {
                                        Token::Plus => {
                                            ObjectType::OString(
                                                Rc::new(RefCell::new(format!("{}{}", l.borrow(), r.borrow()))),
                                            )
                                        },
                                        _ => ObjectType::new_error(
                                            format!("unknown operator: STRING {} STRING", op)
                                        ),
                                    }
                                }
                            (l, r) => ObjectType::new_error(
                                format!("unknown operator: {} {} {}", l.type_name(), op, r.type_name())
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
            Self::Ident(_) => {
                match (*environment).borrow().get(self) {
                    Some(val) => val.clone(),
                    None => ObjectType::new_error(format!("identifier not found: {}", self.token().as_ref())),
                }
            },
            Self::EString(str) => {
                ObjectType::OString(Rc::new(RefCell::new(str.as_ref().to_string())))
            },
            Self::Array(elements) => {
                let mut array_of_objects: Vec<ObjectType> = Vec::with_capacity(elements.len());

                for el in elements {
                    let object = el.eval(environment.clone());
                    array_of_objects.push(object);
                };

                ObjectType::Array(Rc::new(array_of_objects))
            },
            Self::IndexExpr(left, index) => {
                let array_object = left.eval(environment.clone());
                let index = index.eval(environment.clone());

                match array_object {
                    ObjectType::Array(array) => {
                        if let ObjectType::Int(index) = index {
                            let max_index = array.len() as isize - 1;

                            if index < 0 || index > max_index {
                                return ObjectType::new_error(format!(
                                    "index out of bounds: index={}, max_index={}",
                                    index,
                                    max_index
                                ));
                            }

                            return array[index as usize].clone();
                        };

                        ObjectType::new_error(format!("received {} as index, expected Integer", index.type_name()))
                    },
                    ObjectType::Error(_) => array_object,
                    obj => panic!("received {} Object, Array expected", obj.type_name()),
                }
            },
            Self::Fn(params, body) => {
                let params: Vec<Rc<Expr>> = params.iter().map(|param| param.clone()).collect();
                ObjectType::new_function(params, body.clone(), Rc::clone(&environment))
            },
            Self::Call(func_name, args) => {
                let func_definition = func_name.eval(environment.clone());
                let args = args.into_iter().map(|arg| arg.eval(environment.clone())).collect();

                match func_definition {
                    ObjectType::Function(func) => {
                        let extended_env = Rc::new(RefCell::new(Environment::extend_env(func.2.clone())));

                        if let Err(obj_type) = map_fn_env_params(&func.0, args, extended_env.clone()) {
                            return obj_type;
                        }

                        func.1.clone().eval(extended_env)
                    },
                    ObjectType::Error(err) => {
                        let fn_name: &str = func_name.as_ref();
                        let builtin_fn = BUILTIN_FNS.get(fn_name);

                        match builtin_fn {
                            Some(builtin_fn) => builtin_fn(args),
                            None => ObjectType::new_error(err),
                        }
                    },
                    ob_type => panic!("expected ObjectType::Function, got {:?}", ob_type),
                }
            },
        }
    }
}

impl Evaluator for Statement {
    fn eval(&self, environment: Rc<RefCell<Environment>>) -> ObjectType {
        match self {
            Statement::ExprStatement(expr) => expr.eval(environment),
            Statement::Block(stmts) => eval_statements(stmts, environment),
            Self::Let(ident, expr) => {
                match expr {
                    Some(expr) => {
                        let object_res = expr.eval(Rc::clone(&environment));

                        if let ObjectType::Error(_) = object_res {
                            return object_res;
                        }

                        (*environment).borrow_mut().set(ident.clone(), object_res);
                    },
                    None => {
                        (*environment).borrow_mut().set(ident.clone(), NULL_OBJ);
                    },
                };

                NULL_OBJ
            },
            _ => panic!("given Statement {} is not implemented yet", self),
        }
    }
}

impl Evaluator for Program {
    fn eval(&self, environment: Rc<RefCell<Environment>>) -> ObjectType {
        eval_statements(self.statements(), environment)
    }
}

fn eval_statements(statements: &Vec<Box<Statement>>, environment: Rc<RefCell<Environment>>) -> ObjectType {
    let mut result = NULL_OBJ;

    for stmt in statements {
        result = stmt.eval(Rc::clone(&environment));

        if let ObjectType::Error(_) = result {
            return result;
        }
    }

    result
}

fn map_fn_env_params(params: &Vec<Rc<Expr>>, args: Vec<ObjectType>, env: Rc<RefCell<Environment>>)
-> Result<(), ObjectType>
{
    if params.len() != args.len() {
        return Err(ObjectType::new_error(format!(
            "wrong number of arguments: expected {}, got {}",
            params.len(),
            args.len()
        )));
    }

    for (index, arg) in args.into_iter().enumerate() {
        let param = params.get(index).unwrap();

        if let ObjectType::Error(_) = arg {
            return Err(arg.clone());
        }

        env.borrow_mut().set(param.clone(), arg);
    };

    Ok(())
}

#[cfg(test)]
mod test {
    use std::rc::Rc;
    use std::cell::RefCell;
    use super::Evaluator;
    use crate::object::{ObjectType, FALSE_OBJ, NULL_OBJ, TRUE_OBJ};
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
            ("\"str1\" + \"str2\"", ObjectType::OString(Rc::new(RefCell::new("str1str2".to_string())))),
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
            ("\"str1\" - \"str2\"", "unknown operator: STRING - STRING"),
            ("-\"str1\"", "unknown operator: -STRING"),
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
            ("
                let add = fn(a, b) { a + b };
                let applyFunc = fn(a, b, func) { func(a, b) };
                applyFunc(2, 2, add);
            ", ObjectType::Int(4)),
        ];

        for (input, expected) in examples.iter() {
            let result = Parser::new(Lexer::new(input)).parse_program().eval(environment.clone());

            assert_eq!(result, *expected);
        }
    }

    #[test]
    fn test_built_in_functions() {
        let environment = Rc::new(RefCell::new(Environment::new()));
        let examples = [
            ("len(\"\")", ObjectType::Int(0)),
            ("len(\"four\")", ObjectType::Int(4)),
            ("len(\"hello world\")", ObjectType::Int(11)),
            ("len(1)", ObjectType::Error("argument to `len` not supported, got INTEGER".to_string())),
            ("len()", ObjectType::Error("got no arguments, expected 1".to_string())),
            ("puts(\"hello\")", NULL_OBJ),
        ];

        for (input, expected) in examples.iter() {
            let result = Parser::new(Lexer::new(input)).parse_program().eval(environment.clone());

            assert_eq!(result, *expected);
        };
    }

    #[test]
    fn test_arrays() {
        let environment = Rc::new(RefCell::new(Environment::new()));
        let examples = [
            ("[1, 2 * 2, 3 + 3]", ObjectType::Array(Rc::new(vec![
                ObjectType::Int(1),
                ObjectType::Int(4),
                ObjectType::Int(6),
            ]))),
            ("[1, false, \"string\",]", ObjectType::Array(Rc::new(vec![
                ObjectType::Int(1),
                FALSE_OBJ,
                ObjectType::OString(Rc::new(RefCell::new("string".to_string()))),
            ]))),
            ("[]", ObjectType::Array(Rc::new(vec![]))),
        ];

        for (input, expected) in examples.iter() {
            let result = Parser::new(Lexer::new(input)).parse_program().eval(environment.clone());

            assert_eq!(result, *expected);
        };
    }

    #[test]
    fn test_index_expressions() {
        let environment = Rc::new(RefCell::new(Environment::new()));
        let examples = [
            ("[1, 2, 3][0]", ObjectType::Int(1)),
            ("[1, 2, 3][1]", ObjectType::Int(2)),
            ("[1, 2, 3][2]", ObjectType::Int(3)),
            ("[1, 2, 3][1 + 1];", ObjectType::Int(3)),
            ("let myArray = [1, 2, 3]; myArray[2];", ObjectType::Int(3)),
            ("let myArray = [1, 2, 3]; myArray[3];", ObjectType::Error("index out of bounds: index=3, max_index=2".to_string())),
            ("let myArray = [1, 2, 3]; myArray[-1];", ObjectType::Error("index out of bounds: index=-1, max_index=2".to_string())),
        ];

        for (input, expected) in examples.iter() {
            let result = Parser::new(Lexer::new(input)).parse_program().eval(environment.clone());

            assert_eq!(result, *expected);
        };
    }
}
