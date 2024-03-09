use std::rc::Rc;
use crate::token::Token;
use std::collections::VecDeque;
use std::fmt::{Display, Debug};

#[derive(Eq, Hash, PartialEq, Clone)]
pub enum Expr {
    Ident(Token),
    Int(Token),
    EString(Token),
    Array(Vec<Box<Expr>>),
    IndexExpr(Box<Expr>, Box<Expr>), // array[5]
    Bool(Token),
    Prefix(Token, Box<Expr>),
    Infix(Box<Expr>, Token, Box<Expr>),
    If(Box<Expr>, Box<Statement>, Option<Box<Statement>>),
    Fn(Vec<Rc<Expr>>, Rc<Statement>), // Statement is a Block
    Call(Box<Expr>, VecDeque<Box<Expr>>),

}

#[derive(Eq, Hash, PartialEq, Clone)]
pub enum Statement {
    Let(Rc<Expr>, Option<Box<Expr>>),
    Return(Box<Expr>),
    ExprStatement(Box<Expr>),
    Block(Vec<Box<Statement>>),
}

pub struct Program {
    statements: Vec<Box<Statement>>,
}

impl Program {
    pub fn new(statements: Vec<Box<Statement>>) -> Program {
        Self { statements }
    }

    pub fn statements(&self) -> &Vec<Box<Statement>> {
        &self.statements
    }
}

impl Expr {
    // usually use it for debugging purposes
    pub fn token(&self) -> Token {
        match self {
            Self::Ident(token) => token.clone(),
            Self::Int(i) => Token::Int(i.to_string()),
            Self::EString(s) => Token::TString(s.to_string()),
            Self::Array(_) => Token::LBracket,
            Self::IndexExpr(_, _) => Token::LBracket,
            Self::Bool(t) => t.clone(),
            Self::Prefix(token, _) => token.clone(),
            Self::Infix(_, token, _) => token.clone(),
            Self::If(_, _, _) => Token::If,
            Self::Fn(_, _) => Token::Function,
            Self::Call(_, _) => Token::Lparen,
        }
    }
}

impl AsRef<str> for Box<Expr> {
    fn as_ref(&self) -> &str {
        let expr: &Expr = self.as_ref();
        expr.as_ref()
    }
}

impl AsRef<str> for Expr {
    fn as_ref(&self) -> &str {
        match self {
            Self::Ident(token) | Self::Int(token) => token.as_ref(),
            _ => panic!("not expected as_ref call on non-ident or non-int token"),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ident(token) => write!(f, "{}", token),
            Self::Int(i) => write!(f, "{}", i),
            Self::EString(s) => write!(f, "\"{}\"", s),
            Self::Array(arrays) => write!(
                f,
                "[{}]",
                arrays.iter().map(|array| array.to_string()).collect::<Vec<String>>().join(", "),
            ),
            Self::IndexExpr(left, index) => write!(f, "{}[{}]", left, index),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Prefix(token, expr) => write!(f, "({}{})", token, expr),
            Self::Infix(left, token, right) => write!(f, "({} {} {})", left, token, right),
            Self::If(condition, consequence, alternative) => {
                if let Some(alternative) = &alternative {
                    write!(
                        f,
                        "if {} {} else {}",
                        &condition,
                        &consequence,
                        &alternative
                    )
                } else {
                    write!(
                        f,
                        "if {} {}",
                        &condition,
                        &consequence
                    )
                }
            },
            Self::Fn(params, body) => write!(f, "fn({}) {}", params.iter().map(|param| param.to_string()).collect::<Vec<String>>().join(", "), body),
            Self::Call(function, args) => write!(f, "{}({})", function, args.iter().map(|arg| arg.to_string()).collect::<Vec<String>>().join(", ")),
        }
    }
}

impl Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ident(token) => write!(f, "{:?}", token),
            Self::Int(i) => write!(f, "{:?}", i),
            Self::EString(s) => write!(f, "{:?}", s),
            Self::Array(arrays) => {
                f.debug_list()
                    .entries(arrays.iter().map(|array| format!("{:#?}", array)))
                    .finish()
            },
            Self::IndexExpr(left, index) => {
                f.debug_struct("IndexExpr")
                    .field("left", &format_args!("{:#?}", left))
                    .field("index", &format_args!("{:#?}", index))
                    .finish()
            },
            Self::Bool(b) => write!(f, "{:?}", b),
            Self::Prefix(token, expr) => {
                f.debug_struct("Prefix")
                    .field("token", &token)
                    .field("expr", &format_args!("{:#?}", expr))
                    .finish()
            },
            Self::Infix(left, token, right) => {
                f.debug_struct("InfixExpr")
                    .field("left", &format_args!("{:#?}", left))
                    .field("token", &token)
                    .field("right", &format_args!("{:#?}", right))
                    .finish()
            },
            Self::If(condition, consequence, alternative) => {
                f.debug_struct("If")
                    .field("condition", &format_args!("{:#?}", condition))
                    .field("consequence", &consequence)
                    .field("alternative", &format_args!("{:#?}", alternative))
                    .finish()
            },
            Self::Fn(params, body) => {
                f.debug_struct("Fn")
                    .field("params", &format_args!("{:#?}", params))
                    .field("body", &body)
                    .finish()
            },
            Self::Call(function, args) => {
                f.debug_struct("Call")
                    .field("function", &format_args!("{:#?}", function))
                    .field("args", &args)
                    .finish()
            },
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Let(name, value) => {
                if let Some(value) = value {
                    write!(f, "{} {} = {};", self.token(), name, value)
                } else {
                    write!(f, "{} {};", self.token(), name)
                }
            },
            Self::Return(value) => write!(f, "{} {};", self.token(), value),
            Self::ExprStatement(expr) => write!(f, "{};", expr),
            Self::Block(statements) => write!(f, "{{\n{}\n}}", statements.iter().map(|statement| statement.to_string()).collect::<Vec<String>>().join("\n")),
        }
    }
}

impl Statement {
    // usually use it for debugging purposes
    pub fn token(&self) -> Token {
        match self {
            Self::Let(_, _) => Token::Let,
            Self::Return(_) => Token::Return,
            Self::ExprStatement(t) => t.token(),
            Self::Block(_) => Token::Lbrace,
        }
    }

    pub fn name(&self) -> &Rc<Expr> {
        match self {
            Self::Let(ident, _) => ident,
            _ => todo!(),
        }
    }
}

impl Debug for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Let(name, value) => {
                f.debug_struct("LetStatement")
                    .field("name", &format_args!("{:#?}", name))
                    .field("value", &format_args!("{:#?}", value))
                    .finish()
            },
            Self::Return(value) => {
                f.debug_struct("ReturnStatement")
                    .field("value", &format_args!("{:#?}", value))
                    .finish()
            },
            Self::ExprStatement(expr) => {
                f.debug_struct("ExprStatement")
                    .field("expression", &format_args!("{:#?}", expr))
                    .finish()
            },
            Self::Block(statements) => {
                f.debug_struct("BlockStatement")
                    .field("token", &self.token())
                    .field("statements", &format_args!("{:#?}", statements))
                    .finish()
            },
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.statements.iter().map(|statement| statement.to_string()).collect::<Vec<String>>().join("\n")
        )
    }
}


impl Debug for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.statements.iter().map(|statement| format!("{:#?}", statement)).collect::<Vec<String>>().join("\n")
        )
    }
}


#[cfg(test)]
mod test {
    use std::rc::Rc;
    use crate::token::Token;
    use super::{
        Program,
        Statement,
        Expr,
    };

    #[test]
    fn to_string_converts_let_statement_to_readable_code() {
        let program = Program {
            statements: vec![
                Box::new(Statement::Let(
                    Rc::new(Expr::Ident(Token::Ident("myVar".to_string()))),
                    Some(Box::new(Expr::Ident(
                        Token::Ident("anotherVar".to_string()),
                    ))),
                )),
                Box::new(Statement::Return(
                    Box::new(Expr::Ident(Token::Ident("myVar".to_string()))),
                )),
            ],
        };

        assert_eq!(program.to_string(), "let myVar = anotherVar;\nreturn myVar;");
    }

    #[test]
    fn to_string_converts_prefix_expression_to_readable_code() {
        let program = Program {
            statements: vec![
                Box::new(Statement::ExprStatement(
                    Box::new(Expr::Prefix(
                        Token::Bang,
                        Box::new(Expr::Ident (
                            Token::Ident("myVar".to_string()),
                        )),
                    )),
                )),
                Box::new(Statement::ExprStatement(
                    Box::new(Expr::Prefix(
                        Token::Minus,
                        Box::new(Expr::Int(
                            Token::Int("4".to_string()),
                        )),
                    )),
                )),
            ]
        };

        assert_eq!(program.to_string(), "(!myVar);\n(-4);");
    }

    #[test]
    fn to_string_converts_infix_expression_to_readable_code() {
        let program = Program {
            statements: vec![
                Box::new(Statement::ExprStatement(
                    Box::new(Expr::Infix(
                        Box::new(Expr::Int(
                            Token::Int("4".to_string()),
                        )),
                        Token::Plus,
                        Box::new(Expr::Ident(
                            Token::Int("x".to_string()),
                        )),
                    )),
                )),
            ]
        };

        assert_eq!(program.to_string(), "(4 + x);");
    }

    #[test]
    fn to_string_converts_fn_expression_to_readable_code() {
        let program = Expr::Fn(
            vec![
                Rc::new(Expr::Ident(
                    Token::Ident("x".to_string()),
                )),
                Rc::new(Expr::Ident(
                    Token::Ident("y".to_string()),
                )),
            ],
            Rc::new(Statement::Block(
                vec![
                    Box::new(Statement::ExprStatement(
                        Box::new(Expr::Infix(
                            Box::new(Expr::Ident(
                                Token::Ident("x".to_string()),
                            )),
                            Token::Plus,
                            Box::new(Expr::Ident(
                                Token::Ident("y".to_string()),
                            )),
                        )),
                    )),
                ],
            )),
        );

        assert_eq!(program.to_string(), "fn(x, y) {\n(x + y);\n}");
    }
}
