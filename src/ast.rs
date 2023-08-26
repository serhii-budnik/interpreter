use crate::token::Token;

pub trait Node {
    // these methods are needed for debugging
    fn token(&self) -> Token;
    fn to_string(&self) -> String;
}

pub trait Statement: Node {
    fn name(&self) -> &Identifier;
    fn statement_node(&self) -> Box<dyn Statement>;
}

pub trait Expression: Node {
    fn expression_node(&self) -> Box<dyn Expression>;
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

pub struct LetStatement {
    pub name: Identifier,
    pub value: Box<dyn Expression>,
}

pub struct ReturnStatement {
    pub value: Box<dyn Expression>,
}

pub struct ExpressionStatement {
    pub expression: Box<dyn Expression>,
}

#[derive(PartialEq, Debug)]
pub struct Identifier {
    pub token: Token,
    // value just duplicate of token Ident(value). Let's keep it for now, maybe we can delete it later
    pub value: String,
}

impl Expression for Identifier {
    fn expression_node(&self) -> Box<dyn Expression> {
        todo!()
    }
}

impl Node for Identifier {
    fn token(&self) -> Token {
        self.token.clone()
    }

    fn to_string(&self) -> String {
        self.token.to_string()
    }
}

impl Statement for LetStatement {
    fn name(&self) -> &Identifier {
        &self.name
    }

    fn statement_node(&self) -> Box<dyn Statement> {
        todo!()
    }
}

impl Node for LetStatement {
    fn token(&self) -> Token {
       Token::Let
    }

    fn to_string(&self) -> String {
        format!("{} {} = {};", self.token(), self.name.to_string(), self.value.to_string())
    }
}

impl Statement for ReturnStatement {
    fn name(&self) -> &Identifier {
        todo!()
    }

    fn statement_node(&self) -> Box<dyn Statement> {
        todo!()
    }
}

impl Node for ReturnStatement {
    fn token(&self) -> Token {
        Token::Return
    }

    fn to_string(&self) -> String {
        format!("{} {};", self.token(), self.value.to_string())
    }
}

impl Node for Program {
    fn token(&self) -> Token {
        if self.statements.len() > 0 {
            return self.statements[0].token();
        } else {
            return Token::Eof;
        }
    }

    fn to_string(&self) -> String {
        self.statements.iter().map(|statement| statement.to_string()).collect::<Vec<String>>().join("\n")
    }
}

impl Node for ExpressionStatement {
    fn token(&self) -> Token {
        self.expression.token()
    }

    fn to_string(&self) -> String {
        self.expression.to_string()
    }
}

impl Statement for ExpressionStatement {
    fn name(&self) -> &Identifier {
        todo!()
    }

    fn statement_node(&self) -> Box<dyn Statement> {
        todo!()
    }
}

#[cfg(test)]
mod test {
    use crate::token::Token;
    use crate::ast::{Program, LetStatement, Identifier, Node, ReturnStatement };

    #[test]
    fn to_string_converts_let_statement_to_readable_code() {
        let program = Program {
            statements: vec![
                Box::new(LetStatement {
                    name: Identifier {
                        token: Token::Ident("myVar".to_string()),
                        value: "myVar".to_string(),
                    },
                    value: Box::new(Identifier {
                        token: Token::Ident("anotherVar".to_string()),
                        value: "anotherVar".to_string(),
                    }),
                }),
                Box::new(ReturnStatement {
                    value: Box::new(Identifier {
                        token: Token::Ident("myVar".to_string()),
                        value: "myVar".to_string(),
                    }),
                }),
            ],
        };

        assert_eq!(program.to_string(), "let myVar = anotherVar;\nreturn myVar;");
    }
}
