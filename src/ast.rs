use crate::token::Token;

pub trait Node {
    // this method is needed for debugging
    fn token(&self) -> Token;
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

#[derive(PartialEq, Debug)]
pub struct Identifier {
    pub token: Token,
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
}

impl Node for Program {
    fn token(&self) -> Token {
        if self.statements.len() > 0 {
            return self.statements[0].token();
        } else {
            return Token::Eof;
        }
    }
}
