use crate::token::Token;
use std::fmt::{Display, Debug};

pub trait Node: Display + Debug {
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

pub struct ReturnStatement {
    pub value: Box<dyn Expression>,
}

pub struct ExpressionStatement {
    pub expression: Box<dyn Expression>,
}

#[derive(Debug)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

#[derive(Debug)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<dyn Expression>,
}

pub struct InfixExpression {
    pub token: Token,
    pub operator: String,

    pub left: Box<dyn Expression>,
    pub right: Box<dyn Expression>,
}

pub struct IfExpression {
    pub token: Token,
    pub condition: Box<dyn Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Box<dyn Statement>>,
}

#[derive(Debug)]
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
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token)
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

impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} = {};", self.token(), self.name, self.value)
    }
}

impl Debug for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LetStatement")
            .field("name", &format_args!("{:#?}", self.name))
            .field("value", &format_args!("{:#?}", self.value))
            .finish()
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
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {};", self.token(), self.value)
    }
}


impl Debug for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ReturnStatement")
            .field("value", &format_args!("{:#?}", self.value))
            .finish()
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

impl Node for ExpressionStatement {
    fn token(&self) -> Token {
        self.expression.token()
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

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{};", self.expression)
    }
}

impl Debug for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ExpressionStatement")
            .field("expression", &format_args!("{:#?}", self.expression))
            .finish()
    }
}

impl Node for IntegerLiteral {
    fn token(&self) -> Token {
        self.token.clone()
    }
}

impl Expression for IntegerLiteral {
    fn expression_node(&self) -> Box<dyn Expression> {
        todo!()
    }
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Node for Boolean {
    fn token(&self) -> Token {
        self.token.clone()
    }
}

impl Expression for Boolean {
    fn expression_node(&self) -> Box<dyn Expression> {
        todo!()
    }
}

impl Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token)
    }
}

impl Node for PrefixExpression {
    fn token(&self) -> Token {
        self.token.clone()
    }
}

impl Expression for PrefixExpression {
    fn expression_node(&self) -> Box<dyn Expression> {
        todo!()
    }
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.token.to_string(), self.right)
    }
}

impl Debug for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("PrefixExpression")
            .field("token", &self.token)
            .field("operator", &self.operator)
            .field("right", &self.right)
            .finish()
    }
}

impl Node for InfixExpression {
    fn token(&self) -> Token {
        self.token.clone()
    }
}

impl Expression for InfixExpression {
    fn expression_node(&self) -> Box<dyn Expression> {
        todo!()
    }
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.token.to_string(), self.right)
    }
}

impl Debug for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("InfixExpression")
            .field("token", &self.token)
            .field("operator", &self.operator)
            .field("left", &format_args!("{:#?}", self.left))
            .field("right", &format_args!("{:#?}", self.right))
            .finish()
    }
}

impl Node for IfExpression {
    fn token(&self) -> Token {
        self.token.clone()
    }
}

impl Expression for IfExpression {
    fn expression_node(&self) -> Box<dyn Expression> {
        todo!()
    }
}

impl Debug for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("IfExpression")
            .field("token", &self.token)
            .field("condition", &format_args!("{:#?}", self.condition))
            .finish()
    }
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(alternative) = &self.alternative {
            write!(
                f,
                "if {} {} else {}",
                &self.condition,
                &self.consequence,
                &alternative
            )
        } else {
            write!(
                f,
                "if {} {}",
                &self.condition,
                &self.consequence
            )
        }
    }
}

impl Node for BlockStatement {
    fn token(&self) -> Token {
        self.token.clone()
    }
}

impl Debug for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BlockStatement")
            .field("token", &self.token)
            .field("statements", &format_args!("{:#?}", self.statements))
            .finish()
    }
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{\n{}\n}}",
            self.statements.iter().map(|statement| statement.to_string()).collect::<Vec<String>>().join("\n")
        )
    }
}

#[cfg(test)]
mod test {
    use crate::token::Token;
    use super::{
        ExpressionStatement,
        Identifier,
        IntegerLiteral,
        LetStatement,
        PrefixExpression,
        Program,
        ReturnStatement,
        InfixExpression,
    };

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

    #[test]
    fn to_string_converts_prefix_expression_to_readable_code() {
        let program = Program {
            statements: vec![
                Box::new(ExpressionStatement { 
                    expression: Box::new(PrefixExpression {
                        token: Token::Bang,
                        operator: "!".to_string(),
                        right: Box::new(Identifier {
                            token: Token::Ident("myVar".to_string()),
                            value: "myVar".to_string(),
                        }),
                    }),
                }),
                Box::new(ExpressionStatement { 
                    expression: Box::new(PrefixExpression {
                        token: Token::Minus,
                        operator: "-".to_string(),
                        right: Box::new(IntegerLiteral {
                            token: Token::Int("4".to_string()),
                            value: 4,
                        }),
                    }),
                }),
            ]
        };

        assert_eq!(program.to_string(), "(!myVar);\n(-4);");
    }

    #[test]
    fn to_string_converts_infix_expression_to_readable_code() {
        let program = Program {
            statements: vec![
                Box::new(ExpressionStatement {
                    expression: Box::new(InfixExpression {
                        token: Token::Plus,
                        operator: "+".to_string(),
                        left: Box::new(IntegerLiteral {
                            token: Token::Int("4".to_string()),
                            value: 4,
                        }),
                        right: Box::new(Identifier {
                            token: Token::Int("x".to_string()),
                            value: "x".to_string(),
                        }),
                    }),
                })
            ]
        };

        assert_eq!(program.to_string(), "(4 + x);");
    }
}
