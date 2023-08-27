use crate::{
    lexer::Lexer,
    token::Token,
    ast::{
        Expression,
        ExpressionStatement,
        Identifier,
        IntegerLiteral,
        LetStatement,
        PrefixExpression,
        Program,
        ReturnStatement,
        Statement,
    },
};

#[derive(PartialEq, PartialOrd, Debug)]
pub enum Precedence {
    Lowest,
    Equals, // == LESSGREATER // > or <
    Sum, // +
    Product, // *
    Prefix, // -X or !X
    Call, // myFunction(X)
}

#[allow(dead_code)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,

    cur_token: Token,
    peek_token: Token,

    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    #[allow(dead_code)]
    fn new(mut lexer: Lexer<'a>) -> Parser<'a> {
        Self {
            cur_token: lexer.next_token(),
            peek_token: lexer.next_token(),
            lexer,
            errors: Vec::new(),
        }
    }

    #[allow(dead_code)]
    fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    #[allow(dead_code)]
    fn add_error(&mut self, err: String) {
        self.errors.push(err);
    }

    #[allow(dead_code)]
    fn peek_error(&mut self, token: Token) {
        let err = format!("expected next token to be {:?}, got {:?} instead", &token, &self.peek_token);
        self.add_error(err);
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn skip_until_semicolon(&mut self) {
        while self.cur_token != Token::Semicolon { self.next_token() }
    }

    #[allow(dead_code)]
    fn parse_program(&mut self) -> Program {
        let mut statements = Vec::new();

        while self.cur_token != Token::Eof {
            let stmt = self.parse_statement();

            if let Some(st) = stmt { statements.push(st) }

            self.next_token();
        }

        Program { statements }
    }

    fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match self.cur_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Box<dyn Statement>> { 
        match self.peek_token.clone() { 
            Token::Ident(ident) => {
                self.next_token();

                match self.peek_token.clone() { 
                    Token::Assign => {
                        let identifier = Identifier { token: self.cur_token.take(), value: ident };

                        // TODO: We're skipping the expressioin until we encouter a semicolon
                        self.skip_until_semicolon();

                        return Some(Box::new(LetStatement {
                            name: identifier,
                            value: Box::new(Identifier {
                                token: Token::Int("42".into()),
                                value: "42".into(),
                            }),
                        }));
                    },
                    _ => return {
                        self.peek_error(Token::Assign);
                        self.skip_until_semicolon();
                        None
                    },
                }
            },
            _ => {
                self.peek_error(Token::Ident("<variable_name>".to_string()));
                self.skip_until_semicolon();
                None
            },
        }
    }

    #[allow(dead_code)]
    fn parse_return_statement(&mut self) -> Option<Box<dyn Statement>> {
        self.next_token();

        // TODO: We're skipping the expressioin until we encouter a semicolon
        self.skip_until_semicolon();

        Some(Box::new(ReturnStatement {
            value: Box::new(
                Identifier { token: Token::Int("42".into()), value: "42".into() }
            ),
        }))
    }

    #[allow(dead_code)]
    fn parse_expression_statement(&mut self) -> Option<Box<dyn Statement>> {
        let result = self.parse_expression(Precedence::Lowest);

        if let Err(err) = result {
            self.add_error(err);

            return None;
        }

        let expression = result.unwrap();

        let stmt = ExpressionStatement { expression };

        if let Token::Semicolon = self.peek_token {
            self.next_token();
        }

        Some(Box::new(stmt))
    }

    fn parse_expression(&mut self, _precedence: Precedence) -> Result<Box<dyn Expression>, String> {
        self.parse_prefix()
    }

    fn parse_identifier(&mut self) -> Result<Box<dyn Expression>, String> {
        let token = self.cur_token.take();
        Ok(Box::new(Identifier { value: token.to_string(), token }))
    }

    #[allow(dead_code)]
    fn parse_integer_literal(&mut self) -> Result<Box<dyn Expression>, String> {
        let token = self.cur_token.take();
        let res = token.parse_integer();

        if let Err(err) = res {
            self.add_error(err.clone());

            return Err(err);
        }

        let int = res.unwrap();

        Ok(Box::new(
            IntegerLiteral { token, value: int }
        ))
    }

    fn parse_prefix_expression(&mut self) -> Result<Box<dyn Expression>, String> {
        let token = self.cur_token.take();
        let operator = token.to_string();

        self.next_token();

        let result = self.parse_expression(Precedence::Prefix);

        if let Err(err) = result {
            self.add_error(err.clone());

            return Err(err);
        }

        let right = result.unwrap();

        Ok(Box::new(
            PrefixExpression {
                token,
                operator,
                right,
            }
        ))
    }

    pub fn parse_prefix(&mut self) -> Result<Box<dyn Expression>, String> {
        match &self.cur_token {
            Token::Ident(_) => self.parse_identifier(),
            Token::Int(_) => self.parse_integer_literal(),
            Token::Bang | Token::Minus => self.parse_prefix_expression(),
            token => Err(format!("expected tokens to parse prefix are (Ident, ...), got {:?}", token)),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{
        ast::{Identifier, ReturnStatement, Node, LetStatement, Statement},
        lexer::Lexer,
        parser::Parser,
        token::Token,
    };

    fn test_let_statement(test: &LetStatement, statement: &Box<dyn Statement>) -> bool {
        if Token::Let != statement.token() {
            return false;
        }

        test.name().token() == statement.name().token()
    }

    #[test]
    fn test_let_statements() {
        let input = r#"
            let x = 5;
            let y = 10;
            let foobar = 848484;
        "#.trim();

        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(parser.errors().is_empty(), true);
        assert_eq!(program.statements.len(), 3);

        let tests = [
            LetStatement {
                name: Identifier { token: Token::Ident("x".to_string()), value: "x".to_string() },
                value: Box::new(Identifier { token: Token::Int("5".into()), value: "5".into()})
            },
            LetStatement {
                name: Identifier { token: Token::Ident("y".to_string()), value: "y".to_string() },
                value: Box::new(Identifier { token: Token::Int("10".into()), value: "10".into()})
            },
            LetStatement {
                name: Identifier { token: Token::Ident("foobar".to_string()), value: "foobar".to_string() },
                value: Box::new(Identifier { token: Token::Int("848484".into()), value: "848484".into()})
            },
        ];

        for (index, test) in tests.iter().enumerate() {
            assert_eq!(test_let_statement(test, &program.statements[index]), true)
        }
    }

    #[test]
    fn test_return_statements() {
        let input = r#"
            return 5;
            return 10;
            return 993322;
        "#.trim();

        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(parser.errors().is_empty(), true);
        assert_eq!(program.statements.len(), 3);

        let tests = [
            ReturnStatement {
                value: Box::new(Identifier { token: Token::Int("5".into()), value: "5".into()})
            },
            ReturnStatement {
                value: Box::new(Identifier { token: Token::Int("10".into()), value: "10".into()})
            },
            ReturnStatement {
                value: Box::new(Identifier { token: Token::Int("848484".into()), value: "848484".into()})
            },
        ];

        for (index, test) in tests.iter().enumerate() {
            assert_eq!(test.token(), program.statements[index].token());
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(parser.errors.is_empty(), true);

        assert_eq!(program.statements.len(), 1);
        assert_eq!(program.statements[0].token(), Token::Ident("foobar".to_string()));
        assert_eq!(program.statements[0].to_string(), "foobar");
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";

        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(parser.errors.is_empty(), true);

        assert_eq!(program.statements.len(), 1);
        assert_eq!(program.statements[0].token(), Token::Int("5".to_string()));
        assert_eq!(program.statements[0].to_string(), "5");
    }

    #[test]
    fn returns_errors_when_missing_assign_char() {
        let input = r#"
            let x = 5;
            let y 10;
            let foobar = 848484;
        "#.trim();

        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);

        let _ = parser.parse_program();

        assert_eq!(
            parser.errors(),
            &vec![r#"expected next token to be Assign, got Int("10") instead"#]
        );
    }

    #[test]
    fn returns_errors_when_missing_ident_char() {
        let input = r#"
            let x = 5;
            let = 10;
            let foobar = 848484;
        "#.trim();

        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);

        let _ = parser.parse_program();

        assert_eq!(
            parser.errors(),
            &vec![r#"expected next token to be Ident("<variable_name>"), got Assign instead"#],
        );
    }

    #[test]
    fn test_parsing_prefix_expression() {
        todo!()
        // page 59 or do it yourself ;)
    }
}
