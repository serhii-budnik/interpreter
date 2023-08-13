use crate::{
    lexer::Lexer,
    token::Token,
    ast::{Program, Identifier, LetStatement, Statement, ReturnStatement},
};

#[allow(dead_code)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,

    cur_token: Token,
    peek_token: Token,

    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    fn new(mut lexer: Lexer<'a>) -> Parser<'a> {
        Self {
            cur_token: lexer.next_token(),
            peek_token: lexer.next_token(),
            lexer,
            errors: Vec::new(),
        }
    }

    fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    fn peek_error(&mut self, token: Token) {
        let error = format!("expected next token to be {:?}, got {:?} instead", &token, &self.peek_token);
        self.errors.push(error);
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

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
            _ => None
        }
    }

    fn parse_let_statement(&mut self) -> Option<Box<dyn Statement>> { 
        match self.peek_token.clone() { 
            Token::Ident(ident) => {
                self.next_token();

                match self.peek_token.clone() { 
                    Token::Assign => {
                        let identifier = Identifier { token: self.cur_token.clone(), value: ident };

                        // TODO: We're skipping the expressioin until we encouter a semicolon
                        while self.cur_token != Token::Semicolon {
                            self.next_token();
                        }

                        return Some(Box::new(LetStatement {
                            name: identifier,
                            value: Box::new(Identifier {
                                token: Token::Int("42".into()),
                                value: "42".into(),
                            }),
                        }));
                    },
                    token => return {
                        self.peek_error(Token::Assign);
                        None
                    },
                }
            },
            token => {
                self.peek_error(Token::Ident("<variable_name>".to_string()));
                None
            },
        }
    }

    fn parse_return_statement(&mut self) -> Option<Box<dyn Statement>> {
        self.next_token();

        // TODO: We're skipping the expressioin until we encouter a semicolon
        while self.cur_token != Token::Semicolon {
            self.next_token();
        }

        Some(Box::new(ReturnStatement {
            value: Box::new(
                Identifier { token: Token::Int("42".into()), value: "42".into() }
            ),
        }))
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
                value: Box::new(Identifier { token: Token::Int("848484".into()), value: "993322".into()})
            },
        ];

        for (index, test) in tests.iter().enumerate() {
            assert_eq!(test.token(), program.statements[index].token());
        }
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
}
