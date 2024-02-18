use crate::{
    lexer::Lexer,
    token::Token,
    ast::{ Expr, Program, Statement },
};

use std::mem;

#[derive(PartialEq, PartialOrd, Debug)]
pub enum Precedence {
    Lowest,
    Equals, // ==, !=
    LessGreater, // > or <
    Sum, // +
    Product, // *
    Prefix, // -X or !X
    Call, // myFunction(X)
}

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,

    cur_token: Token,
    peek_token: Token,

    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Parser<'a> {
        Self {
            cur_token: lexer.next_token(),
            peek_token: lexer.next_token(),
            lexer,
            errors: Vec::new(),
        }
    }

    pub fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    fn add_error(&mut self, err: String) {
        self.errors.push(err);
    }

    fn peek_err_msg(&self, token: Token) -> String {
       format!("expected next token to be {:?}, got {:?} instead", &token, &self.peek_token)
    }

    fn cur_precedence(&self) -> Precedence {
        self.cur_token.map_precedence()
    }

    fn peek_precedence(&self) -> Precedence {
        self.peek_token.map_precedence()
    }

    fn next_token(&mut self) {
        mem::swap(&mut self.cur_token, &mut self.peek_token);
        self.peek_token = self.lexer.next_token();
    }

    fn skip_until_semicolon(&mut self) {
        while self.cur_token != Token::Semicolon && self.cur_token != Token::Eof { self.next_token() }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut statements = Vec::new();

        while self.cur_token != Token::Eof {
            match self.parse_statement() {
                Ok(stmt) => {
                    statements.push(stmt);
                },
                Err(err) => {
                    self.add_error(err);
                    self.skip_until_semicolon();
                },
            };

            self.next_token();
        }

        Program::new(statements)
    }

    fn parse_statement(&mut self) -> Result<Box<Statement>, String> {
        match self.cur_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Box<Statement>, String> {
        if !matches!(self.peek_token, Token::Ident(_)) {
            return Err(self.peek_err_msg(Token::Ident("<variable_name>".into())));
        }
        self.next_token();

        let token = self.cur_token.take();
        let identifier = Expr::Ident(token);

        if self.peek_token != Token::Assign {
            return Err(self.peek_err_msg(Token::Assign))
        }

        self.next_token();
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        while self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Ok(Box::new(Statement::Let(Box::new(identifier), Some(value))))
    }

    fn parse_return_statement(&mut self) -> Result<Box<Statement>, String> {
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        while self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Ok(Box::new(Statement::Return(value)))
    }

    fn parse_expression_statement(&mut self) -> Result<Box<Statement>, String> {
        let expression = self.parse_expression(Precedence::Lowest)?;

        let stmt = Statement::ExprStatement(expression);

        while self.peek_token  == Token::Semicolon {
            self.next_token();
        }

        Ok(Box::new(stmt))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Box<Expr>, String> {
        let mut left = self.parse_prefix()?;

        while self.peek_token != Token::Semicolon && precedence < self.peek_precedence() {
            self.next_token();

            left = self.parse_infix(left)?;
        }

        Ok(left)
    }

    fn parse_identifier(&mut self) -> Result<Box<Expr>, String> {
        let token = self.cur_token.take();
        Ok(Box::new(Expr::Ident(token)))
    }

    fn parse_integer_literal(&mut self) -> Result<Box<Expr>, String> {
        let token = self.cur_token.take();
        let res = token.parse_integer();

        if let Err(err) = res {
            self.add_error(err.clone());

            return Err(err);
        }

        Ok(Box::new(Expr::Int(token)))
    }

    fn parse_prefix_expression(&mut self) -> Result<Box<Expr>, String> {
        let token = self.cur_token.take();

        self.next_token();

        let result = self.parse_expression(Precedence::Prefix);

        if let Err(err) = result {
            self.add_error(err.clone());

            return Err(err);
        }

        let right = result.unwrap();

        Ok(Box::new(
            Expr::Prefix(token, right)
        ))
    }

    pub fn parse_infix_expression(&mut self, left: Box<Expr>) -> Result<Box<Expr>, String> {
        let precedence = self.cur_precedence();
        let token = self.cur_token.take();

        self.next_token();

        let res = self.parse_expression(precedence);

        if let Err(err) = res {
            self.add_error(err.clone());

            return Err(err);
        }

        let right = res.unwrap();

        Ok(Box::new(Expr::Infix(left, token, right)))
    }

    pub fn parse_boolean(&mut self) -> Result<Box<Expr>, String> {
        Ok(Box::new(Expr::Bool(self.cur_token.take())))
    }

    pub fn parse_grouped_expression(&mut self) -> Result<Box<Expr>, String> {
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest)?;

        if let Token::Rparen = self.peek_token {
            self.next_token();
        } else {
            return Err(self.peek_err_msg(Token::Rparen));
        }

        Ok(exp)
    }

    pub fn parse_if_expression(&mut self) -> Result<Box<Expr>, String> {
        if self.peek_token != Token::Lparen {
            return Err(self.peek_err_msg(Token::Lparen));
        }

        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token != Token::Lbrace {
            return Err(self.peek_err_msg(Token::Lbrace));
        }
        self.next_token();

        let consequence = self.parse_block_statement()?;
        let mut alternative = None;

        if self.peek_token == Token::Else {
            self.next_token();

            if self.peek_token != Token::Lbrace {
                return Err(self.peek_err_msg(Token::Lbrace));
            }
            self.next_token();

            alternative = Some(Box::new(self.parse_block_statement()?));
        }

        Ok(Box::new(Expr::If(condition, Box::new(consequence), alternative)))
    }

    pub fn parse_block_statement(&mut self) -> Result<Statement, String> {
        let mut statements = Vec::new();

        while self.peek_token != Token::Rbrace && self.peek_token != Token::Eof {
            self.next_token();

            let exp_st = self.parse_statement()?;
            statements.push(exp_st);
        }

        if self.peek_token != Token::Rbrace {
            return Err(self.peek_err_msg(Token::Rbrace));
        }

        self.next_token();

        Ok(Statement::Block(statements))
    }

    pub fn parse_fn_expression(&mut self) -> Result<Box<Expr>, String> {
        if self.peek_token != Token::Lparen {
            return Err(self.peek_err_msg(Token::Lparen));
        }

        self.next_token();

        let params = self.parse_function_parameters()?;

        if self.peek_token != Token::Lbrace {
            return Err(self.peek_err_msg(Token::Lbrace));
        }

        self.next_token();

        let body = self.parse_block_statement()?;

        Ok(Box::new(Expr::Fn(params, body)))
    }

    pub fn parse_function_parameters(&mut self) -> Result<Vec<Box<Expr>>, String> {
        let mut identifiers = Vec::new();

        while self.peek_token != Token::Rparen && self.peek_token != Token::Eof {
            if !matches!(self.peek_token, Token::Ident(_)) {
                return Err(self.peek_err_msg(Token::Ident("<param>".into())));
            }

            self.next_token();

            let ident = self.parse_identifier()?;
            identifiers.push(ident);

            if self.peek_token == Token::Comma {
                self.next_token();
            }
        }

        self.next_token();

        Ok(identifiers)
    }

    pub fn parse_call_expression(&mut self, function: Box<Expr>) -> Result<Box<Expr>, String> {
        let args = self.parse_call_args()?;

        Ok(Box::new(Expr::Call(function, args)))
    }

    pub fn parse_call_args(&mut self) -> Result<Vec<Box<Expr>>, String> {
        let mut args = Vec::new();

        if self.peek_token == Token::Rparen {
            self.next_token();

            return Ok(args);
        }

        self.next_token();

        let arg = self.parse_expression(Precedence::Lowest)?;
        args.push(arg);

        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();

            let arg = self.parse_expression(Precedence::Lowest)?;
            args.push(arg);
        }

        if self.peek_token != Token::Rparen {
            return Err(self.peek_err_msg(Token::Rparen));
        }

        self.next_token();

        Ok(args)
    }

    pub fn parse_prefix(&mut self) -> Result<Box<Expr>, String> {
        match &self.cur_token {
            Token::Ident(_) => self.parse_identifier(),
            Token::Int(_) => self.parse_integer_literal(),
            Token::Bang | Token::Minus => self.parse_prefix_expression(),
            Token::True | Token::False => self.parse_boolean(),
            Token::Lparen => self.parse_grouped_expression(),
            Token::If => self.parse_if_expression(),
            Token::Function => self.parse_fn_expression(),
            token => Err(
                format!(
                    "expected tokens to parse prefix are (Ident, Int, Bang, Minus, True, False, Lparen, If) got {:?}",
                    token
                )
            ),
        }
    }

    pub fn parse_infix(&mut self, left: Box<Expr>) -> Result<Box<Expr>, String> {
        match &self.cur_token {
            Token::Plus |
            Token::Minus |
            Token::Slash |
            Token::Asterisk |
            Token::Eq |
            Token::NotEq |
            Token::LessThen |
            Token::GreaterThen => self.parse_infix_expression(left),
            Token::Lparen => self.parse_call_expression(left),
            token => {
                let valid_tokens = "Plus, Minus, Slash, Asterisk, Eq, NotEq, LessThen, GreaterThen";
                let reference = "See src/token.rs #map_precedence for reference";

                Err(format!("expected tokens to parse infix are ({}), got {}\n{}", valid_tokens, token, reference))
            },
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{
        ast::{ Expr, Statement },
        lexer::Lexer,
        parser::Parser,
        token::Token,
    };

    fn test_let_statement(test: &Statement, statement: &Box<Statement>) -> bool {
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
        assert_eq!(program.statements().len(), 3);

        let tests = [
            Statement::Let(
                Box::new(Expr::Ident(Token::Ident("x".to_string()))),
                Some(Box::new(Expr::Int(Token::Int("5".into()))))
            ),
            Statement::Let(
                Box::new(Expr::Ident(Token::Ident("y".to_string()))),
                Some(Box::new(Expr::Int(Token::Int("10".into()))))
            ),
            Statement::Let(
                Box::new(Expr::Ident(Token::Ident("foobar".to_string()))),
                Some(Box::new(Expr::Int(Token::Int("848484".into()))))
            ),
        ];

        for (index, test) in tests.iter().enumerate() {
            assert_eq!(test_let_statement(test, &program.statements()[index]), true)
        }
    }

    #[test]
    fn test_return_statements() {
        let input = r#"
            return 5;
            return 10;
            return 993322;
            return 5 + x;
        "#.trim();

        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(parser.errors().is_empty(), true);
        assert_eq!(program.statements().len(), 4);

        let tests = [
            Statement::Return(
                Box::new(Expr::Ident(Token::Int("5".into())))
            ),
            Statement::Return(
                Box::new(Expr::Ident(Token::Int("10".into())))
            ),
            Statement::Return(
                Box::new(Expr::Ident(Token::Int("993322".into())))
            ),
            Statement::Return(
                Box::new(Expr::Infix(
                    Box::new(Expr::Int(Token::Int("5".into()))),
                    Token::Plus,
                    Box::new(Expr::Ident(Token::Ident("x".into()))),
                ))
            ),
        ];

        for (index, test) in tests.iter().enumerate() {
            assert_eq!(test.to_string(), program.statements()[index].to_string());
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(parser.errors.is_empty(), true);

        assert_eq!(program.statements().len(), 1);
        assert_eq!(program.statements()[0].token(), Token::Ident("foobar".to_string()));
        assert_eq!(program.statements()[0].to_string(), "foobar;");
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";

        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(parser.errors.is_empty(), true);

        assert_eq!(program.statements().len(), 1);
        assert_eq!(program.statements()[0].token(), Token::Int("5".to_string()));
        assert_eq!(program.statements()[0].to_string(), "5;");
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
        let input = r#"
            !9;
            -10;
            !true;
            !false;
        "#.trim();

        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(parser.errors().is_empty(), true);
        assert_eq!(program.statements().len(), 4);

        let tests = [
            Statement::ExprStatement(
                Box::new(Expr::Prefix(
                    Token::Bang,
                    Box::new(Expr::Int(Token::Int("9".into())))
                ))
            ),
            Statement::ExprStatement(
                Box::new(Expr::Prefix(
                    Token::Minus,
                    Box::new(Expr::Int(Token::Int("10".into())))
                )),
            ),
            Statement::ExprStatement(
                Box::new(Expr::Prefix(
                    Token::Bang,
                    Box::new(Expr::Bool(Token::True))
                )),
            ),
            Statement::ExprStatement(
                Box::new(Expr::Prefix(
                    Token::Bang,
                    Box::new(Expr::Bool(Token::False))
                )),
            ),
        ];

        for (index, test) in tests.iter().enumerate() {
            assert_eq!(test.token(), program.statements()[index].token());
            assert_eq!(test.to_string(), program.statements()[index].to_string());
        }
    }

    #[test]
    fn test_parsing_infix_expression() {
        let input = r#"
            4 + 5;
            4 - 5;
            4 * 5;
            4 / 5;
            4 > 5;
            4 < 5;
            4 == 5;
            4 != 5;
        "#.trim();

        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(parser.errors().is_empty(), true);
        assert_eq!(program.statements().len(), 8);

        let tests = [
            Statement::ExprStatement(
                Box::new(Expr::Infix(
                    Box::new(Expr::Int(Token::Int("4".into()))),
                    Token::Plus,
                    Box::new(Expr::Int(Token::Int("5".into()))),
                )),
            ),
            Statement::ExprStatement(
                Box::new(Expr::Infix(
                    Box::new(Expr::Int(Token::Int("4".into()))),
                    Token::Minus,
                    Box::new(Expr::Int(Token::Int("5".into()))),
                )),
            ),
            Statement::ExprStatement(
                Box::new(Expr::Infix(
                    Box::new(Expr::Int(Token::Int("4".into()))),
                    Token::Asterisk,
                    Box::new(Expr::Int(Token::Int("5".into()))),
                )),
            ),
            Statement::ExprStatement(
                Box::new(Expr::Infix(
                    Box::new(Expr::Int(Token::Int("4".into()))),
                    Token::Slash,
                    Box::new(Expr::Int(Token::Int("5".into()))),
                )),
            ),
            Statement::ExprStatement(
                Box::new(Expr::Infix(
                    Box::new(Expr::Int(Token::Int("4".into()))),
                    Token::GreaterThen,
                    Box::new(Expr::Int(Token::Int("5".into()))),
                )),
            ),
            Statement::ExprStatement(
                Box::new(Expr::Infix(
                    Box::new(Expr::Int(Token::Int("4".into()))),
                    Token::LessThen,
                    Box::new(Expr::Int(Token::Int("5".into()))),
                )),
            ),
            Statement::ExprStatement(
                Box::new(Expr::Infix(
                    Box::new(Expr::Int(Token::Int("4".into()))),
                    Token::Eq,
                    Box::new(Expr::Int(Token::Int("5".into()))),
                )),
            ),
            Statement::ExprStatement(
                Box::new(Expr::Infix(
                    Box::new(Expr::Int(Token::Int("4".into()))),
                    Token::NotEq,
                    Box::new(Expr::Int(Token::Int("5".into()))),
                )),
            ),
        ];

        for (index, test) in tests.iter().enumerate() {
            assert_eq!(test.token(), program.statements()[index].token());
            assert_eq!(test.to_string(), program.statements()[index].to_string());
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let input = r#"
            -a * b;
            !-a;
            a + b + c;
            a + b - c;
            a * b * c;
            a * b / c;
            a + b / c;
            a + b * c;
            a + b * c + d / e - f;
            3 + 4;
            -5 * 5;
            5 > 4 == 3 < 4;
            5 < 4 != 3 > 4;
            3 + 4 * 5 == 3 * 1 + 4 * 5;
            3 > 5 == false;
            3 < 5 == true;
            1 + (2 + 3) + 4;
            (5 + 5) * 2;
            2 / (5 + 5);
            -(5 + 5);
            !(true == true);
            a + add(b * c) + d;
            add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8));
            add(a + b + c * d / f + g);
        "#.trim();

        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        let tests = [
            "((-a) * b);",
            "(!(-a));",
            "((a + b) + c);",
            "((a + b) - c);",
            "((a * b) * c);",
            "((a * b) / c);",
            "(a + (b / c));",
            "(a + (b * c));",
            "(((a + (b * c)) + (d / e)) - f);",
            "(3 + 4);",
            "((-5) * 5);",
            "((5 > 4) == (3 < 4));",
            "((5 < 4) != (3 > 4));",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));",
            "((3 > 5) == false);",
            "((3 < 5) == true);",
            "((1 + (2 + 3)) + 4);",
            "((5 + 5) * 2);",
            "(2 / (5 + 5));",
            "(-(5 + 5));",
            "(!(true == true));",
            "((a + add((b * c))) + d);",
            "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)));",
            "add((((a + b) + ((c * d) / f)) + g));",
        ].iter().map(|s| s.to_string());

        assert_eq!(parser.errors(), &Vec::<String>::new());

        for (index, test) in tests.enumerate() {
            assert_eq!(program.statements()[index].to_string(), test);
        }
    }

    #[test]
    fn test_boolean_parsing() {
        let input = r#"
            false;
            true;
        "#.trim();

        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        let tests = [
            "false;",
            "true;",
        ].iter().map(|s| s.to_string());

        for (index, test) in tests.enumerate() {
            assert_eq!(program.statements()[index].to_string(), test);
        }
    }

    #[test]
    fn test_invalid_syntax_for_grouped_expression() {
        let input = r#"
            (5 + 5);
            (9 + 1 ;
            (2 + 2);
        "#.trim();

        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);

        let _ = parser.parse_program();

        assert_eq!(parser.errors, vec!["expected next token to be Rparen, got Semicolon instead".to_string()]);
    }

    #[test]
    fn test_if_expression() {
        let input = r#"
            if (x < y) { x };
            if (x < y) { x; x + y };
            if (x < y) { x } else { y };
            if (x < y) { x } else { y; y + x };
        "#.trim();

        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(parser.errors.is_empty(), true);

        let tests = [
            "if (x < y) {\nx;\n};",
            "if (x < y) {\nx;\n(x + y);\n};",
            "if (x < y) {\nx;\n} else {\ny;\n};",
            "if (x < y) {\nx;\n} else {\ny;\n(y + x);\n};",
        ].iter().map(|s| s.to_string());

        for (index, test) in tests.enumerate() {
            assert_eq!(program.statements()[index].to_string(), test);
        }
    }

    #[test]
    fn test_invalid_if_expression() {
        let input = r#"
            if x < y { x };
            if (x < y) { x } else y;
            if (x < y) { x;
        "#.trim();

        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);

        let _ = parser.parse_program();

        assert_eq!(parser.errors, vec![
            "expected next token to be Lparen, got Ident(\"x\") instead".to_string(),
            "expected next token to be Lbrace, got Ident(\"y\") instead".to_string(),
            "expected next token to be Rbrace, got Eof instead".to_string(),
        ]);
    }

    #[test]
    fn test_fn_expression() {
        let input = r#"
            fn (x) { return x }
            fn (x, y) { return x + y; }
        "#.trim();

        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        let tests = [
            "fn(x) {\nreturn x;\n};",
            "fn(x, y) {\nreturn (x + y);\n};",
        ].iter().map(|s| s.to_string());

        assert_eq!(parser.errors(), &Vec::<String>::new());

        for (index, test) in tests.enumerate() {
            assert_eq!(program.statements()[index].to_string(), test);
        }
    }

    #[test]
    fn test_call_expression() {
        let input = r#"
            add();
            add(1);
            add(1, 2 * 3);
            add(1, bar(5), x + 1);
        "#.trim();

        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        let tests = [
            "add();",
            "add(1);",
            "add(1, (2 * 3));",
            "add(1, bar(5), (x + 1));",
        ].iter().map(|s| s.to_string());

        assert_eq!(parser.errors, Vec::<String>::new());

        for (index, test) in tests.enumerate() {
            assert_eq!(program.statements()[index].to_string(), test);
        }
    }
}
