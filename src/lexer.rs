use crate::token::Token;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug)]
#[allow(dead_code)]
pub struct Lexer<'a> {
    input: &'a str,
    chars: Peekable<Chars<'a>>,

    // maybe we can get rid of them later
    position: u32,
    read_position: u32,

    ch: Option<char>,
}

impl<'a> Lexer<'a> {
    #[allow(dead_code)]
    pub fn new(input: &'a str) -> Lexer<'a> {
        let mut l = Self {
            ch: None,
            chars: input.chars().peekable(),
            input,

            position: 0,
            read_position: 0,
        };
        l.read_char();
        l
    } 

    fn read_char(&mut self) -> Option<char> {
        self.ch = self.chars.next();
        self.ch
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.ch {
            Some('=') => {
                if let Some('=') = self.peek_char() {
                    self.read_char();
                    Token::Eq
                } else {
                    Token::Assign
                }
            },
            Some(';') => Token::Semicolon,
            Some('(') => Token::Lparen,
            Some(')') => Token::Rparen,
            Some('{') => Token::Lbrace,
            Some('}') => Token::Rbrace,
            Some('+') => Token::Plus,
            Some(',') => Token::Comma,
            Some('<') => Token::LessThen,
            Some('>') => Token::GreaterThen,
            Some('!') => {
                if let Some('=') = self.peek_char() {
                    self.read_char();
                    Token::NotEq
                } else {
                    Token::Bang
                }
            },
            Some('*') => Token::Asterisk,
            Some('-') => Token::Minus,
            Some('/') => Token::Slash,
            Some(ch) => {
                if self.is_letter(ch) {
                    let ident = self.read_identifier();
                    return Token::lookup_ident(&ident)
                } else if self.is_digit(ch) {
                    return Token::Int(self.read_number())
                } else {
                    Token::Illegal
                }
            },
            None => Token::Eof,
        };

        self.read_char();

        token
    }

    fn read_identifier(&mut self) -> String {
        let mut identifier = String::new();

        while let Some(ch) = self.ch  {
            if !self.is_letter(ch) { break }

            identifier.push(ch);
            self.read_char();
        }

        identifier
    }

    fn read_number(&mut self) -> String { 
        let mut number = String::new();
         
        while let Some(ch) = self.ch  {
            if !self.is_digit(ch) { break }

            number.push(ch);
            self.read_char();
        }

        number
    }

    fn is_letter(&self, ch: char) -> bool {
        ('a'..='z').contains(&ch) || ('A'..='Z').contains(&ch) || ch == '_'
    }
    
    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.ch {
            match ch {
                ' ' | '\t' | '\r' | '\n'  => { self.read_char(); },
                _ => break,
            };
        };
    }
    
    fn is_digit(&self, ch: char) -> bool { 
        ('0'..='9').contains(&ch)
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::Lexer;
    use crate::token::Token;

    #[test]
    fn test_next_token() {
        let input = "=+(){},;";

        let mut lexer = Lexer::new(input);

        let expected_tokens = [
            Token::Assign,
            Token::Plus,
            Token::Lparen,
            Token::Rparen,
            Token::Lbrace,
            Token::Rbrace,
            Token::Comma,
            Token::Semicolon,
        ];

        for expected_token in expected_tokens {
            assert_eq!(expected_token, lexer.next_token());
        }
    }

    #[test]
    fn test_next_token_with_literals() {
        let input = r#"
            let five = 5;
            let ten = 10;

            let add = fn(x, y) {
                x + y;
            };

            let result = add(five, ten);
        "#.trim();

        let mut lexer = Lexer::new(input);

        let expected_tokens = [
            Token::Let,
            Token::Ident("five".into()),
            Token::Assign,
            Token::Int("5".into()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".into()),
            Token::Assign,
            Token::Int("10".into()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".into()),
            Token::Assign,
            Token::Function,
            Token::Lparen,
            Token::Ident("x".into()),
            Token::Comma,
            Token::Ident("y".into()),
            Token::Rparen,
            Token::Lbrace,
            Token::Ident("x".into()),
            Token::Plus,
            Token::Ident("y".into()),
            Token::Semicolon,
            Token::Rbrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".into()),
            Token::Assign,
            Token::Ident("add".into()),
            Token::Lparen,
            Token::Ident("five".into()),
            Token::Comma,
            Token::Ident("ten".into()),
            Token::Rparen,
            Token::Semicolon,
            Token::Eof,
        ];

        for expected_token in expected_tokens {
            assert_eq!(expected_token, lexer.next_token());
        }
    }


    #[test]
    fn test_next_token_with_special_chars() {
        let input = r#"
            !-/*5;
            5 < 10 > 5;
        "#.trim();

        let mut lexer = Lexer::new(input);

        let expected_tokens = [
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int("5".into()),
            Token::Semicolon,
            Token::Int("5".into()),
            Token::LessThen,
            Token::Int("10".into()),
            Token::GreaterThen,
            Token::Int("5".into()),
            Token::Semicolon,
        ];

        for expected_token in expected_tokens {
            assert_eq!(expected_token, lexer.next_token());
        }
    }


    #[test]
    fn test_next_token_with_keywords() {
        let input = r#"
            if (5 < 10) {
                return true;
            } else {
                return false;
            }

            10 == 10;
            10 != 9;
        "#.trim();

        let mut lexer = Lexer::new(input);

        let expected_tokens = [
            Token::If,
            Token::Lparen,
            Token::Int("5".into()),
            Token::LessThen,
            Token::Int("10".into()),
            Token::Rparen,
            Token::Lbrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::Rbrace,
            Token::Else,
            Token::Lbrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::Rbrace,
            Token::Int("10".into()),
            Token::Eq,
            Token::Int("10".into()),
            Token::Semicolon,
            Token::Int("10".into()),
            Token::NotEq,
            Token::Int("9".into()),
            Token::Semicolon,
        ];

        for expected_token in expected_tokens {
            assert_eq!(expected_token, lexer.next_token());
        }
    }
}
