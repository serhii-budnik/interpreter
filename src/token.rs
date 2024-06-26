use std::{fmt::Display, mem};
use std::convert::AsRef;

use crate::parser::Precedence;

#[derive(Eq, Hash, Debug, PartialEq, Clone)]
pub enum Token {
    Illegal,
    Eof,

    // Identifiers + literals
    Ident(String), // add, foobar, x, y, ...
    Int(String), // 1343456
    TString(String), // "hello world"

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Eq,
    NotEq,

    // Delimiters
    Comma,
    Semicolon,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    LBracket,
    RBracket,
    LessThen,
    GreaterThen,

    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,

    // smth to put instead of value that was taken to fool borrow checker
    _Taken,
}

impl Token {
    pub fn lookup_ident(ident: &str) -> Token {
        match ident {
            "fn" => Token::Function,
            "let" => Token::Let,
            "return" => Token::Return,
            "else" => Token::Else,
            "if" => Token::If,
            "true" => Token::True,
            "false" => Token::False,
            _ => Token::Ident(ident.into()),
        }
    }

    pub fn parse_integer(&self) -> Result<i64, String> {
        if let Token::Int(val) = self {
            return match val.parse::<i64>() {
                Ok(int) => Ok(int),
                Err(err) => Err(format!("expected integer, got {:?}", err)),
            }
        }

        Err(format!("expected token to parse Token::Int, got {:?}", self))
    }

    pub fn map_precedence(&self) -> Precedence {
        match self {
            Self::Eq | Self::NotEq => Precedence::Equals,
            Self::LessThen | Self::GreaterThen => Precedence::LessGreater,
            Self::Plus | Self::Minus => Precedence::Sum,
            Self::Slash | Self::Asterisk => Precedence::Product,
            Self::Lparen => Precedence::Call,
            Self::LBracket => Precedence::IndexExpr,
            _ => Precedence::Lowest,
        }
    }

    pub fn take(&mut self) -> Self {
        mem::take(self)
    }

    pub fn is_data_type(&self) -> bool {
        matches!(self, Self::Int(_) | Self::TString(_) | Self::Ident(_) | Self::True | Self::False | Self::LBracket)
    }

    pub fn value(self) -> String {
        match self {
            Self::Ident(s) | Self::Int(s) => s,
            _ => panic!("Token::value() called on non-value token"),
        }
    }
}

impl Default for Token {
    fn default() -> Self {
        Self::_Taken
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let stringified_token = match self {
            Self::Ident(s) | Self::Int(s) | Self::TString(s) => s,
            Self::Illegal => "Illegal",
            Self::Eof => "Eof",
            Self::Assign => "=",
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Bang => "!",
            Self::Asterisk => "*",
            Self::Slash => "/",
            Self::Eq => "==",
            Self::NotEq => "!=",
            Self::Comma => ",",
            Self::Semicolon => ";",
            Self::Lparen => "(",
            Self::Rparen => ")",
            Self::Lbrace => "{",
            Self::Rbrace => "}",
            Self::LBracket => "[",
            Self::RBracket => "]",
            Self::LessThen => "<",
            Self::GreaterThen => ">",
            Self::Function => "fn",
            Self::Let => "let",
            Self::True => "true",
            Self::False => "false",
            Self::If => "if",
            Self::Else => "else",
            Self::Return => "return",
            Self::_Taken => panic!(
                "Token::_Taken is the stub that was filled instead of moved value. Should not be stringified"
            ),
        };

        write!(f, "{}", stringified_token)
    }
}

impl AsRef<str> for Token {
    fn as_ref(&self) -> &str {
        match self {
            Self::Ident(s) | Self::Int(s) | Self::TString(s) => s,
            _ => panic!("Token::as_ref() called on non-value token"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::token::Token;

    #[test]
    fn take_method_swaps_value_in_memory() {
        let mut token = Token::Eq;

        let taken_token = token.take();

        assert_eq!(taken_token, Token::Eq);
        assert_eq!(token, Token::_Taken);
    }
}

