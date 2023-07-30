use crate::lexer::Lexer;
use crate::token::Token;
use std::io::Write;

const PROMPT: &'static str = ">> ";

pub fn start() {
    loop {
        let mut line = String::new();

        print!("{}", PROMPT);
        let _ = std::io::stdout().flush();

        std::io::stdin().read_line(&mut line).unwrap();

        if line == "exit\n" { break }

        let mut lexer = Lexer::new(&line);

        loop {
            let token = lexer.next_token();
            if let Token::Eof = token { break };

            println!("{:?}", token);
        }
    }
}
