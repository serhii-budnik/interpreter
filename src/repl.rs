use crate::lexer::Lexer;
use crate::parser::Parser;
use std::io::Write;
use crate::evaluator::Evaluator;

const PROMPT: &'static str = ">> ";

pub fn start() {
    loop {
        let mut line = String::new();

        print!("{}", PROMPT);
        let _ = std::io::stdout().flush();

        std::io::stdin().read_line(&mut line).unwrap();

        if line == "exit\n" { break }

        let lexer = Lexer::new(&line);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        if !parser.errors().is_empty() {
            println!("\tparser errors: ");

            for error in parser.errors() {
                println!("\t\t{}", error);
            }

            continue;
        }

        let evaluated = program.eval();
        println!("{}", evaluated);

        println!("")
    }
}
