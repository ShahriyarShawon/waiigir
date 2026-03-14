use crate::evaluator::Evaluator;
use crate::lexer;
use crate::parser;
use std::io;

const PROMPT: &str = ">> ";

pub fn start() {
    let mut buffer = String::new();

    loop {
        // eprint so it flushes io
        eprint!("{PROMPT}");
        buffer.clear();
        if let Err(e) = io::stdin().read_line(&mut buffer) {
            println!("Error reading {e}");
            return;
        }

        let lexer = lexer::Lexer::new(&buffer);
        let mut parser = parser::Parser::new(lexer);
        let program = parser.parse_program();
        if !parser.errors.is_empty() {
            print_parser_errors(&parser);
            continue;
        }

        let mut evaluator = Evaluator::new();
        let evaluated = evaluator.eval(program);
        if let Some(e) = evaluated {
            println!("{}", e);
        };
    }
}

fn print_parser_errors(parser: &parser::Parser) {
    for msg in &parser.errors {
        println!("\t{}", msg);
    }
}
