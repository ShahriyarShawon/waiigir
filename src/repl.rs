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
        match io::stdin().read_line(&mut buffer) {
            Err(e) => {
                println!("Error reading {e}");
                return;
            }
            Ok(_) => {}
        }

        let lexer = lexer::Lexer::new(&buffer);
        let mut parser = parser::Parser::new(lexer);
        let program = parser.parse_program();
        if parser.errors.len() != 0 {
            print_parser_errors(&parser);
            continue;
        }

        println!("{}", program.to_string());

    }

    fn print_parser_errors(parser: &parser::Parser) {
        for msg in &parser.errors {
            println!("\t{}", msg);
        }
    }
}
