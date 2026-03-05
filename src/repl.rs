use crate::lexer;
use crate::parser;
use crate::evaluator;
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

        // println!("{}", program.to_string());
        let evaluated = evaluator::eval(program);
        match evaluated {
            Some(e) => println!("{}", e.Inspect()),
            _ => todo!()
        }

    }

    fn print_parser_errors(parser: &parser::Parser) {
        for msg in &parser.errors {
            println!("\t{}", msg);
        }
    }
}
