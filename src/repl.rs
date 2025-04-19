use std::io::{self, Write};
use crate::lexer::Lexer;
use crate::token::TokenType;

const PROMPT: &str = ">> ";

pub fn start() {
    let stdin = io::stdin();
    let mut buffer = String::new();
    
    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();

        match stdin.read_line(&mut buffer) {
            Ok(_) => {},
            Err(e) => println!("Error reading line {:?}", e)
        }

        let mut l = Lexer::new(buffer.as_str());
        
        loop {
            let tok = l.next_token();

            if tok.ttype == TokenType::EOF {
                break;
            }
            println!("{:?}", tok);

        }

    }
}
