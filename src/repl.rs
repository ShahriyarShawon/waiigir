use crate::lexer;
use crate::token;
use std::io;

const PROMPT: &str = ">> ";

pub fn start() {
    let mut buffer = String::new();

    loop {
        print!("{PROMPT}");
        match io::stdin().read_line(&mut buffer) {
            Err(e) => {
                println!("Error reading {e}");
                return;
            }
            Ok(v) => {
                println!("{v} bytes read");
            }
        }
        let mut lexer = lexer::Lexer::new(&buffer);

        loop {
            let tok = lexer.next_token();
            if tok.token_type == token::TokenType::EOF {
                break;
            }

            println!("{:?}", tok);
        }
    }
}
