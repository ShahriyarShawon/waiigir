mod token;
mod lexer;
mod repl;
mod ast;
mod parser;

use crate::token::*;

fn main() {
    repl::start();
}
