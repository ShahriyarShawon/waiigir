mod token;
mod lexer;
mod repl;
mod ast;
mod parser;

use crate::token::*;
use crate::lexer::*;

fn main() {
    repl::start();
}
