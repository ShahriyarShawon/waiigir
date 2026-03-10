mod token;
mod lexer;
mod repl;
mod ast;
mod parser;
mod object;
mod evaluator;
mod environment;
mod builtins;

use crate::token::*;

fn main() {
    repl::start();
}
