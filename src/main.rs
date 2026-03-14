mod ast;
mod builtins;
mod environment;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;
mod code;
mod compiler;

fn main() {
    repl::start();
}
