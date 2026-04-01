mod ast;
mod builtins;
mod code;
mod compiler;
mod environment;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;
mod vm;

fn main() {
    repl::start();
}
