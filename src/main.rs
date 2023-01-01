mod token;
mod lexer;
mod repl;
mod ast;
mod parser;

fn main() {
    println!("start repl");
    repl::start();
}