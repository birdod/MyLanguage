mod token;
mod lexer;
mod repl;
mod ast;
mod parser; mod utill;

fn main() {
    println!("start repl");
    repl::start();
}