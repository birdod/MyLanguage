mod token;
mod lexer;
mod repl;
mod ast;
mod parser; mod utill;
mod object;
fn main() {
    println!("start repl");
    repl::start();
}