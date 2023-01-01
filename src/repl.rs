use std::io;
use crate::lexer;
use std::io::Write;


pub fn start() {
    loop {
        print!(">>"); 
        io::stdout().flush().unwrap();
        let stdin = io::stdin();
        let mut inp = String::new();
        stdin.read_line(&mut inp).unwrap();
        if inp == "" {
            return
        }
        let mut lexer = lexer::Lexer::new(inp);
        while lexer.ch != '\0' {
            let tok = lexer.next_token();
            println!("type: {}, literal: {}",tok.r#type, tok.literal);
        }

    }    
}