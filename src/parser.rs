#![allow(dead_code)]
mod parser_test;

use std::mem::swap;
use crate::{ast::{self, Expression}, lexer, token};


struct Parser {
    l: lexer::Lexer,
    cur_token: token::Token,
    peek_token: token::Token,
    errors: Vec<String>
}

impl Parser {
    fn new(l: lexer::Lexer) -> Parser {
        let mut p = Parser{
            l:l, 
            cur_token: token::Token::empty_token(), 
            peek_token: token::Token::empty_token(),
            errors: vec!()
        };
        p.next_token();
        p.next_token();
        p
    }
    
    fn next_token(&mut self) {
        swap(&mut self.peek_token, &mut self.cur_token);
        self.peek_token = self.l.next_token();
    }

    fn parse_statement(&mut self) -> Option<ast::Statement> {
        let mat:&str = &self.cur_token.r#type;
        match mat {
            token::LET => {
                let temp = self.parse_letstatemnet();
                match temp {
                    Some(x) => return Some(ast::Statement::LetStatement(x)),
                    _ => return None
                }
            }
            _ => return None
        }
    }

    fn parse_letstatemnet(&mut self) -> Option<ast::LetStatement>{
        let tok = self.cur_token.clone();
        if !self.expect_peek(token::IDENT.to_string()) {
            return None
        }
        let iden = ast::Identifier{token: self.cur_token.clone(), value:self.cur_token.literal.clone()};
        let stmt = ast::LetStatement{token: tok, name: iden, value:Expression::Temp};

        if !self.expect_peek(token::ASSIGN.to_string()){
            return None
        }

        //TODO : Implement expression handle
        while !self.cur_token_is(token::SEMICOLON.to_string()){
            self.next_token();
        }

        Some(stmt)
    }

    fn parse_program(&mut self) -> ast::Program {
        let mut program = ast::Program{statements: Vec::new()};
        while !self.cur_token_is(token::EOF.to_string()) {
            let stmt = self.parse_statement();
            match stmt {
                Some(s) => program.statements.push(s),
                _ => ()
            }
            self.next_token();
        }
        program
    }

    fn cur_token_is(&self, t: token::TokenType) -> bool {
        return self.cur_token.r#type == t
    }
    
    fn peek_token_is(&self, t: &token::TokenType) -> bool {
        return self.peek_token.r#type == *t
    }

    fn expect_peek(&mut self, t:token::TokenType) -> bool {
        if self.peek_token_is(&t){
            self.next_token();
            return true
        } else {
            self.peek_error(t);
            return false
        }
    }
    fn peek_error(&mut self, t: token::TokenType){
        self.errors.push(format!("expect next token to be {}, but get {} instead",t,self.peek_token.r#type))
    }
    fn errors(&self) -> &Vec<String> {
        &self.errors
    }
}

