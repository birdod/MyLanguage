#![allow(dead_code)]
mod parser_test;

use std::{mem::swap, collections::HashMap};
use crate::{ast::{self, Expression, Identifier, IntegerLiteral, PrefixExpression, InfixExpression}, lexer, token};

type PrefixParseFn = fn(&mut Parser) -> ast::Expression;
type InfixParseFn = fn(&mut Parser, ast::Expression) -> ast::Expression;

struct Parser {
    l: lexer::Lexer,
    errors: Vec<String>,

    cur_token: token::Token,
    peek_token: token::Token,

    prefix_parse_fns: HashMap<token::TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<token::TokenType, InfixParseFn>,

    precedences: HashMap<token::TokenType, i32>
}

impl Parser {
    fn new(l: lexer::Lexer) -> Parser {
        let temp: HashMap<token::TokenType, PrefixParseFn> = HashMap::new();
        let temp2: HashMap<token::TokenType, InfixParseFn> = HashMap::new();
        let precedences: HashMap<token::TokenType, i32> = HashMap::from([
            (token::EQ.to_string(), EQUAL),
            (token::NOT_EQ.to_string(), EQUAL),
            (token::LT.to_string(), LESSGREATER),
            (token::GT.to_string(), LESSGREATER),
            (token::PLUS.to_string(), SUM),
            (token::MINUS.to_string(), SUM),
            (token::SLASH.to_string(), PRODUCT),
            (token::ASTERLISK.to_string(), PRODUCT),
        ]);
        let mut p = Parser{
            l:l, 
            errors: vec!(),
            cur_token: token::Token::empty_token(), 
            peek_token: token::Token::empty_token(),
            prefix_parse_fns: temp,
            infix_parse_fns: temp2,
            precedences: precedences
        };
        p.register_prefixfn(token::IDENT.to_string(), Parser::parse_identifier);
        p.register_prefixfn(token::INT.to_string(), Parser::parse_integerliteral);
        p.register_prefixfn(token::MINUS.to_string(), Parser::parse_prefix_expression);
        p.register_prefixfn(token::BANG.to_string(), Parser::parse_prefix_expression);

        p.register_infixfn(token::PLUS.to_string(), Parser::parse_infix_expression);
        p.register_infixfn(token::MINUS.to_string(), Parser::parse_infix_expression);
        p.register_infixfn(token::SLASH.to_string(), Parser::parse_infix_expression);
        p.register_infixfn(token::ASTERLISK.to_string(), Parser::parse_infix_expression);
        p.register_infixfn(token::LT.to_string(), Parser::parse_infix_expression);
        p.register_infixfn(token::GT.to_string(), Parser::parse_infix_expression);
        p.register_infixfn(token::EQ.to_string(), Parser::parse_infix_expression);
        p.register_infixfn(token::NOT_EQ.to_string(), Parser::parse_infix_expression);
        p.next_token();
        p.next_token();
        p
    }
    
    fn register_prefixfn(&mut self,token_type:token::TokenType, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_type,func);
    }

    fn register_infixfn(&mut self,token_type:token::TokenType, func: InfixParseFn) {
        self.infix_parse_fns.insert(token_type,func);
    }

    fn next_token(&mut self) {
        swap(&mut self.peek_token, &mut self.cur_token);
        self.peek_token = self.l.next_token();
    }
    fn parse_infix_expression(&mut self, left: Expression) -> Expression {
        let mut exp = InfixExpression{
                token: self.cur_token.clone(),
                operator: self.cur_token.literal.clone(),
                left: Box::new(left),
                right: Box::new(Expression::Temp)
            };
        let precedence = self.cur_precedence();
        self.next_token();
        exp.right = Box::new(self.parse_expression(precedence).unwrap());
        Expression::InfixExpression(exp)
    }

    fn parse_prefix_expression(&mut self) -> ast::Expression {
        let temp = self.cur_token.clone();
        self.next_token();

        Expression::PrefixExpression(
            PrefixExpression{
                token: temp.clone(), 
                operator: temp.literal, 
                right: Box::new(self.parse_expression(PREFIX).unwrap())
            }
        )
    }

    fn parse_identifier(&mut self) -> ast::Expression {
        ast::Expression::Identifier(Identifier{token: self.cur_token.clone(), value: self.cur_token.literal.clone()})
    }

    fn parse_integerliteral(&mut self) -> ast::Expression {
        let val = self.cur_token.literal.parse::<i32>().unwrap();
        ast::Expression::IntegerLiteral(IntegerLiteral{token: self.cur_token.clone(), value: val})
    }

    fn parse_expression(&mut self, precednce: i32) -> Option<ast::Expression> {
        let prefix = self.prefix_parse_fns.get(&self.cur_token.r#type);
        let mut leftexp;
        match prefix {
            Some(x) => {
                leftexp = x(self);
            }
            None => {
                self.no_prefix_parse_fn_error(self.cur_token.r#type.clone());
                return None
            }
        }
        while !self.peek_token_is(&token::SEMICOLON.to_string())&&(precednce<self.peek_precedence()) {
            let infix = self.infix_parse_fns.get(&self.peek_token.r#type).cloned();
            match infix {
                Some(x) => {
                    self.next_token();
                    leftexp = x(self,leftexp);
                }
                None => {
                    return Some(leftexp)
                }
            }
        }
        Some(leftexp)
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
            token::RETURN => {
                let temp = self.parse_returnstatement();
                match temp {
                    Some(x) => return Some(ast::Statement::ReturnStatement(x)),
                    _ => return None
                }
            }
            _ => {
                let temp = self.parse_expressionstatement();
                match temp {
                    Some(x) => return Some(ast::Statement::ExpressionStatement(x)),
                    _ => return None
                }
            }
        }
    }
    fn parse_expressionstatement(&mut self) -> Option<ast::ExpressionStatement> {
        let mut stmt = ast::ExpressionStatement{
            token: self.cur_token.clone(), 
            expression:Expression::Temp
        };
        let temp = self.parse_expression(LOWEST);
        match temp {
            Some(x) => stmt.expression = x,
            None => ()
        }
        if self.peek_token_is(&token::SEMICOLON.to_string()) {
            self.next_token();
        }
        Some(stmt)
    }

    fn parse_returnstatement(&mut self) -> Option<ast::ReturnStatement> {
        let stmt = ast::ReturnStatement{token: self.cur_token.clone(), return_value:Expression::Temp};
        self.next_token();
        while !self.cur_token_is(token::SEMICOLON.to_string()){
            self.next_token();
        }
        Some(stmt)
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
    fn peek_precedence(&self) -> i32 {
        let temp = self.precedences.get(&self.peek_token.r#type);
        match temp {
            Some(x) => *x,
            _ => LOWEST
        }
    }
    fn cur_precedence(&self) -> i32 {
        let temp = self.precedences.get(&self.cur_token.r#type);
        match temp {
            Some(x) => *x,
            _ => LOWEST
        }
    }
    fn peek_error(&mut self, t: token::TokenType){
        self.errors.push(format!("expect next token to be {}, but get {} instead",t,self.peek_token.r#type))
    }
    fn no_prefix_parse_fn_error(&mut self, t:token::TokenType) {
        self.errors.push(format!("no parse fn match with {}",t))
    }
    fn errors(&self) -> &Vec<String> {
        &self.errors
    }
}

const LOWEST: i32 = 1;
const EQUAL: i32 = 2;
const LESSGREATER: i32 = 3;
const SUM: i32 = 4;
const PRODUCT: i32 = 5;
const PREFIX: i32 = 6;
const CALL: i32 = 7;
