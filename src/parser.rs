#![allow(dead_code)]
mod parser_test;

use std::{mem::swap, collections::HashMap};
use crate::{ast::{self, Expression, Identifier, IntegerLiteral, PrefixExpression, InfixExpression, IfExpression, FunctionLiteral}, lexer, token};

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
            (token::LPAREN.to_string(), CALL),
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
        p.register_prefixfn(token::TRUE.to_string(), Parser::parse_booleanliteral);
        p.register_prefixfn(token::FALSE.to_string(), Parser::parse_booleanliteral);
        p.register_prefixfn(token::LPAREN.to_string(), Parser::parse_grouped_expression);
        p.register_prefixfn(token::IF.to_string(), Parser::parse_if_expression);
        p.register_prefixfn(token::FUNCTION.to_string(), Parser::parse_functionliteral);

        p.register_infixfn(token::PLUS.to_string(), Parser::parse_infix_expression);
        p.register_infixfn(token::MINUS.to_string(), Parser::parse_infix_expression);
        p.register_infixfn(token::SLASH.to_string(), Parser::parse_infix_expression);
        p.register_infixfn(token::ASTERLISK.to_string(), Parser::parse_infix_expression);
        p.register_infixfn(token::LT.to_string(), Parser::parse_infix_expression);
        p.register_infixfn(token::GT.to_string(), Parser::parse_infix_expression);
        p.register_infixfn(token::EQ.to_string(), Parser::parse_infix_expression);
        p.register_infixfn(token::NOT_EQ.to_string(), Parser::parse_infix_expression);
        p.register_infixfn(token::LPAREN.to_string(), Parser::parse_call_expression);
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

    fn parse_call_expression(&mut self, func: Expression) -> Expression {
        let tok = self.cur_token.clone();
        let args = self.parse_call_arguments();
        Expression::CallExpression(ast::CallExpression{
            token: tok,
            function: Box::new(func),
            arguments: args
        }
        )
    }
    
    fn parse_call_arguments(&mut self) -> Vec<Expression> {
        let mut v = Vec::new();
        self.next_token();
        if self.cur_token_is(token::RPAREN.to_string()){
            return v
        }
        v.push(self.parse_expression(LOWEST).unwrap());
        if self.peek_token_is(&token::RPAREN.to_string()){
            self.next_token();
            return v
        }
        self.next_token();
        while self.cur_token_is(token::COMMA.to_string()) {
            self.next_token();
            v.push(self.parse_expression(LOWEST).unwrap());
            self.next_token();
        }

        if !self.cur_token_is(token::RPAREN.to_string()){
            panic!("parenthesis mismatch from paramlist, now: {}",self.cur_token.r#type)
        }
        v
    }

    fn parse_function_parameters(&mut self) -> Vec<Identifier> {
        let mut v = Vec::new();
        self.next_token();
        if self.cur_token_is(token::RPAREN.to_string()){
            return v
        }
        match self.parse_identifier() {
            Expression::Identifier(temp) => v.push(temp),
            _ => panic!("not identifier")
        }
        if self.peek_token_is(&token::RPAREN.to_string()){
            self.next_token();
            return v
        }
        self.next_token();
        while self.cur_token_is(token::COMMA.to_string()) {
            self.next_token();
            match self.parse_identifier() {
                Expression::Identifier(temp) => v.push(temp),
                _ => panic!("not identifier")
            }
            self.next_token();
        }

        if !self.cur_token_is(token::RPAREN.to_string()){
            panic!("parenthesis mismatch from paramlist, now: {}",self.cur_token.r#type)
        }
        v
    }
    fn parse_functionliteral(&mut self) -> ast::Expression {
        let tok = self.cur_token.clone();
        if !self.expect_peek(token::LPAREN.to_string()) {
            panic!("require parenthesis for argument lisst")
        }
        let params = self.parse_function_parameters();
        if !self.expect_peek(token::LBRACE.to_string()) {
            panic!("requrire brace for function body")
        }
        let body = self.parse_block_statements();
        let exp = FunctionLiteral{
            token: tok,
            parameters: params,
            body: Box::new(body)
        };
        Expression::FunctionLiteral(exp)
    }

    fn parse_block_statements(&mut self) -> ast::BlockStatement{
        let mut bs = ast::BlockStatement{token: self.cur_token.clone(),statements: Vec::new()};
        self.next_token();
        while !self.cur_token_is(token::RBRACE.to_string()) {
            let stmt = self.parse_statement();
            match stmt {
                Some(s) => bs.statements.push(s),
                _ => ()
            }
            self.next_token();
        }
        bs
    }

    fn parse_if_expression(&mut self) -> ast::Expression {
        let tok = self.cur_token.clone();
        if !self.expect_peek(token::LPAREN.to_string()) {
            panic!("require condition parenthesis")
        }
        self.next_token();
        let cond = self.parse_expression(LOWEST).unwrap();
        if !self.expect_peek(token::RPAREN.to_string()) {
            panic!("require condition parenthesis")
        }
        if !self.expect_peek(token::LBRACE.to_string()) {
            panic!("require blocking brace")
        }
        let consequence = self.parse_block_statements();
        let mut exp = IfExpression{
            token: tok, condition: Box::new(cond), consequence:Box::new(consequence), alternative: None};
        if self.peek_token_is(&token::ELSE.to_string()) {
            self.next_token();
            if !self.expect_peek(token::LBRACE.to_string()) {
                panic!("require blocking brace")
            }
            exp.alternative = Some(Box::new(self.parse_block_statements()));
         }
        Expression::IfExpression(exp)
    }

    fn parse_grouped_expression(&mut self) -> ast::Expression{
        self.next_token();
        let exp = self.parse_expression(LOWEST);
        if !self.expect_peek(token::RPAREN.to_string()){
            self.errors.push("parenthesis mismatch".to_string());
        }
        return exp.unwrap()
    }

    fn parse_booleanliteral(&mut self) -> ast::Expression {
        ast::Expression::BooleanLiteral(
            ast::BooleanLiteral{token:self.cur_token.clone(), value: self.cur_token_is(token::TRUE.to_string())})
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
        let temp = self.parse_expression(precedence);
        match temp {
            Some(x) => exp.right = Box::new(x),
            None => ()
        }
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
        let mut stmt = ast::ReturnStatement{token: self.cur_token.clone(), return_value:Expression::Temp};
        self.next_token();
        stmt.return_value = self.parse_expression(LOWEST).unwrap();
        if self.peek_token_is(&token::SEMICOLON.to_string()){
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
        let mut stmt = ast::LetStatement{token: tok, name: iden, value:Expression::Temp};

        if !self.expect_peek(token::ASSIGN.to_string()){
            return None
        }
        self.next_token();
        stmt.value = self.parse_expression(LOWEST).unwrap();
        if !self.expect_peek(token::SEMICOLON.to_string()){
            return None
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
