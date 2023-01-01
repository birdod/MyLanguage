#![allow(dead_code)]
use crate::token;

pub trait Node {
    fn token_literal(&self) -> String;

}
pub struct LetStatement {
    pub token: token::Token,
    pub name: Identifier,
    pub value: Expression
}
pub struct ReturnStatement {
    pub token: token::Token,
    pub return_value: Expression
}

pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement)
}

pub enum Expression {
    Temp
}

pub struct Identifier {
    pub token: token::Token,
    pub value: String
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Statement::LetStatement(x) => x.token.literal.clone(),
            Statement::ReturnStatement(x) => x.token.literal.clone()
        }
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

pub struct Program {
   pub statements: Vec<Statement>
}