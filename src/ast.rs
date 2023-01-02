#![allow(dead_code)]
use crate::token;
pub struct Program {
    pub statements: Vec<Statement>
 }
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement)
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

    pub struct ExpressionStatement {
        pub token: token::Token,
        pub expression: Expression
    }


pub enum Expression {
    Temp,
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    PrefixExpression(PrefixExpression)
}
    pub struct Identifier {
        pub token: token::Token,
        pub value: String
    }
    pub struct IntegerLiteral {
        pub token: token::Token,
        pub value: i32
    }
    pub struct PrefixExpression {
        pub token: token::Token,
        pub operator: String,
        pub right: Box<Expression>
    }

pub trait Node {
    fn token_literal(&self) -> String;
    fn string(&self) -> String;
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else{
            "".to_string()
        }
    }

    fn string(&self) -> String {
        let mut out = String::from("");
        for stmt in &self.statements {
            out += &(stmt.string());
        }
        out
    }
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Statement::LetStatement(x) => x.token.literal.clone(),
            Statement::ReturnStatement(x) => x.token.literal.clone(),
            Statement::ExpressionStatement(x) => x.token.literal.clone()
        }
    }
    fn string(&self) -> String {
        let mut out = String::from("");
        match self {
            Statement::LetStatement(ls) => {
                out += &(ls.token.literal);
                out += " ";
                out += &(ls.name.string());
                out += "=";
                out += &(ls.value.string());
                out += ";";
                out
            }
            Statement::ReturnStatement(rs) => {
                out += &(rs.token.literal);
                out += " "; 
                out += &(rs.return_value.string());
                out += ";";
                out
            }
            Statement::ExpressionStatement(es) => {
                out += &(es.expression.string());
                out
            }
        }
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn string(&self) -> String {
        self.value.clone()
    }
}


// TODO: implement this
impl Node for Expression {
    fn token_literal(&self) -> String {
        match self{
            Expression::Identifier(id) => {id.token_literal()},
            Expression::IntegerLiteral(il) => {il.token.literal.clone()}
            Expression::PrefixExpression(pe) => {pe.token.literal.clone()}
            _ => "".to_string()
        }
    }
    fn string(&self) -> String {
        "".to_string()
    }
}