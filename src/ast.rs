#![allow(dead_code)]
use crate::token;
pub struct Program {
    pub statements: Vec<Statement>
}

pub struct BlockStatement {
    pub token: token::Token,
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
    BooleanLiteral(BooleanLiteral),
    FunctionLiteral(FunctionLiteral),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    IfExpression(IfExpression),
    CallExpression(CallExpression)
}
    pub struct Identifier {
        pub token: token::Token,
        pub value: String
    }
    pub struct IntegerLiteral {
        pub token: token::Token,
        pub value: i32
    }
    pub struct BooleanLiteral {
        pub token: token::Token,
        pub value: bool
    }
    pub struct FunctionLiteral {
        pub token: token::Token,
        pub parameters: Vec<Identifier>,
        pub body: Box<BlockStatement>
    }
    pub struct PrefixExpression {
        pub token: token::Token,
        pub operator: String,
        pub right: Box<Expression>
    }
    pub struct InfixExpression {
        pub token: token::Token,
        pub left: Box<Expression>,
        pub operator: String,
        pub right: Box<Expression>
    }
    pub struct IfExpression{
        pub token: token::Token,
        pub condition: Box<Expression>,
        pub consequence: Box<BlockStatement>,
        pub alternative: Option<Box<BlockStatement>>
    }
    pub struct CallExpression {
        pub token: token::Token,
        pub function: Box<Expression>,
        pub arguments: Vec<Expression>
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
impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
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
            Expression::BooleanLiteral(bl) => {bl.token.literal.clone()}
            Expression::FunctionLiteral(fl) => {fl.token.literal.clone()}
            Expression::PrefixExpression(pe) => {pe.token.literal.clone()}
            Expression::InfixExpression(ie) => {ie.token.literal.clone()}
            Expression::CallExpression(ce) => {ce.token.literal.clone()}
            _ => "".to_string()
        }
    }
    fn string(&self) -> String {
        let mut out = String::from("");
        match self {
            Expression::Identifier(id) => {
                out += &id.string();
                out
            }
            Expression::IntegerLiteral(il) =>{
                out += &il.token.literal.clone();
                out
            }
            Expression::BooleanLiteral(bl) =>{
                out += &bl.token.literal.clone();
                out
            }
            Expression::FunctionLiteral(fl) => {
                let mut params = Vec::new();
                for param in &fl.parameters {
                    params.push(param.string());
                }
                out += "fn"; out += "(";
                out += &params.join(", "); out += ") ";
                out += &fl.body.string();
                out
            }
            Expression::PrefixExpression(pe) => {
                out += "("; 
                out += &pe.operator; 
                out += &pe.right.string();
                out += ")";
                out            }
            Expression::InfixExpression(ie) => {
                out += "("; out += &ie.left.string();
                out += " "; out += &ie.operator; out += " ";
                out += &ie.right.string();
                out += ")";
                out
            },
            Expression::IfExpression(ie) => {
                out += "if"; 
                out += &ie.condition.string();
                out += " "; out += &ie.consequence.string();
                match &ie.alternative {
                    None => (),
                    Some(x) => {out += "else"; out+= &x.string();}
                }
                out
            }
            Expression::CallExpression(ce) => {
                let mut args = Vec::new();
                for arg in &ce.arguments {
                    args.push(arg.string());
                }
                out += &ce.function.string();
                out += "(";
                out += &args.join(", ");
                out += ")";
                out
            }
            _=> out
        }
    }
}