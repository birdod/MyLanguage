#![allow(dead_code)]

pub type TokenType = String;

pub struct Token {
    pub r#type: TokenType,
    pub literal: String
}

impl Token {
    pub fn empty_token() -> Token {
        return Token{r#type: EOF.to_string(), literal: "".to_string()}
    }
    pub fn new_token(token_type: TokenType, ch: char) -> Token {
        return Token{r#type: token_type, literal: ch.to_string()}
    }
}

impl Clone for Token {
    fn clone(&self) -> Token {
        Token{r#type: self.r#type.clone(), literal: self.literal.clone()}
    }
}

pub const ILLEGAL: &str = "ILLEGAL";
pub const EOF: &str     = "EOF";

pub const IDENT: &str = "IDENT";
pub const INT: &str   = "INT";

pub const ASSIGN: &str = "=";
pub const PLUS: &str   = "+";
pub const MINUS: &str   = "-";
pub const BANG: &str   = "!";
pub const SLASH: &str   = "/";
pub const ASTERLISK: &str   = "*";
pub const LT: &str   = "<";
pub const GT: &str   = ">";

pub const COMMA: &str     = ",";
pub const SEMICOLON: &str = ";";
pub const LPAREN: &str = "(";
pub const RPAREN: &str = ")";
pub const LBRACE: &str = "{";
pub const RBRACE: &str = "}";

pub const EQ: &str = "==";
pub const NOT_EQ: &str = "!=";

pub const FUNCTION: &str = "FUNCTION";
pub const LET: &str      = "LET";
pub const IF: &str      = "IF";
pub const ELSE: &str      = "ELSE";
pub const TRUE: &str      = "TRUE";
pub const FALSE: &str      = "FALSE";
pub const RETURN: &str      = "RETURN";

pub fn ident_to_type(ident: &str) -> TokenType {
    match ident {
        "let" => LET.to_string(),
        "fn" => FUNCTION.to_string(),
        "if" => IF.to_string(),
        "else" => ELSE.to_string(),
        "true" => TRUE.to_string(),
        "false" => FALSE.to_string(),
        "return" => RETURN.to_string(),
        _ => IDENT.to_string(),
    }
}