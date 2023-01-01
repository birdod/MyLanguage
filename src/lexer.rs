#![allow(dead_code)]
use crate::{token, token::Token};

fn is_letter(ch: char) -> bool{
    ('a' <= ch && ch <= 'z') || ('A'<=ch && ch<='Z') || ch=='_'
}

fn is_digit(ch: char) -> bool {
    '0' <= ch && ch <= '9'
}

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    pub ch: char
}


impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut l = Lexer{input: input, position:0, read_position:0, ch:'a'};
        l.read_char();
        l
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input.chars().nth(self.read_position).unwrap();
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&mut self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input.chars().nth(self.read_position).unwrap()
        }
    }

    fn read_identifier(&mut self) -> &str{
        let now = self.position;
        while is_letter(self.ch){
            self.read_char();
        }
        &self.input[now..self.position]
    }

    fn read_number(&mut self) -> &str{
        let now = self.position;
        while is_digit(self.ch){
            self.read_char();
        }
        &self.input[now..self.position]
    }


    fn skip_whitespace(&mut self) {
        while self.ch==' ' || self.ch=='\n' || self.ch=='\t' || self.ch=='\r' {
            self.read_char();
        }
    }

    pub fn next_token(&mut self) -> Token {
        let mut tok: Token;
        self.skip_whitespace();
        match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    tok = Token::new_token(token::EQ.to_string(), 't');
                    tok.literal = "==".to_string();                
                } else{
                    tok = Token::new_token(token::ASSIGN.to_string(), self.ch)
                }
            },
            ';' => tok = Token::new_token(token::SEMICOLON.to_string(), self.ch),
            '(' => tok = Token::new_token(token::LPAREN.to_string(), self.ch),
            ')' => tok = Token::new_token(token::RPAREN.to_string(), self.ch),
            ',' => tok = Token::new_token(token::COMMA.to_string(), self.ch),
            '+' => tok = Token::new_token(token::PLUS.to_string(), self.ch),
            '-' => tok = Token::new_token(token::MINUS.to_string(), self.ch),
            '*' => tok = Token::new_token(token::ASTERLISK.to_string(), self.ch),
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    tok = Token::new_token(token::NOT_EQ.to_string(), 't');
                    tok.literal = "!=".to_string();                
                } else{
                    tok = Token::new_token(token::BANG.to_string(), self.ch)
                }
            },            
            '/' => tok = Token::new_token(token::SLASH.to_string(), self.ch),
            '<' => tok = Token::new_token(token::LT.to_string(), self.ch),
            '>' => tok = Token::new_token(token::GT.to_string(), self.ch),
            '{' => tok = Token::new_token(token::LBRACE.to_string(), self.ch),
            '}' => tok = Token::new_token(token::RBRACE.to_string(), self.ch),
            '\0' => tok = Token::new_token(token::EOF.to_string(), '\0'),
            x => {
                if is_letter(x) {
                    let literal = self.read_identifier();
                    let tokentype = token::ident_to_type(literal);
                    tok = Token::new_token(tokentype, 't');
                    tok.literal = literal.to_string();
                    return tok
                } else if is_digit(x) {
                    let literal = self.read_number();
                    let tokentype = token::INT.to_string();
                    tok = Token::new_token(tokentype, 't');
                    tok.literal = literal.to_string();
                    return tok        
                }
                else {
                    tok = Token::new_token(token::ILLEGAL.to_string(), self.ch);
                }
            },
        }
        self.read_char();
        return tok
    }

}




#[cfg(test)]
mod tests {

    use {super::*, crate::token};


    #[test]
    fn test_next_token() {
        let input = 
        "
        let five = 5;
        let ten = 10;
        let add = fn(x,y) {
            x + y;
        };
        let result = add(five, ten);
        !=/*5;
        5 < 10 > 5;
        if (5>10) {
            return true
        } else {
            return false
        }
        ==
        !=
        ";
        let tests = [
            (token::LET, "let"),
            (token::IDENT, "five"),
            (token::ASSIGN, "="),
            (token::INT, "5"),
            (token::SEMICOLON, ";"),
            (token::LET, "let"),
            (token::IDENT, "ten"),
            (token::ASSIGN, "="),
            (token::INT, "10"),
            (token::SEMICOLON, ";"),
            (token::LET, "let"),
            (token::IDENT, "add"),
            (token::ASSIGN, "="),
            (token::FUNCTION, "fn"),
            (token::LPAREN, "("),
            (token::IDENT, "x"),
            (token::COMMA, ","),
            (token::IDENT, "y"),
            (token::RPAREN, ")"),
            (token::LBRACE, "{"),
            (token::IDENT, "x"),
            (token::PLUS, "+"),
            (token::IDENT, "y"),
            (token::SEMICOLON, ";"),
            (token::RBRACE, "}"),
            (token::SEMICOLON, ";"),
            (token::LET, "let"),
            (token::IDENT, "result"),
            (token::ASSIGN, "="),
            (token::IDENT, "add"),
            (token::LPAREN, "("),
            (token::IDENT, "five"),
            (token::COMMA, ","),
            (token::IDENT, "ten"),
            (token::RPAREN, ")"),
            (token::SEMICOLON, ";"),
            (token::NOT_EQ, "!="),
            (token::SLASH, "/"),
            (token::ASTERLISK, "*"),
            (token::INT, "5"),
            (token::SEMICOLON, ";"),
            (token::INT, "5"),
            (token::LT, "<"),
            (token::INT, "10"),
            (token::GT, ">"),
            (token::INT, "5"),
            (token::SEMICOLON, ";"),
            (token::IF, "if"),
            (token::LPAREN, "("),
            (token::INT, "5"),
            (token::GT, ">"),
            (token::INT, "10"),
            (token::RPAREN, ")"),
            (token::LBRACE, "{"),
            (token::RETURN, "return"),
            (token::TRUE, "true"),
            (token::RBRACE, "}"),
            (token::ELSE, "else"),
            (token::LBRACE, "{"),
            (token::RETURN, "return"),
            (token::FALSE, "false"),
            (token::RBRACE, "}"),
            (token::EQ, "=="),
            (token::NOT_EQ, "!=")
            
            
        ];
        let l = &mut Lexer::new(input.to_string());
    
        for (i, (tokentype, literal)) in tests.iter().enumerate() {
            let tok = l.next_token();
            if *tokentype!=tok.r#type {
                println!("tests{} - tokentype wrong. expected: {}, got: {}",
				i, tokentype, tok.r#type)
            }
            if *literal!=tok.literal {
                println!("tests{} - literal wrong. expected: {}, got: {}",
				i, literal, tok.literal)
            }
            // assert_eq!(*tokentype, tok.r#type);
            // assert_eq!(*literal, tok.literal);
        }
    }
}