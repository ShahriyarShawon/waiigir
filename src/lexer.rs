use crate::token::{Token, TokenType};
use std::collections::HashMap;

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(i: &str) -> Lexer {
        let mut l = Lexer {
            input: String::from(i),
            position: 0,
            read_position: 0,
            ch: '0',
        };
        l.read_char();
        l
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            return '\0';
        } else {
            return *self.input.as_bytes().get(self.read_position).unwrap() as char;
        }
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            // unwrap because we checked bounds of input in the if statement
            self.ch = self.input.as_bytes()[self.read_position] as char;
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_identifier(&mut self) -> String {
        let pos = self.position;
        while self.ch.is_alphabetic() {
            self.read_char();
        }
        self.input[pos..self.position].to_string()
    }

    fn read_number(&mut self) -> String {
        let pos = self.position;
        while self.ch.is_numeric() {
            self.read_char()
        }
        self.input[pos..self.position].to_string()
    }

    fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
            self.read_char();
        }
    }

    pub fn next_token(&mut self) -> Token {
        let keywords: HashMap<&str, TokenType> = HashMap::from([
            ("fn", TokenType::FUNCTION),
            ("let", TokenType::LET),
            ("true", TokenType::TRUE),
            ("false", TokenType::FALSE),
            ("if", TokenType::IF),
            ("else", TokenType::ELSE),
            ("return", TokenType::RETURN),
        ]);

        self.skip_whitespace();

        let token = match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token {
                        ttype: TokenType::EQ,
                        literal: String::from("=="),
                    }
                } else {
                    Token {
                        ttype: TokenType::ASSIGN,
                        literal: String::from("="),
                    }
                }
            }
            '+' => Token {
                ttype: TokenType::PLUS,
                literal: String::from("+"),
            },
            '-' => Token {
                ttype: TokenType::MINUS,
                literal: String::from("-"),
            },
            '/' => Token {
                ttype: TokenType::SLASH,
                literal: String::from("/"),
            },
            '*' => Token {
                ttype: TokenType::ASTERISK,
                literal: String::from("*"),
            },
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token {
                        ttype: TokenType::NOT_EQ,
                        literal: String::from("!="),
                    }
                } else {
                    Token {
                        ttype: TokenType::BANG,
                        literal: String::from("!"),
                    }
                }
            }
            '>' => Token {
                ttype: TokenType::GT,
                literal: String::from(">"),
            },
            '<' => Token {
                ttype: TokenType::LT,
                literal: String::from("<"),
            },
            '(' => Token {
                ttype: TokenType::LPAREN,
                literal: String::from("("),
            },
            ')' => Token {
                ttype: TokenType::RPAREN,
                literal: String::from(")"),
            },
            '{' => Token {
                ttype: TokenType::LBRACE,
                literal: String::from("{"),
            },
            '}' => Token {
                ttype: TokenType::RBRACE,
                literal: String::from("}"),
            },
            ',' => Token {
                ttype: TokenType::COMMA,
                literal: String::from(","),
            },
            ';' => Token {
                ttype: TokenType::SEMICOLON,
                literal: String::from(";"),
            },
            '\0' => Token {
                ttype: TokenType::EOF,
                literal: String::from(""),
            },

            _ => {
                if self.ch.is_alphabetic() {
                    let ident = self.read_identifier();
                    let ttype = if keywords.contains_key(ident.as_str()) {
                        *keywords.get(ident.as_str()).unwrap()
                    } else {
                        TokenType::IDENT
                    };
                    return Token {
                        ttype,
                        literal: ident,
                    };
                } else if self.ch.is_numeric() {
                    return Token {
                        ttype: TokenType::INT,
                        literal: self.read_number(),
                    };
                } else {
                    Token {
                        ttype: TokenType::ILLEGAL,
                        literal: String::from(self.ch),
                    }
                }
            }
        };
        self.read_char();
        token
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = r#"
let five = 5;
let ten = 10;
let add = fn(x, y) {
    x + y;
};
let result = add(five, ten);
!-/*5;
5 < 10 > 5;
if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;
"#;

        let expected_tokens = vec![
            (TokenType::LET, "let"),
            (TokenType::IDENT, "five"),
            (TokenType::ASSIGN, "="),
            (TokenType::INT, "5"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::LET, "let"),
            (TokenType::IDENT, "ten"),
            (TokenType::ASSIGN, "="),
            (TokenType::INT, "10"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::LET, "let"),
            (TokenType::IDENT, "add"),
            (TokenType::ASSIGN, "="),
            (TokenType::FUNCTION, "fn"),
            (TokenType::LPAREN, "("),
            (TokenType::IDENT, "x"),
            (TokenType::COMMA, ","),
            (TokenType::IDENT, "y"),
            (TokenType::RPAREN, ")"),
            (TokenType::LBRACE, "{"),
            (TokenType::IDENT, "x"),
            (TokenType::PLUS, "+"),
            (TokenType::IDENT, "y"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::RBRACE, "}"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::LET, "let"),
            (TokenType::IDENT, "result"),
            (TokenType::ASSIGN, "="),
            (TokenType::IDENT, "add"),
            (TokenType::LPAREN, "("),
            (TokenType::IDENT, "five"),
            (TokenType::COMMA, ","),
            (TokenType::IDENT, "ten"),
            (TokenType::RPAREN, ")"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::BANG, "!"),
            (TokenType::MINUS, "-"),
            (TokenType::SLASH, "/"),
            (TokenType::ASTERISK, "*"),
            (TokenType::INT, "5"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::INT, "5"),
            (TokenType::LT, "<"),
            (TokenType::INT, "10"),
            (TokenType::GT, ">"),
            (TokenType::INT, "5"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::IF, "if"),
            (TokenType::LPAREN, "("),
            (TokenType::INT, "5"),
            (TokenType::LT, "<"),
            (TokenType::INT, "10"),
            (TokenType::RPAREN, ")"),
            (TokenType::LBRACE, "{"),
            (TokenType::RETURN, "return"),
            (TokenType::TRUE, "true"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::RBRACE, "}"),
            (TokenType::ELSE, "else"),
            (TokenType::LBRACE, "{"),
            (TokenType::RETURN, "return"),
            (TokenType::FALSE, "false"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::RBRACE, "}"),
            (TokenType::INT, "10"),
            (TokenType::EQ, "=="),
            (TokenType::INT, "10"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::INT, "10"),
            (TokenType::NOT_EQ, "!="),
            (TokenType::INT, "9"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::EOF, ""),
        ];

        let mut lexer = Lexer::new(input);

        for (i, (expected_type, expected_literal)) in expected_tokens.iter().enumerate() {
            let tok = lexer.next_token();

            assert_eq!(
                &tok.ttype, expected_type,
                "tests[{}] - token type wrong. expected={:?}, got={:?}",
                i, expected_type, tok.ttype
            );

            assert_eq!(
                &tok.literal, expected_literal,
                "tests[{}] - literal wrong. expected={}, got={}",
                i, expected_literal, tok.literal
            );
        }
    }
}
