use crate::token::*;

#[derive(Debug, Default)]
pub struct Lexer {
    input: Vec<char>,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut l = Lexer {
            input: input.chars().collect(),
            ..Default::default()
        };
        l.read_char();
        l
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input[self.read_position]
        }
    }
    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_identifier(&mut self) -> String {
        let p = self.position;
        while self.ch.is_alphabetic() {
            self.read_char();
        }
        self.input[p..self.position].iter().collect()
    }

    fn read_number(&mut self) -> String {
        let p = self.position;
        while self.ch.is_numeric() {
            self.read_char();
        }
        self.input[p..self.position].iter().collect()
    }

    fn read_string(&mut self) -> String {
        let p = self.position + 1;
        self.read_char();
        loop {
            if self.ch == '"' || self.ch == '\0' {
                self.read_char();
                break;
            }
            self.read_char();
        }
        let end = self.position - 1;
        if end <= p {
            String::new()
        } else {
            self.input[p..self.position - 1].iter().collect()
        }
    }

    fn lookup_ident(ident: &str) -> TokenType {
        match ident {
            "fn" => TokenType::FUNCTION,
            "let" => TokenType::LET,
            "if" => TokenType::IF,
            "else" => TokenType::ELSE,
            "true" => TokenType::TRUE,
            "false" => TokenType::FALSE,
            "return" => TokenType::RETURN,
            _ => TokenType::IDENT,
        }
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() {
            self.read_char();
        }
    }

    pub fn next_token(&mut self) -> Token {
        let mut t: Token = Default::default();

        self.skip_whitespace();

        match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    let cc = self.ch;
                    self.read_char();
                    t = Token::new(TokenType::EQ, cc.to_string() + &self.ch.to_string());
                } else {
                    t = Token::new(TokenType::ASSIGN, String::from(self.ch))
                }
            }
            ';' => t = Token::new(TokenType::SEMICOLON, String::from(self.ch)),
            '(' => t = Token::new(TokenType::LPAREN, String::from(self.ch)),
            ')' => t = Token::new(TokenType::RPAREN, String::from(self.ch)),
            ',' => t = Token::new(TokenType::COMMA, String::from(self.ch)),
            '+' => t = Token::new(TokenType::PLUS, String::from(self.ch)),
            '{' => t = Token::new(TokenType::LBRACE, String::from(self.ch)),
            '}' => t = Token::new(TokenType::RBRACE, String::from(self.ch)),
            '!' => {
                if self.peek_char() == '=' {
                    let cc = self.ch;
                    self.read_char();
                    t = Token::new(TokenType::NOT_EQ, cc.to_string() + &self.ch.to_string());
                } else {
                    t = Token::new(TokenType::BANG, String::from(self.ch))
                }
            }
            '-' => t = Token::new(TokenType::MINUS, String::from(self.ch)),
            '/' => t = Token::new(TokenType::SLASH, String::from(self.ch)),
            '*' => t = Token::new(TokenType::ASTERISK, String::from(self.ch)),
            '>' => t = Token::new(TokenType::GT, String::from(self.ch)),
            '<' => t = Token::new(TokenType::LT, String::from(self.ch)),
            '\0' => t = Token::new(TokenType::EOF, String::from("")),
            '"' => {
                t.literal = self.read_string();
                t.token_type = TokenType::STRING;
                return t;
            }

            _ => {
                if self.ch.is_alphabetic() {
                    t.literal = self.read_identifier();
                    t.token_type = Lexer::lookup_ident(&t.literal);
                    return t;
                } else if self.ch.is_numeric() {
                    t.token_type = TokenType::INT;
                    t.literal = self.read_number();
                    return t;
                } else {
                    t = Token::new(TokenType::ILLEGAL, String::from(self.ch))
                }
            }
        };

        self.read_char();
        t
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = "let five = 5;
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
\"foobar\"
\"foo bar\"
";

        let tests: Vec<(TokenType, &str)> = vec![
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
            (TokenType::STRING, "foobar"),
            (TokenType::STRING, "foo bar"),
            (TokenType::EOF, ""),
        ];
        let mut l = Lexer::new(input);

        for (i, (expected_type, expected_literal)) in tests.iter().enumerate() {
            let tok = l.next_token();

            assert_eq!(
                tok.token_type, *expected_type,
                "tests[{}] - token type wrong. expected={:?}, got={:?}",
                i, expected_type, tok.token_type
            );

            assert_eq!(
                tok.literal, *expected_literal,
                "tests[{}] - literal wrong. expected={:?}, got={:?}",
                i, expected_literal, tok.literal
            );
        }
    }
}
