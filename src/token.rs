#[derive(Debug, Clone)]
pub struct Token {
    pub ttype: TokenType,
    pub literal: String,
}

impl Token {
    pub fn default() -> Token {
        Token {
            ttype: TokenType::DEFAULT,
            literal: String::from("")
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum TokenType {
    ILLEGAL,
    EOF,
    IDENT,
    INT,
    ASSIGN,
    PLUS,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    FUNCTION,
    LET,
    BANG,
    MINUS,
    SLASH,
    ASTERISK,
    GT,
    LT,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
    EQ,
    NOT_EQ,
    DEFAULT
}

// const ILLEGAL: &str = "ILLEGAL";
// const EOF: &str = "EOF";
// const IDENT: &str = "IDENT";
// const INT: &str = "INT";
// const ASSIGN: &str = "ASSIGN";
// const PLUS: &str = "PLUS";
// const COMMA: &str = "COMMA";
// const SEMICOLON: &str = "SEMICOLON";
// const LPAREN: &str = "LPAREN";
// const RPAREN: &str = "RPAREN";
// const LBRACE: &str = "LBRACE";
// const RBRACE: &str = "RBRACE";
// const FUNCTION: &str = "FUNCTION";
// const LET: &str = "LET";
