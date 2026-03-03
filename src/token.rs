#[derive(Debug, Default, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(_token_type: TokenType, _literal: String) -> Self {
        Token {
            token_type: _token_type,
            literal: _literal.clone(),
        }
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Default, PartialEq, Clone)]
pub enum TokenType {
    #[default]
    DEFAULT,
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
    LT,
    GT,
    RETURN,
    IF,
    ELSE,
    TRUE,
    FALSE,
    EQ, 
    NOT_EQ
}

// pub static ILLEGAL: &str = "ILLEGAL";
// pub static EOF: &str = "EOF";
// pub static IDENT: &str = "IDENT";
// pub static INT: &str = "INT";
// pub static ASSIGN: &str = "=";
// pub static PLUS: &str = "+";
// pub static COMMA: &str = ",";
// pub static SEMICOLON: &str = ";";
// pub static LPAREN: &str = "(";
// pub static RPAREN: &str = ")";
// pub static LBRACE: &str = "{";
// pub static RBRACE: &str = "}";
// pub static FUNCTION: &str = "FUNCTION";
// pub static LET: &str = "LET";
