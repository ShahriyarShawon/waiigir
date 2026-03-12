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

#[allow(non_camel_case_types, clippy::upper_case_acronyms)]
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
    LBRACKET,
    RBRACKET,
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
    NOT_EQ,
    STRING
}
