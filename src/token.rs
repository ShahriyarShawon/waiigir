#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(token_type: TokenType, literal: String) -> Self {
        Token {
            token_type,
            literal,
        }
    }
}

#[allow(non_camel_case_types, clippy::upper_case_acronyms)]
#[derive(Debug, Default, PartialEq, Clone, Eq, Hash)]
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
    COLON,
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
    STRING,
}
