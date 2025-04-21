use crate::token::{self, Token};

pub trait NodeT {
    fn token_literal(&self) -> String;
    fn string(&self) -> String;
}

pub trait StatementT: NodeT {
    fn statement_node(&self);
}

pub trait ExpressionT: NodeT {
    fn expression_node(&self);
}

#[derive(Debug)]
pub enum Expression {
    Identifier { id: Identifier },
    Default,
}

impl Expression {
    fn string(&self) -> String {
        match self {
            Expression::Identifier { id } => id.string(),
            Expression::Default => String::from(""),
        }
    }
}

#[derive(Debug)]
pub enum Statement {
    Let { ls: LetStatement },
    Return { r: ReturnStatement },
    Expression { e: ExpressionStatement },
}
impl NodeT for Statement {
    fn token_literal(&self) -> String {
        match self {
            Statement::Let { ls } => ls.token_literal(),
            Statement::Return { r } => r.token_literal(),
            Statement::Expression { e } => e.token_literal(),
        }
    }

    fn string(&self) -> String {
        match self {
            Statement::Let { ls } => ls.string(),
            Statement::Return { r } => r.string(),
            Statement::Expression { e } => e.string(),
        }
    }
}
impl StatementT for Statement {
    fn statement_node(&self) {
        match self {
            Statement::Let { ls } => ls.statement_node(),
            Statement::Return { r } => r.statement_node(),
            Statement::Expression { e } => e.statement_node(),
        }
    }
}

pub struct Program {
    // pub statements: Vec<Box<dyn StatementT>>,
    pub statements: Vec<Statement>,
}

impl NodeT for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            let stmt = self.statements.get(0).unwrap();
            return match stmt {
                Statement::Let { ls } => ls.token_literal(),
                Statement::Return { r } => r.token_literal(),
                Statement::Expression { e } => e.token_literal(),
            };
        } else {
            return String::from("");
        }
    }

    fn string(&self) -> String {
        let mut out = String::from("");
        for s in &self.statements {
            out += s.string().as_str();
        }

        return out;
    }
}

#[derive(Debug)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl ExpressionT for Identifier {
    fn expression_node(&self) {}
}

impl NodeT for Identifier {
    fn token_literal(&self) -> String {
        return self.token.literal.clone();
    }

    fn string(&self) -> String {
        self.value.clone()
    }
}

impl Identifier {
    pub fn default() -> Identifier {
        Identifier {
            token: Token::default(),
            value: String::from(""),
        }
    }
}

#[derive(Debug)]
pub struct LetStatement {
    pub token: token::Token,
    pub name: Identifier,
    pub value: Expression,
}

impl StatementT for LetStatement {
    fn statement_node(&self) {}
}

impl NodeT for LetStatement {
    fn token_literal(&self) -> String {
        return self.token.literal.clone();
    }

    fn string(&self) -> String {
        let mut out = String::from("");

        out += self.token_literal().as_str();
        out += " ";
        out += self.name.string().as_str();
        out += " = ";
        out += self.value.string().as_str();
        out += ";";

        return out;
    }
}
impl LetStatement {
    fn new() -> LetStatement {
        LetStatement {
            token: token::Token::default(),
            name: Identifier::default(),
            value: Expression::Default,
        }
    }
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Expression,
}

impl StatementT for ReturnStatement {
    fn statement_node(&self) {}
}

impl NodeT for ReturnStatement {
    fn token_literal(&self) -> String {
        return self.token.literal.clone();
    }

    fn string(&self) -> String {
        let mut out = String::from("");

        out += self.token_literal().as_str();
        out += " ";
        out += self.return_value.string().as_str();
        out += ";";

        return out;
    }
}
impl ReturnStatement {
    pub fn new() -> ReturnStatement {
        ReturnStatement {
            token: token::Token::default(),
            return_value: Expression::Default,
        }
    }
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

impl StatementT for ExpressionStatement {
    fn statement_node(&self) {}
}

impl NodeT for ExpressionStatement {
    fn token_literal(&self) -> String {
        return self.token.literal.clone();
    }

    fn string(&self) -> String {
        return self.expression.string();
    }
}
impl ExpressionStatement {
    pub fn new() -> ExpressionStatement {
        ExpressionStatement {
            token: token::Token::default(),
            expression: Expression::Default,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::TokenType;

    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![
                Statement::Let{ls: LetStatement {
                    token: Token {
                        ttype: TokenType::LET,
                        literal: "let".to_string(),
                    },
                    name: Identifier {
                        token: Token {
                            ttype: TokenType::IDENT,
                            literal: "myVar".to_string(),
                        },
                        value: "myVar".to_string(),
                    },
                    value: Expression::Identifier{id: Identifier {
                        token: Token {
                            ttype: TokenType::IDENT,
                            literal: "anotherVar".to_string(),
                        },
                        value: "anotherVar".to_string(),
                    }},
                }},
            ],
        };

        let expected = "let myVar = anotherVar;";
        let result = program.string();

        assert_eq!(
            result, expected,
            "program.to_string() wrong. got='{}'",
            result
        );
    }
}
