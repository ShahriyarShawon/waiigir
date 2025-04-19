use crate::token::{self, Token};

pub trait NodeT {
    fn token_literal(&self) -> String;
}

pub trait StatementT: NodeT {
    fn statement_node(&self);
}

pub trait ExpressionT: NodeT {
    fn expression_node(&self);
}

#[derive(Debug)]
pub enum Expression {
    Identifier{id: Identifier},
    Default
}

#[derive(Debug)]
pub enum Statement {
    Let{ls: LetStatement}
}
impl NodeT for Statement {
    fn token_literal(&self) -> String {
        match self {
            Statement::Let{ls} => ls.token_literal()
        }
    }
}
impl StatementT for Statement {
    fn statement_node(&self) {
        match self {
            Statement::Let{ls} => ls.statement_node(),
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
                Statement::Let{ls} => ls.token_literal()
            }
        } else {
            return String::from("");
        }
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
}

impl Identifier {
    pub fn default() -> Identifier {
        Identifier {
            token: Token::default(),
            value: String::from("")
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
}
impl LetStatement {
    fn new() -> LetStatement {
        LetStatement {
            token: token::Token::default(),
            name: Identifier::default(),
            value: Expression::Default
        }

    }
}
