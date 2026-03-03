use crate::token;

pub trait Node {
    fn token_literal(&self) -> String;
    fn to_string(&self) -> String;
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn to_string(&self) -> String {
        let mut out = String::new();
        for s in &self.statements {
            out += &s.to_string();

        }

        out
    }
}

#[derive(Debug)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

#[allow(dead_code)]
impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Statement::Let(s) => s.token.literal.clone(),
            Statement::Return(s) => s.token.literal.clone(),
            Statement::Expression(s) => s.token.literal.clone(),
        }
    }

    fn to_string(&self) -> String {
        match self {
            Statement::Let(s) => {
                let mut out = String::new();
                out += &format!("{} {} = ", &s.token.literal.clone(), s.name.to_string());

                match &s.value {
                    Some(v) => out += &v.to_string(),
                    None => {}
                }

                out += ";";
                out
            }
            Statement::Return(s) => {
                let mut out = String::new();
                out += &format!("{} ", s.token.literal.clone());

                match &s.return_value {
                    Some(v) => out += &v.to_string(),
                    None => {}
                }
                out += ";";

                out
            }
            Statement::Expression(s) => match &s.expression {
                Some(thing) => thing.to_string(),
                None => String::new(),
            },
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Default)]
pub struct LetStatement {
    pub token: token::Token,
    pub name: IdentifierExpression,
    pub value: Option<Expression>,
}

#[allow(dead_code)]
#[derive(Debug, Default)]
pub struct ReturnStatement {
    pub token: token::Token,
    pub return_value: Option<Expression>,
}

#[allow(dead_code)]
#[derive(Debug, Default)]
pub struct ExpressionStatement {
    pub token: token::Token,
    pub expression: Option<Expression>,
}

#[derive(Debug, Default)]
pub enum Expression {
    #[default]
    Default,
    Identifier(IdentifierExpression),
    Integer(IntegerLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression)
}

#[allow(dead_code)]
impl Node for Expression {
    fn token_literal(&self) -> String {
        todo!()
    }

    fn to_string(&self) -> String {
        match self {
            Expression::Identifier(ie) => return ie.value.clone(),
            Expression::Integer(ie) => return ie.to_string(),
            Expression::Prefix(pe) => return pe.to_string(),
            Expression::Infix(ie) => return ie.to_string(),
            _ => todo!()
        }
    }
}

#[allow(dead_code)]
impl Program {
    pub fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            return self.statements[0].token_literal();
        } else {
            return String::new();
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Default)]
pub struct IdentifierExpression {
    pub token: token::Token,
    pub value: String,
}

#[allow(dead_code)]
impl Node for IdentifierExpression {
    fn token_literal(&self) -> String {
        return self.token.literal.clone();
    }

    fn to_string(&self) -> String {
        self.value.clone()
    }
}

#[allow(dead_code)]
#[derive(Debug, Default)]
pub struct PrefixExpression {
    pub token: token::Token,
    pub operator: String,
    pub right: Box<Expression>
}

#[allow(dead_code)]
impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        return self.token.literal.clone();
    }

    fn to_string(&self) -> String {
        let mut out = String::new();
        out += &format!("({}{})", self.operator, self.right.to_string());
        out
    }
}

#[allow(dead_code)]
#[derive(Debug, Default)]
pub struct InfixExpression {
    pub token: token::Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>
}

#[allow(dead_code)]
impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        return self.token.literal.clone();
    }

    fn to_string(&self) -> String {
        let mut out = String::new();
        out += &format!("({} {} {})", self.left.to_string(), self.operator, self.right.to_string());
        out
    }
}

#[allow(dead_code)]
#[derive(Debug, Default)]
pub struct IntegerLiteral {
    pub token: token::Token,
    pub value: i64,
}

#[allow(dead_code)]
impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        return self.token.literal.clone();
    }

    fn to_string(&self) -> String {
        self.token.literal.clone()
    }
}



#[test]
fn test_string() {
    let program = Program {
        statements: vec![Statement::Let(LetStatement {
            token: token::Token {
                token_type: token::TokenType::LET,
                literal: String::from("let"),
            },
            name: IdentifierExpression {
                token: token::Token {
                    token_type: token::TokenType::IDENT,
                    literal: String::from("myVar"),
                },
                value: String::from("myVar"),
            },
            value: Some(Expression::Identifier(IdentifierExpression{
                token: token::Token {
                    token_type: token::TokenType::IDENT,
                    literal: String::from("anotherVar"),
                },
                value: String::from("anotherVar"),
            })),
        })],
    };

    eprintln!("THIS IS WHAT I GOT FOR PROGRAM {}", program.to_string());
    assert_eq!(
        program.to_string(),
        "let myVar = anotherVar;",
        "program.to_string() wrong. got={}",
        program.to_string()
    );
}
