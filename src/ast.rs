use crate::token;
use std::fmt;

pub struct Program {
    pub statements: Vec<Statement>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let params: Vec<String> = self.statements.iter().map(|p| p.to_string()).collect();

        write!(f, "{}", params.join("\n"))
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let(s) => {
                let val = if let Some(v) = &s.value {
                    &v.to_string()
                } else {
                    ""
                };
                write!(f, "{} {} = {};", &s.token.literal.clone(), s.name, val)
            }
            Statement::Return(s) => {
                // let mut out = String::new();
                // out += &format!("{} ", s.token.literal.clone());
                //
                // if let Some(v) = &s.return_value {
                //     out += &v.to_string()
                // }
                // out += ";";
                //
                // out;
                let val = match &s.return_value {
                    Some(v) => v.to_string(),
                    None => "".to_string(),
                };
                write!(f, "{} {};", s.token.literal.clone(), val)
            }
            Statement::Expression(s) => match &s.expression {
                Some(thing) => write!(f, "{}", thing),
                None => write!(f, ""),
            },
        }
    }
}

#[allow(dead_code)]
impl Statement {
    pub fn token_literal(&self) -> String {
        match self {
            Statement::Let(s) => s.token.literal.clone(),
            Statement::Return(s) => s.token.literal.clone(),
            Statement::Expression(s) => s.token.literal.clone(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Default, Clone)]
pub struct LetStatement {
    pub token: token::Token,
    pub name: IdentifierExpression,
    pub value: Option<Expression>,
}

#[allow(dead_code)]
#[derive(Debug, Default, Clone)]
pub struct ReturnStatement {
    pub token: token::Token,
    pub return_value: Option<Expression>,
}

#[allow(dead_code)]
#[derive(Debug, Default, Clone)]
pub struct ExpressionStatement {
    pub token: token::Token,
    pub expression: Option<Expression>,
}

#[allow(dead_code)]
#[derive(Debug, Default, Clone)]
pub struct BlockStatement {
    pub token: token::Token,
    pub statements: Vec<Statement>,
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let params: Vec<String> = self.statements.iter().map(|p| p.to_string()).collect();

        write!(f, "{}", params.join("\n"))
    }
}

#[derive(Debug, Default, Clone)]
pub enum Expression {
    #[default]
    Default,
    Identifier(IdentifierExpression),
    Integer(IntegerLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Boolean(BooleanExpression),
    If(IfExpression),
    Function(FunctionLiteral),
    Call(CallExpression),
    String(StringLiteral),
    Array(ArrayLiteral),
    Index(IndexExpression),
    Hash(HashLiteral),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = match self {
            Expression::Identifier(ie) => ie.value.clone(),
            Expression::Integer(ie) => ie.to_string(),
            Expression::Prefix(pe) => pe.to_string(),
            Expression::Infix(ie) => ie.to_string(),
            Expression::Boolean(ie) => ie.to_string(),
            Expression::If(ie) => ie.to_string(),
            Expression::Function(ie) => ie.to_string(),
            Expression::Call(ie) => ie.to_string(),
            Expression::String(se) => se.to_string(),
            Expression::Array(al) => al.to_string(),
            Expression::Index(ie) => ie.to_string(),
            Expression::Hash(he) => he.to_string(),
            _ => todo!(),
        };

        write!(f, "{}", res)
    }
}

#[allow(dead_code)]
impl Program {
    pub fn token_literal(&self) -> String {
        if !self.statements.is_empty() {
            self.statements[0].token_literal()
        } else {
            String::new()
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Default, Clone)]
pub struct IdentifierExpression {
    pub token: token::Token,
    pub value: String,
}

// Used for testing
#[allow(dead_code)]
impl IdentifierExpression {
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl fmt::Display for IdentifierExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[allow(dead_code)]
#[derive(Debug, Default, Clone)]
pub struct PrefixExpression {
    pub token: token::Token,
    pub operator: String,
    pub right: Box<Expression>,
}

impl fmt::Display for PrefixExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

#[allow(dead_code)]
#[derive(Debug, Default, Clone)]
pub struct InfixExpression {
    pub token: token::Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

impl fmt::Display for InfixExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

// TODO: Remove
// #[allow(dead_code)]
// impl Node for InfixExpression {
//     fn token_literal(&self) -> String {
//         self.token.literal.clone()
//     }
//
//     fn to_string(&self) -> String {
//         let mut out = String::new();
//         out += &format!(
//             "({} {} {})",
//             self.left.to_string(),
//             self.operator,
//             self.right.to_string()
//         );
//         out
//     }
// }

#[allow(dead_code)]
#[derive(Debug, Default, Clone)]
pub struct IntegerLiteral {
    pub token: token::Token,
    pub value: i64,
}
impl fmt::Display for IntegerLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.token.literal.clone())
    }
}

#[allow(dead_code)]
#[derive(Debug, Default, Clone)]
pub struct BooleanExpression {
    pub token: token::Token,
    pub value: bool,
}

impl fmt::Display for BooleanExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.token.literal.clone())
    }
}

#[allow(dead_code)]
#[derive(Debug, Default, Clone)]
pub struct IfExpression {
    pub token: token::Token,
    pub condition: Box<Expression>,
    pub consequence: Box<BlockStatement>,
    pub alternative: Option<Box<BlockStatement>>,
}

impl fmt::Display for IfExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let alt = match &self.alternative {
            Some(v) => &format!("else {}", v),
            _ => "",
        };
        write!(f, "if {} {} {}", self.condition, self.consequence, alt)
    }
}

#[allow(dead_code)]
#[derive(Debug, Default, Clone)]
pub struct FunctionLiteral {
    pub token: token::Token,
    pub parameters: Vec<IdentifierExpression>,
    pub body: BlockStatement,
}

impl fmt::Display for FunctionLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let params: Vec<String> = self.parameters.iter().map(|p| p.to_string()).collect();

        write!(f, "fn({}){}", params.join(", "), self.body)
    }
}

#[allow(dead_code)]
#[derive(Debug, Default, Clone)]
pub struct CallExpression {
    pub token: token::Token,
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl fmt::Display for CallExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}({})",
            self.function,
            self.arguments
                .iter()
                .map(|a| a.to_string())
                .collect::<Vec<String>>()
                .join(" ")
        )
    }
}

// #[allow(dead_code)]
// impl Node for CallExpression {
//     fn token_literal(&self) -> String {
//         self.token.literal.clone()
//     }
//
//     fn to_string(&self) -> String {
//         let mut out = String::new();
//         let mut args: Vec<String> = Vec::new();
//
//         for a in &self.arguments {
//             args.push(a.to_string());
//         }
//
//         out += &format!("{}({})", self.function.to_string(), args.join(" "));
//
//         out
//     }
// }

#[allow(dead_code)]
#[derive(Debug, Default, Clone)]
pub struct StringLiteral {
    pub token: token::Token,
    pub value: String,
}

impl fmt::Display for StringLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.token.literal.clone())
    }
}

#[allow(dead_code)]
#[derive(Debug, Default, Clone)]
pub struct ArrayLiteral {
    pub token: token::Token,
    pub elements: Vec<Expression>,
}

impl fmt::Display for ArrayLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let elements = self
            .elements
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "[{}]", elements)
    }
}

#[allow(dead_code)]
#[derive(Debug, Default, Clone)]
pub struct IndexExpression {
    pub token: token::Token,
    pub left: Box<Expression>,
    pub index: Box<Expression>,
}

impl fmt::Display for IndexExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}[{}])", self.left, self.index)
    }
}

#[allow(dead_code)]
#[derive(Debug, Default, Clone)]
pub struct HashLiteral {
    pub token: token::Token,
    pub pairs: Vec<(Expression, Expression)>,
}

impl fmt::Display for HashLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut pair_string: Vec<String> = Vec::new();

        for (key, value) in self.pairs.iter() {
            pair_string.push(format!("{}:{}", key, value));
        }

        write!(f, "{{ {} }}", pair_string.join(", "))
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
            value: Some(Expression::Identifier(IdentifierExpression {
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
