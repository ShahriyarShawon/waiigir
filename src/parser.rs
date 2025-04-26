use std::collections::HashMap;

use crate::ast::{
    Expression, ExpressionStatement, Identifier, LetStatement, NodeT, Program, ReturnStatement,
    Statement,
};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

pub type PrefixParseFn = Box<dyn Fn(&Parser) -> Expression>;
pub type InfixParseFn = fn(Expression) -> Expression;

enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

pub struct Parser {
    l: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
}

impl Parser {
    pub fn new(l: Lexer) -> Parser {
        let mut p = Parser {
            l,
            cur_token: Token::default(),
            peek_token: Token::default(),
            errors: Vec::new(),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        p.register_prefix(TokenType::IDENT, Box::new(Parser::parse_identifier));

        p.next_token();
        p.next_token();
        return p;
    }

    fn next_token(&mut self) {
        self.cur_token = std::mem::replace(&mut self.peek_token, self.l.next_token())
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program {
            statements: Vec::new(),
        };

        while self.cur_token.ttype != TokenType::EOF {
            let stmt = self.parse_statement();
            match stmt {
                Some(s) => {
                    // eprintln!("parse_program: pushing {:?}", s);
                    program.statements.push(s);
                }
                None => {
                    // eprintln!("NEXT TOKEN");
                    self.next_token();
                }
            }
        }

        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        if self.cur_token.ttype == TokenType::SEMICOLON{
            return None
        }
        match self.cur_token.ttype {
            TokenType::LET => self.parse_let_statement(),
            TokenType::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        // eprintln!("parse_let_statement:");
        let mut stmt = LetStatement {
            token: self.cur_token.clone(),
            name: Identifier::default(),
            value: Expression::Default,
        };

        if !self.expect_peek(TokenType::IDENT) {
            return None;
        }

        stmt.name = Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        };

        if !self.expect_peek(TokenType::ASSIGN) {
            return None;
        }

        // TODO: We're skipping the expressions until we
        // encounter a semicolon
        while !self.cur_token_is(TokenType::SEMICOLON) {
            // eprintln!("cur token is {:?}, onto the next token.", {self.cur_token.clone()});
            self.next_token();
        }

        Some(Statement::Let { ls: stmt })
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let mut stmt = ReturnStatement::new();
        stmt.token = self.cur_token.clone();
        self.next_token();

        while !self.cur_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }
        Some(Statement::Return { r: stmt })
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let mut stmt = ExpressionStatement::new();
        stmt.token = self.cur_token.clone();

        stmt.expression = self.parse_expression(Precedence::Lowest);

        if self.peek_token_is(TokenType::EOF) {
            return None;
        }
        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        return Some(Statement::Expression { e: stmt });
    }

    fn parse_expression(&mut self, _precedence: Precedence) -> Option<Expression> {
        let prefix = self.prefix_parse_fns.get(&self.cur_token.ttype);
        match prefix {
            Some(pf) => {
                let left_exp = pf(self);
                return Some(left_exp);
            }
            None => None,
        }
    }

    fn parse_identifier(p: &Parser) -> Expression {
        Expression::Identifier {
            id: Identifier {
                token: p.cur_token.clone(),
                value: p.cur_token.literal.clone(),
            },
        }
    }

    fn cur_token_is(&self, t: TokenType) -> bool {
        self.cur_token.ttype == t
    }
    fn peek_token_is(&self, t: TokenType) -> bool {
        self.peek_token.ttype == t
    }

    fn expect_peek(&mut self, t: TokenType) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            true
        } else {
            self.peek_error(t);
            false
        }
    }

    fn errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    fn peek_error(&mut self, t: TokenType) {
        let msg = format!(
            "Expected next token to be {:?}, got {:?} instead",
            t, self.peek_token.ttype
        );
        self.errors.push(msg);
    }

    fn register_prefix(&mut self, tt: TokenType, f: PrefixParseFn) {
        self.prefix_parse_fns.insert(tt, f);
    }
    fn register_infix(&mut self, tt: TokenType, f: InfixParseFn) {
        self.infix_parse_fns.insert(tt, f);
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Statement;

    use super::*;

    fn check_parser_errors(p: &Parser) {
        let err_count = p.errors.len();
        if err_count == 0 {
            return;
        }

        eprintln!("Parser has {} errors.", err_count);
        for e in p.errors() {
            eprintln!("parser error: {}", e);
        }
        panic!("Got parser errors")
    }

    #[test]
    fn test_let_statements() {
        let input = r#"
let x = 5;
let y = 10;
let foobar = 838383;
"#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        // assert!(program.is_some(), "parse_program() returned None");
        // let program = program.unwrap();

        assert_eq!(
            program.statements.len(),
            3,
            "program.statements does not contain 3 statements. got={}",
            program.statements.len()
        );

        let tests = vec!["x", "y", "foobar"];

        for (i, expected_ident) in tests.iter().enumerate() {
            let stmt = &program.statements[i];
            assert!(
                test_let_statement(stmt, expected_ident),
                "test_let_statement failed for statement [{}]",
                i
            );
        }
    }

    fn test_let_statement(stmt: &Statement, expected_name: &str) -> bool {
        if stmt.token_literal() != "let" {
            eprintln!("stmt.token_literal not 'let'. got={}", stmt.token_literal());
            return false;
        }

        match stmt {
            Statement::Let { ls } => {
                if ls.name.value != expected_name {
                    eprintln!(
                        "ls.name.value not '{}'. got='{}'",
                        expected_name, ls.name.value
                    );
                    return false;
                }

                if ls.name.token_literal() != expected_name {
                    eprintln!(
                        "ls.name.token_literal() not '{}'. got='{}'",
                        expected_name,
                        ls.name.token_literal()
                    );
                    return false;
                }

                true
            }
            _ => {
                eprintln!("stmt is not a LetStatement. got={:?}", stmt);
                false
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let input = r#"
return 5;
return 10;
return 993322;
"#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(
            program.statements.len(),
            3,
            "program.statements does not contain 3 statements. got={}",
            program.statements.len()
        );

        for stmt in &program.statements {
            match stmt {
                Statement::Return { r } => {
                    assert_eq!(
                        r.token_literal(),
                        "return",
                        "r.token_literal not 'return'. got='{}'",
                        r.token_literal()
                    );
                }
                _ => {
                    panic!("stmt not ReturnStatement. got={:?}", stmt);
                }
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = r#"foobar;"#;
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);

        assert_eq!(
            program.statements.len(),
            1,
            "program has not enough statements. got={}",
            program.statements.len()
        );

        let stmt = match &program.statements[0] {
            Statement::Expression { e } => e,
            other => panic!(
                "program.statements[0] is not ExpressionStatement. got={:?}",
                other
            ),
        };

        let ident = match &stmt.expression {
            Some(e) => match e {
                Expression::Identifier { id } => id,
                other => panic!("exp not Identifier. got={:?}", other),
            },
            None => panic!("empty exp"),
        };

        assert_eq!(
            ident.value, "foobar",
            "ident.value not 'foobar'. got={}",
            ident.value
        );
        assert_eq!(
            ident.token.literal, "foobar",
            "ident.token.literal not 'foobar'. got={}",
            ident.token.literal
        );
    }
}
