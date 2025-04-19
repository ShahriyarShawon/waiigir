use crate::ast::{Expression, Identifier, LetStatement, Program, Statement, NodeT};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

pub struct Parser {
    l: Lexer,
    cur_token: Token,
    peek_token: Token,
}

impl Parser {
    pub fn new(l: Lexer) -> Parser {
        let mut p = Parser {
            l,
            cur_token: Token::default(),
            peek_token: Token::default(),
        };
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
                Some(s) => program.statements.push(s),
                None => self.next_token(),
            }
        }

        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token.ttype {
            TokenType::LET => self.parse_let_statement(),
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
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
            self.next_token();
        }

        Some(Statement::Let { ls: stmt })
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
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Statement;

    use super::*;

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
            Statement::Let{ls} => {
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
}
