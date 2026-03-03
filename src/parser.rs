use std::fmt::format;

use crate::TokenType;
use crate::ast;
use crate::ast::ExpressionStatement;
use crate::ast::IdentifierExpression;
use crate::ast::IntegerLiteral;
use crate::ast::LetStatement;
use crate::ast::ReturnStatement;
use crate::lexer;
use crate::token;

type prefix_parse_fn = fn(&mut Parser) -> Option<ast::Expression>;
type infix_parse_fn = fn(&mut Parser, ast::Expression) -> Option<ast::Expression>;

#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub enum Precedence {
    LOWEST,
    EQUALS,      // ==
    LESSGREATER, // > or
    SUM,         // +
    PRODUCT,     // *
    PREFIX,      // -X or !X
    CALL,        // myFunction(X)
}

#[allow(dead_code)]
#[derive(Debug, Default)]
pub struct Parser {
    l: lexer::Lexer,
    cur_token: token::Token,
    peek_token: token::Token,
    pub errors: Vec<String>,
}

#[allow(dead_code)]
impl Parser {
    pub fn new(_l: lexer::Lexer) -> Self {
        let mut p = Parser {
            l: _l,
            ..Default::default()
        };

        p.next_token();
        p.next_token();

        return p;
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    fn parse_program(&mut self) -> ast::Program {
        let mut program = ast::Program {
            statements: Vec::new(),
        };

        while self.cur_token.token_type != token::TokenType::EOF {
            let stmt = self.parse_statement();
            match stmt {
                Some(s) => program.statements.push(s),
                None => {}
            }
            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Option<ast::Statement> {
        match self.cur_token.token_type {
            token::TokenType::LET => match self.parse_let_statement() {
                Some(ls) => Some(ast::Statement::Let(ls)),
                None => None,
            },
            token::TokenType::RETURN => match self.parse_return_statement() {
                Some(rs) => Some(ast::Statement::Return(rs)),
                None => None,
            },
            _ => match self.parse_expression_statement() {
                Some(es) => Some(ast::Statement::Expression(es)),
                None => None,
            },
        }
    }

    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        let mut stmt = ast::LetStatement {
            token: self.cur_token.clone(),
            ..Default::default()
        };
        if !self.expect_peek(&TokenType::IDENT) {
            return None;
        }

        stmt.name = ast::IdentifierExpression {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        };

        // skip expressions
        while !self.cur_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(stmt)
    }

    fn parse_return_statement(&mut self) -> Option<ReturnStatement> {
        let stmt = ast::ReturnStatement {
            token: self.cur_token.clone(),
            return_value: None,
        };

        self.next_token();

        while !self.cur_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(stmt)
    }

    fn parse_expression_statement(&mut self) -> Option<ExpressionStatement> {
        let mut stmt = ExpressionStatement {
            token: self.cur_token.clone(),
            ..Default::default()
        };

        stmt.expression = self.parse_expression(Precedence::LOWEST);
        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(stmt)
    }

    fn parse_expression(&mut self, precendence: Precedence) -> Option<ast::Expression> {
        let prefix = match self.prefix_parse_fns(&self.cur_token.token_type) {
            Some(p) => p,
            None => return None,
        };

        let left_exp = prefix(self);
        return left_exp;
    }

    fn parse_identifier(&mut self) -> Option<ast::Expression> {
        return Some(ast::Expression::Identifier(IdentifierExpression {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        }));
    }

    fn parse_integer_literal(&mut self) -> Option<ast::Expression> {
        let mut lit = ast::IntegerLiteral {
            token: self.cur_token.clone(),
            ..Default::default()
        };

        let value = match lit.token.literal.parse::<i64>() {
           Ok(v) => v,
           Err(_) => {
               let msg = format!("could not parse {} as integer", lit.token.literal);
               self.errors.push(msg);
               return None
           }
        };

        lit.value = value;

        Some(ast::Expression::Integer(lit))
    }

    fn cur_token_is(&mut self, t: TokenType) -> bool {
        self.cur_token.token_type == t
    }

    fn peek_token_is(&mut self, t: &TokenType) -> bool {
        &self.peek_token.token_type == t
    }

    fn expect_peek(&mut self, t: &TokenType) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            true
        } else {
            self.peek_error(t);
            false
        }
    }

    pub fn peek_error(&mut self, t: &TokenType) {
        let msg = format!(
            "expected next token to be {:?}, got {:?} instead",
            t, self.peek_token.token_type
        );
        self.errors.push(msg);
    }

    fn prefix_parse_fns(&self, t: &TokenType) -> Option<prefix_parse_fn> {
        match t {
            TokenType::IDENT => Some(Parser::parse_identifier),
            TokenType::INT=> Some(Parser::parse_integer_literal),
            _ => None,
        }
    }
    fn infix_parse_fns(&self, t: &TokenType) -> Option<infix_parse_fn> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use self::ast::Node;

    use super::*;
    use crate::ast::Statement;
    use crate::lexer::Lexer;

    fn check_parser_errors(p: &Parser) {
        if p.errors.len() == 0 {
            return;
        }

        eprintln!("Parser has {} errors", p.errors.len());
        for e in &p.errors {
            eprintln!("parser error: {}", e);
        }
        // panic!("Parser had errors");
    }

    fn test_let_statement(s: &Statement, name: &str) -> bool {
        if s.token_literal() != "let" {
            eprintln!("token_literal not 'let', got={}", s.token_literal());
            return false;
        }

        // downcast to LetStatement
        let let_stmt = match s {
            Statement::Let(ls) => ls,
            _ => {
                eprintln!("s is not a LetStatement");
                return false;
            }
        };

        if let_stmt.name.value != name {
            eprintln!(
                "let_stmt.name.value not '{}', got={}",
                name, let_stmt.name.value
            );
            return false;
        }

        if let_stmt.name.token_literal() != name {
            eprintln!(
                "let_stmt.name.token_literal() not '{}', got={}",
                name,
                let_stmt.name.token_literal()
            );
            return false;
        }

        return true;
    }

    #[test]
    fn test_let_statements() {
        let input = "
let x = 5;
let y = 10;
let foobar = 838383;
";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(
            program.statements.len(),
            3,
            "program.statements does not contain 3 statements, got={}",
            program.statements.len()
        );

        let expected = vec!["x", "y", "foobar"];

        for (i, expected_ident) in expected.iter().enumerate() {
            let stmt = &program.statements[i];
            if !test_let_statement(stmt, expected_ident) {
                panic!("test_let_statement failed for index {}", i);
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let input = "
return 5;
return 10;
return 993322;
";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();

        check_parser_errors(&p);

        assert_eq!(
            program.statements.len(),
            3,
            "program.statements does not contain 3 statements, got={}",
            program.statements.len()
        );

        for stmt in &program.statements {
            match stmt {
                Statement::Return(return_stmt) => {
                    assert_eq!(
                        return_stmt.token.literal, "return",
                        "return_stmt.token.literal not 'return', got={}",
                        return_stmt.token.literal
                    );
                }
                _ => panic!("stmt is not a ReturnStatement, got={:?}", stmt),
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();

        check_parser_errors(&p);

        assert_eq!(
            program.statements.len(),
            1,
            "program has not enough statements, got={}",
            program.statements.len()
        );

        let stmt = match &program.statements[0] {
            Statement::Expression(es) => es,
            _ => panic!(
                "program.statements[0] is not an ExpressionStatement, got={:?}",
                program.statements[0]
            ),
        };

        let ident = match &stmt.expression {
            Some(ast::Expression::Identifier(i)) => i,
            _ => panic!(
                "stmt.expression is not an Identifier, got={:?}",
                stmt.expression
            ),
        };

        assert_eq!(
            ident.value, "foobar",
            "ident.value not 'foobar', got={}",
            ident.value
        );

        assert_eq!(
            ident.token.literal, "foobar",
            "ident.token_literal not 'foobar', got={}",
            ident.token.literal
        );
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();

        check_parser_errors(&p);

        assert_eq!(
            program.statements.len(),
            1,
            "program has not enough statements, got={}",
            program.statements.len()
        );

        let stmt = match &program.statements[0] {
            Statement::Expression(es) => es,
            _ => panic!(
                "program.statements[0] is not an ExpressionStatement, got={:?}",
                program.statements[0]
            ),
        };

        let literal = match &stmt.expression {
            Some(ast::Expression::Integer(il)) => il,
            _ => panic!(
                "stmt.expression is not an IntegerLiteral, got={:?}",
                stmt.expression
            ),
        };

        assert_eq!(
            literal.value, 5,
            "literal.value not 5, got={}",
            literal.value
        );

        assert_eq!(
            literal.token.literal, "5",
            "literal.token.literal not '5', got={}",
            literal.token.literal
        );
    }
}
