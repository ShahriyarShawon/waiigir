use crate::TokenType;
use crate::ast;
use crate::ast::ExpressionStatement;
use crate::ast::IdentifierExpression;
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

fn precedences(t: &TokenType) -> Option<Precedence> {
    match t {
        TokenType::EQ | TokenType::NOT_EQ => Some(Precedence::EQUALS),
        TokenType::LT | TokenType::GT => Some(Precedence::LESSGREATER),
        TokenType::PLUS | TokenType::MINUS => Some(Precedence::SUM),
        TokenType::SLASH | TokenType::ASTERISK => Some(Precedence::PRODUCT),
        _ => None,
    }
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
            None => {
                self.no_prefix_parse_fn_error(self.cur_token.token_type.clone());
                return None;
            }
        };

        let mut left_exp = prefix(self);

        while !self.peek_token_is(&TokenType::SEMICOLON) && precendence < self.peek_precedence() {
            let infix = match self.infix_parse_fns(&self.peek_token.token_type) {
                Some(i) => i,
                None => return left_exp,
            };

            self.next_token();
            left_exp = infix(self, left_exp.expect("I HOPE SO"));
        }

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
                return None;
            }
        };

        lit.value = value;

        Some(ast::Expression::Integer(lit))
    }

    fn parse_prefix_expression(&mut self) -> Option<ast::Expression> {
        let mut expression = ast::PrefixExpression {
            token: self.cur_token.clone(),
            operator: self.cur_token.literal.clone(),
            ..Default::default()
        };

        self.next_token();

        expression.right = Box::new(
            self.parse_expression(Precedence::PREFIX)
                .expect("damn, you fucked up"),
        );

        Some(ast::Expression::Prefix(expression))
    }

    fn parse_infix_expression(&mut self, left: ast::Expression) -> Option<ast::Expression> {
        let mut expression = ast::InfixExpression {
            token: self.cur_token.clone(),
            operator: self.cur_token.literal.clone(),
            left: Box::new(left),
            ..Default::default()
        };

        let precendence = self.cur_precedence();
        self.next_token();
        expression.right = Box::new(self.parse_expression(precendence).expect("IDK MAN"));

        return Some(ast::Expression::Infix(expression));
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

    fn peek_precedence(&self) -> Precedence {
        match precedences(&self.peek_token.token_type) {
            Some(p) => p,
            None => Precedence::LOWEST,
        }
    }
    fn cur_precedence(&self) -> Precedence {
        match precedences(&self.cur_token.token_type) {
            Some(p) => p,
            None => Precedence::LOWEST,
        }
    }

    fn prefix_parse_fns(&self, t: &TokenType) -> Option<prefix_parse_fn> {
        match t {
            TokenType::IDENT => Some(Parser::parse_identifier),
            TokenType::INT => Some(Parser::parse_integer_literal),
            TokenType::BANG => Some(Parser::parse_prefix_expression),
            TokenType::MINUS => Some(Parser::parse_prefix_expression),
            _ => None,
        }
    }

    fn no_prefix_parse_fn_error(&mut self, t: TokenType) {
        let msg = format!("no prefix parse function for {:?} found", t);
        self.errors.push(msg);
    }
    fn infix_parse_fns(&self, t: &TokenType) -> Option<infix_parse_fn> {
        match t {
            TokenType::PLUS
            | TokenType::MINUS
            | TokenType::SLASH
            | TokenType::ASTERISK
            | TokenType::EQ
            | TokenType::NOT_EQ
            | TokenType::LT
            | TokenType::GT => Some(Parser::parse_infix_expression),
            _ => None,
        }
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

    fn test_integer_literal(expr: &ast::Expression, value: i64) -> bool {
        let integ = match expr {
            ast::Expression::Integer(il) => il,
            _ => {
                eprintln!("expr is not an IntegerLiteral, got={:?}", expr);
                return false;
            }
        };

        if integ.value != value {
            eprintln!("integ.value not {}, got={}", value, integ.value);
            return false;
        }

        if integ.token.literal != value.to_string() {
            eprintln!(
                "integ.token.literal not {}, got={}",
                value, integ.token.literal
            );
            return false;
        }

        true
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let prefix_tests = vec![("!5;", "!", 5i64), ("-15;", "-", 15i64)];

        for (input, operator, integer_value) in prefix_tests {
            let l = Lexer::new(input);
            let mut p = Parser::new(l);
            let program = p.parse_program();

            check_parser_errors(&p);

            assert_eq!(
                program.statements.len(),
                1,
                "program.statements does not contain 1 statement, got={}",
                program.statements.len()
            );

            let stmt = match &program.statements[0] {
                Statement::Expression(es) => es,
                _ => panic!(
                    "program.statements[0] is not an ExpressionStatement, got={:?}",
                    program.statements[0]
                ),
            };

            let exp = match &stmt.expression {
                Some(ast::Expression::Prefix(pe)) => pe,
                _ => panic!(
                    "stmt.expression is not a PrefixExpression, got={:?}",
                    stmt.expression
                ),
            };

            assert_eq!(
                exp.operator, operator,
                "exp.operator is not '{}', got={}",
                operator, exp.operator
            );

            test_integer_literal(&exp.right, integer_value);
        }
    }

    #[test]
    fn test_parsing_infix_expression() {
        let infix_tests = vec![
            ("5 + 5;", 5, "+", 5),
            ("5 - 5;", 5, "-", 5),
            ("5 * 5;", 5, "*", 5),
            ("5 / 5;", 5, "/", 5),
            ("5 > 5;", 5, ">", 5),
            ("5 < 5;", 5, "<", 5),
            ("5 == 5;", 5, "==", 5),
            ("5 != 5;", 5, "!=", 5),
        ];

        for (input, left_value, operator, right_value) in infix_tests {
            let mut l = lexer::Lexer::new(input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);

            if program.statements.len() != 1 {
                eprintln!(
                    "program.statements does no contain {} statements, got={}",
                    1,
                    program.statements.len()
                );
            }

            let stmt = match &program.statements[0] {
                ast::Statement::Expression(es) => es,
                _ => panic!(
                    "program.statements[0] is not ExpressionStatement. got {:?}",
                    program.statements[0]
                ),
            };

            let exp = match &stmt.expression {
                Some(ast::Expression::Infix(is)) => is,
                _ => panic!("exp is not InfixExpression got={:?}", stmt.expression),
            };

            if !test_integer_literal(&exp.right, right_value) {
                return;
            }

            if exp.operator != operator {
                panic!("exp.operator is not '{}' got={}", operator, exp.operator);
            }

            if !test_integer_literal(&exp.right, right_value) {
                return;
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
        ];

        for (input, expected) in tests {
            let l = lexer::Lexer::new(input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);

            let actual = program.to_string();
            if actual != expected {
                eprintln!("expected={}, got={}", expected, actual);
            }

        }
    }
}
