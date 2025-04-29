use crate::ast::{
    Expression, ExpressionStatement, Identifier, InfixExpression, IntegerLiteral, LetStatement,
    NodeT, PrefixExpression, Program, ReturnStatement, Statement,
};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

fn precedences_for_token(tt: TokenType) -> Precedence {
    match tt {
        TokenType::EQ | TokenType::NOT_EQ => Precedence::Equals,
        TokenType::LT | TokenType::GT => Precedence::LessGreater,
        TokenType::PLUS | TokenType::MINUS => Precedence::Sum,
        TokenType::SLASH | TokenType::ASTERISK => Precedence::Product,
        _ => Precedence::Lowest,
    }
}

pub struct Parser {
    l: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl Parser {
    pub fn new(l: Lexer) -> Parser {
        let mut p = Parser {
            l,
            cur_token: Token::default(),
            peek_token: Token::default(),
            errors: Vec::new(),
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
            eprintln!("PARSE_PROGRAM: {:?}", stmt);
            match stmt {
                Some(s) => {
                    eprintln!("PUSHING STATEMENT {:?}", s);
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
        if self.cur_token.ttype == TokenType::SEMICOLON {
            return None;
        }
        let stmt = match self.cur_token.ttype {
            TokenType::LET => self.parse_let_statement(),
            TokenType::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        };
        eprintln!("PARSE_STATEMENT: {:?}", stmt);
        stmt
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
        eprintln!("Parsing expression statement: {:?}", stmt);

        if self.peek_token_is(TokenType::EOF) {
            // return None;
            self.next_token();
        }
        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        return Some(Statement::Expression { e: stmt });
    }

    fn no_prefix_parse_fn_error(&mut self, token: TokenType) {
        let msg = format!("No prefix parse function found for {:?}", token);
        self.errors.push(msg);
    }

    fn no_infix_parse_fn_error(&mut self, token: TokenType) {
        let msg = format!("No infix parse function found for {:?}", token);
        self.errors.push(msg);
    }

    fn prefix_parse_fn(&mut self) -> Option<Expression> {
        eprintln!("Cur token type {:?}", self.cur_token.ttype);
        match self.cur_token.ttype {
            TokenType::IDENT => self.parse_identifier(),
            TokenType::INT => self.parse_integer_literal(),
            TokenType::BANG | TokenType::MINUS => self.parse_prefix_expression(),
            _ => {
                self.no_prefix_parse_fn_error(self.cur_token.ttype);
                self.next_token();
                return None;
            }
        }
    }
    fn infix_parse_fn(&mut self, left: Expression) -> Option<Expression> {
        self.next_token();
        match self.cur_token.ttype {
            TokenType::EQ
            | TokenType::NOT_EQ
            | TokenType::LT
            | TokenType::GT
            | TokenType::PLUS
            | TokenType::MINUS
            | TokenType::SLASH
            | TokenType::ASTERISK => self.parse_infix_expression(left),
            _ => {
                self.no_infix_parse_fn_error(self.cur_token.ttype);
                self.next_token();
                return None;
            }
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let prefix = self.prefix_parse_fn();
        let mut left_expression = Expression::Default;
        eprintln!("We got precedence {:?}", precedence);
        eprintln!("parse_expression: prefix: {:?}", prefix);
        match prefix {
            Some(e) => left_expression = e,
            None => {
                self.no_prefix_parse_fn_error(self.cur_token.ttype);
                return None;
            }
        }

        while !self.peek_token_is(TokenType::SEMICOLON) && precedence < self.peek_precedence() {
            let infix = self.infix_parse_fn(left_expression.clone());
            eprintln!("from infix_parse_fn: {:?}", infix);
            match infix {
                Some(e) => left_expression = e,
                None => {
                    return Some(left_expression);
                }
            }
        }

        return Some(left_expression);
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        eprintln!("I SHOULD HAVE BEEN CALLED");
        let prev = self.cur_token.clone();
        self.next_token();

        let expression = PrefixExpression {
            token: prev.clone(),
            operator: prev.literal,
            right: Box::new(self.parse_expression(Precedence::Prefix).unwrap()),
        };

        return Some(Expression::Prefix { p: expression });
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        eprintln!("I shoyld have been called");
        let mut expression = InfixExpression {
            token: self.cur_token.clone(),
            operator: self.cur_token.literal.clone(),
            left: Box::new(left),
            right: Box::new(Expression::Default),
        };

        let precedence = self.cur_precedence();
        self.next_token();
        // this may be unsafe??
        let e = self.parse_expression(precedence);
        eprintln!("This is e: {:?}", e);
        expression.right = Box::new(e.unwrap());
        return Some(Expression::Infix { i: expression });
    }

    fn parse_identifier(&self) -> Option<Expression> {
        Some(Expression::Identifier {
            id: Identifier {
                token: self.cur_token.clone(),
                value: self.cur_token.literal.clone(),
            },
        })
    }

    fn parse_integer_literal(&mut self) -> Option<Expression> {
        let mut lit = IntegerLiteral::new();
        lit.token = self.cur_token.clone();

        let l = lit.token.literal.clone().parse::<i64>();
        match l {
            Ok(i) => lit.value = i,
            Err(_) => {
                let msg = format!("could not parse {:?} as an integer", lit.token);
                self.errors.push(msg);
                return None;
            }
        }

        Some(Expression::Integer { i: lit })
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

    fn peek_precedence(&self) -> Precedence {
        precedences_for_token(self.peek_token.ttype)
    }

    fn cur_precedence(&self) -> Precedence {
        precedences_for_token(self.cur_token.ttype)
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

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();

        assert!(p.errors().is_empty(), "Parser had errors: {:?}", p.errors());

        assert_eq!(
            program.statements.len(),
            1,
            "program should have 1 statement, got={}",
            program.statements.len()
        );

        let stmt = match &program.statements[0] {
            Statement::Expression { e } => e,
            s => panic!("expected ExpressionStatement, got={:?}", s),
        };

        let _literal = match &stmt.expression {
            Some(e) => match e {
                Expression::Integer { i } => {
                    assert_eq!(i.value, 5, "expected integer value 5, got={}", i.value);
                    assert_eq!(
                        i.token.literal, "5",
                        "expected token literal '5', got={}",
                        i.token.literal
                    );
                }
                e => panic!("expected IntegerLiteral expression, got={:?}", e),
            },
            None => panic!("Expected expression, got none"),
        };
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        struct PrefixTest<'a> {
            input: &'a str,
            operator: &'a str,
            integer_value: i64,
        }

        let prefix_tests = vec![
            PrefixTest {
                input: "!5;",
                operator: "!",
                integer_value: 5,
            },
            PrefixTest {
                input: "-15;",
                operator: "-",
                integer_value: 15,
            },
        ];

        for tt in prefix_tests {
            let l = Lexer::new(tt.input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);

            assert_eq!(
                program.statements.len(),
                1,
                "program.statements does not contain 1 statement. got={}",
                program.statements.len()
            );

            let stmt = match &program.statements[0] {
                Statement::Expression { e } => e,
                other => panic!(
                    "program.Statements[0] is not ExpressionStatement. got={:?}",
                    other
                ),
            };

            let expr = match &stmt.expression.as_ref().unwrap() {
                Expression::Prefix { p } => p,
                other => panic!("stmt.expression is not PrefixExpression. got={:?}", other),
            };

            assert_eq!(
                expr.operator, tt.operator,
                "exp.Operator is not '{}'. got='{}'",
                tt.operator, expr.operator
            );
            assert!(test_integer_literal(&*expr.right, tt.integer_value));
        }
    }
    fn test_integer_literal(expr: &Expression, expected_value: i64) -> bool {
        match expr {
            Expression::Integer { i } => {
                if i.value != expected_value {
                    eprintln!("value not {}. got={}", expected_value, i.value);
                    return false;
                }

                if i.token.literal != expected_value.to_string() {
                    eprintln!(
                        "token.literal not {}. got={}",
                        expected_value, i.token.literal
                    );
                    return false;
                }

                true
            }
            other => {
                eprintln!("expr is not IntegerLiteral. got={:?}", other);
                false
            }
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
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
            let l = Lexer::new(input.to_string().as_str());
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);

            assert_eq!(
                program.statements.len(),
                1,
                "program.Statements does not contain 1 statement. got={}",
                program.statements.len()
            );

            let stmt = match &program.statements[0] {
                Statement::Expression { e } => e,
                other => panic!(
                    "program.Statements[0] is not ExpressionStatement. got={:?}",
                    other
                ),
            };

            let exp = match &stmt.expression {
                Some(Expression::Infix { i }) => (&i.left, i.operator.clone(), &i.right),
                other => panic!("stmt.expression is not InfixExpression. got={:?}", other),
            };

            assert!(
                test_integer_literal(&exp.0, left_value),
                "left value did not match"
            );
            assert_eq!(
                exp.1, operator,
                "operator mismatch: expected {}, got {}",
                operator, exp.1
            );
            assert!(
                test_integer_literal(&exp.2, right_value),
                "right value did not match"
            );
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
            let l = Lexer::new(input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);

            let actual = program.string();
            assert_eq!(
                actual, expected,
                "Expected expression formatting failed.\nInput: {}\nExpected: {}\nGot: {}",
                input, expected, actual
            );
        }
    }
}
