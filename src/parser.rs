use crate::TokenType;
use crate::ast;
use crate::ast::BlockStatement;
use crate::ast::BooleanExpression;
use crate::ast::CallExpression;
use crate::ast::ExpressionStatement;
use crate::ast::FunctionLiteral;
use crate::ast::HashLiteral;
use crate::ast::IdentifierExpression;
use crate::ast::IndexExpression;
use crate::ast::LetStatement;
use crate::ast::ReturnStatement;
use crate::ast::StringLiteral;
use crate::lexer;
use crate::token;

type PrefixParseFn = fn(&mut Parser) -> Option<ast::Expression>;
type InfixParseFn = fn(&mut Parser, ast::Expression) -> Option<ast::Expression>;


// used for tests in multiple files
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum ExpectedLiteral {
    Int(i64),
    Str(String),
    Boolean(bool),
    IntArray(Vec<i64>),
    Null(),
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub enum Precedence {
    LOWEST,
    EQUALS,      // ==
    LESSGREATER, // > or
    SUM,         // +
    PRODUCT,     // *
    PREFIX,      // -X or !X
    CALL,        // myFunction(X)
    INDEX,
}

fn precedences(t: &TokenType) -> Option<Precedence> {
    match t {
        TokenType::EQ | TokenType::NOT_EQ => Some(Precedence::EQUALS),
        TokenType::LT | TokenType::GT => Some(Precedence::LESSGREATER),
        TokenType::PLUS | TokenType::MINUS => Some(Precedence::SUM),
        TokenType::SLASH | TokenType::ASTERISK => Some(Precedence::PRODUCT),
        TokenType::LPAREN => Some(Precedence::CALL),
        TokenType::LBRACKET => Some(Precedence::INDEX),
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

        p
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    pub fn parse_program(&mut self) -> ast::Program {
        let mut program = ast::Program {
            statements: Vec::new(),
        };

        while self.cur_token.token_type != token::TokenType::EOF {
            let stmt = self.parse_statement();
            if let Some(s) = stmt {
                program.statements.push(s);
            }
            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Option<ast::Statement> {
        match self.cur_token.token_type {
            token::TokenType::LET => self.parse_let_statement().map(ast::Statement::Let),
            token::TokenType::RETURN => self.parse_return_statement().map(ast::Statement::Return),
            _ => self
                .parse_expression_statement()
                .map(ast::Statement::Expression),
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

        if !self.expect_peek(&TokenType::ASSIGN) {
            return None;
        }
        self.next_token();
        stmt.value = self.parse_expression(Precedence::LOWEST);

        while !self.cur_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(stmt)
    }

    fn parse_return_statement(&mut self) -> Option<ReturnStatement> {
        let mut stmt = ast::ReturnStatement {
            token: self.cur_token.clone(),
            return_value: None,
        };

        self.next_token();

        stmt.return_value = self.parse_expression(Precedence::LOWEST);

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

        left_exp
    }

    fn parse_identifier(&mut self) -> Option<ast::Expression> {
        Some(ast::Expression::Identifier(IdentifierExpression {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        }))
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
        let exp = self.parse_expression(precendence);
        match exp {
            Some(e) => expression.right = Box::new(e),
            None => return None,
        };

        Some(ast::Expression::Infix(expression))
    }

    fn parse_call_expression(&mut self, function: ast::Expression) -> Option<ast::Expression> {
        let mut exp = CallExpression {
            token: self.cur_token.clone(),
            function: Box::new(function),
            ..Default::default()
        };

        if let Some(a) = self.parse_expression_list(TokenType::RPAREN) {
            exp.arguments = a
        }
        Some(ast::Expression::Call(exp))
    }

    fn parse_index_expression(&mut self, left: ast::Expression) -> Option<ast::Expression> {
        let mut exp = IndexExpression {
            token: self.cur_token.clone(),
            left: Box::new(left),
            ..Default::default()
        };

        self.next_token();
        let index = self.parse_expression(Precedence::LOWEST);
        if !self.expect_peek(&TokenType::RBRACKET) {
            return None;
        }
        match index {
            Some(i) => {
                exp.index = Box::new(i);
                return Some(ast::Expression::Index(exp));
            }
            _ => None,
        }
    }

    fn parse_boolean(&mut self) -> Option<ast::Expression> {
        let be = BooleanExpression {
            token: self.cur_token.clone(),
            value: self.cur_token_is(TokenType::TRUE),
        };
        Some(ast::Expression::Boolean(be))
    }

    fn parse_grouped_expression(&mut self) -> Option<ast::Expression> {
        self.next_token();
        match self.parse_expression(Precedence::LOWEST) {
            Some(e) => {
                if !self.expect_peek(&TokenType::RPAREN) {
                    return None;
                }
                Some(e)
            }
            None => None,
        }
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let mut block = BlockStatement {
            token: self.cur_token.clone(),
            ..Default::default()
        };

        self.next_token();
        while !self.cur_token_is(TokenType::RBRACE) && !self.cur_token_is(TokenType::EOF) {
            if let Some(s) = self.parse_statement() {
                block.statements.push(s)
            }
            self.next_token();
        }

        block
    }

    fn parse_if_expression(&mut self) -> Option<ast::Expression> {
        let mut expression = ast::IfExpression {
            token: self.cur_token.clone(),
            ..Default::default()
        };

        if !self.expect_peek(&TokenType::LPAREN) {
            return None;
        }

        self.next_token();
        expression.condition = Box::new(
            self.parse_expression(Precedence::LOWEST)
                .expect("BETTER BE"),
        );

        if !self.expect_peek(&TokenType::RPAREN) {
            return None;
        }
        if !self.expect_peek(&TokenType::LBRACE) {
            return None;
        }
        expression.consequence = Box::new(self.parse_block_statement());

        if self.peek_token_is(&TokenType::ELSE) {
            self.next_token();

            if !self.expect_peek(&TokenType::LBRACE) {
                return None;
            }

            expression.alternative = Some(Box::new(self.parse_block_statement()));
        }

        Some(ast::Expression::If(expression))
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<ast::IdentifierExpression>> {
        let mut idents: Vec<IdentifierExpression> = Vec::new();

        if self.peek_token_is(&TokenType::RPAREN) {
            self.next_token();
            return Some(idents);
        }

        self.next_token();

        let ident = IdentifierExpression {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        };

        idents.push(ident);

        while self.peek_token_is(&TokenType::COMMA) {
            self.next_token();
            self.next_token();
            let ident = IdentifierExpression {
                token: self.cur_token.clone(),
                value: self.cur_token.literal.clone(),
            };
            idents.push(ident);
        }

        if !self.expect_peek(&TokenType::RPAREN) {
            return None;
        }
        Some(idents)
    }

    fn parse_function_literal(&mut self) -> Option<ast::Expression> {
        let mut lit = FunctionLiteral {
            token: self.cur_token.clone(),
            ..Default::default()
        };

        if !self.expect_peek(&TokenType::LPAREN) {
            return None;
        }

        lit.parameters = self.parse_function_parameters().expect("PARAM ERROR");

        if !self.expect_peek(&TokenType::LBRACE) {
            return None;
        }

        lit.body = self.parse_block_statement();

        Some(ast::Expression::Function(lit))
    }

    fn parse_string_literal(&mut self) -> Option<ast::Expression> {
        Some(ast::Expression::String(StringLiteral {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        }))
    }

    fn parse_array_literal(&mut self) -> Option<ast::Expression> {
        let arr = ast::ArrayLiteral {
            token: self.cur_token.clone(),
            elements: self
                .parse_expression_list(TokenType::RBRACKET)
                .expect("SUUURE BUD"),
        };

        Some(ast::Expression::Array(arr))
    }

    fn parse_hash_literal(&mut self) -> Option<ast::Expression> {
        let mut hash = HashLiteral {
            token: self.cur_token.clone(),
            pairs: Vec::new(),
        };

        while !self.peek_token_is(&TokenType::RBRACE) {
            self.next_token();
            let key = self.parse_expression(Precedence::LOWEST);

            if !self.expect_peek(&TokenType::COLON) {
                return None;
            }

            self.next_token();
            let value = self.parse_expression(Precedence::LOWEST);
            hash.pairs.push((key.expect("hehe"), value.expect("hoho")));

            if !self.peek_token_is(&TokenType::RBRACE) && !self.expect_peek(&TokenType::COMMA) {
                return None;
            }
        }

        if !self.expect_peek(&TokenType::RBRACE) {
            return None;
        }

        Some(ast::Expression::Hash(hash))
    }

    fn parse_expression_list(&mut self, end: TokenType) -> Option<Vec<ast::Expression>> {
        let mut list: Vec<ast::Expression> = Vec::new();

        if self.peek_token_is(&end) {
            self.next_token();
            return Some(list);
        }

        self.next_token();
        if let Some(e) = self.parse_expression(Precedence::LOWEST) {
            list.push(e)
        };

        while self.peek_token_is(&TokenType::COMMA) {
            self.next_token();
            self.next_token();

            if let Some(e) = self.parse_expression(Precedence::LOWEST) {
                list.push(e)
            };
        }

        if !self.expect_peek(&end) {
            return None;
        }
        Some(list)
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

    fn prefix_parse_fns(&self, t: &TokenType) -> Option<PrefixParseFn> {
        match t {
            TokenType::IDENT => Some(Parser::parse_identifier),
            TokenType::INT => Some(Parser::parse_integer_literal),
            TokenType::BANG => Some(Parser::parse_prefix_expression),
            TokenType::MINUS => Some(Parser::parse_prefix_expression),
            TokenType::TRUE | TokenType::FALSE => Some(Parser::parse_boolean),
            TokenType::LPAREN => Some(Parser::parse_grouped_expression),
            TokenType::IF => Some(Parser::parse_if_expression),
            TokenType::FUNCTION => Some(Parser::parse_function_literal),
            TokenType::STRING => Some(Parser::parse_string_literal),
            TokenType::LBRACKET => Some(Parser::parse_array_literal),
            TokenType::LBRACE => Some(Parser::parse_hash_literal),
            _ => None,
        }
    }

    fn no_prefix_parse_fn_error(&mut self, t: TokenType) {
        let msg = format!("no prefix parse function for {:?} found", t);
        self.errors.push(msg);
    }

    fn infix_parse_fns(&self, t: &TokenType) -> Option<InfixParseFn> {
        match t {
            TokenType::PLUS
            | TokenType::MINUS
            | TokenType::SLASH
            | TokenType::ASTERISK
            | TokenType::EQ
            | TokenType::NOT_EQ
            | TokenType::LT
            | TokenType::GT => Some(Parser::parse_infix_expression),
            TokenType::LPAREN => Some(Parser::parse_call_expression),
            TokenType::LBRACKET => Some(Parser::parse_index_expression),
            _ => None,
        }
    }
}

#[allow(dead_code)]
pub fn check_parser_errors(p: &Parser) {
    if p.errors.len() == 0 {
        return;
    }

    eprintln!("Parser has {} errors", p.errors.len());
    for e in &p.errors {
        eprintln!("parser error: {}", e);
    }
}

#[cfg(test)]
mod tests {
    use self::ast::Node;

    use super::*;
    use crate::ast::Expression;
    use crate::ast::Statement;
    use crate::lexer::Lexer;

    fn test_let_statement(s: &Statement, name: &str) -> bool {
        if s.token_literal() != "let" {
            eprintln!("token_literal not 'let', got={}", s.token_literal());
            return false;
        }

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
        let tests = vec![
            ("let x = 5;", "x", ExpectedLiteral::Int(5)),
            ("let y = true;", "y", ExpectedLiteral::Boolean(true)),
            (
                "let foobar = y;",
                "foobar",
                ExpectedLiteral::Str("y".to_string()),
            ),
        ];

        for (input, expected_ident, expected_value) in tests {
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

            let stmt = &program.statements[0];

            if !test_let_statement(stmt, expected_ident) {
                return;
            }

            let val = match stmt {
                Statement::Let(ls) => match &ls.value {
                    Some(v) => v,
                    None => {
                        panic!("let statement value is None");
                    }
                },
                _ => panic!("stmt is not a LetStatement"),
            };

            if !test_literal_expression(val, expected_value) {
                return;
            }
        }
    }
    #[test]
    fn test_return_statements() {
        let tests = vec![
            ("return 5;", ExpectedLiteral::Int(5)),
            ("return true;", ExpectedLiteral::Boolean(true)),
            ("return foobar;", ExpectedLiteral::Str("foobar".to_string())),
        ];

        for (input, expected_value) in tests {
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

            let stmt = &program.statements[0];

            let return_stmt = match stmt {
                Statement::Return(rs) => rs,
                _ => panic!("stmt is not a ReturnStatement, got={:?}", stmt),
            };

            assert_eq!(
                return_stmt.token.literal, "return",
                "return_stmt.token.literal not 'return', got={}",
                return_stmt.token.literal
            );

            let val = match &return_stmt.return_value {
                Some(v) => v,
                None => panic!("return statement value is None"),
            };

            if !test_literal_expression(val, expected_value) {
                return;
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
    fn test_boolean_expression() {
        let boolean_tests = vec![("true;", true), ("false;", false)];

        for (input, expected_value) in boolean_tests {
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

            let boolean = match &stmt.expression {
                Some(ast::Expression::Boolean(b)) => b,
                _ => panic!(
                    "stmt.expression is not a Boolean, got={:?}",
                    stmt.expression
                ),
            };

            assert_eq!(
                boolean.value, expected_value,
                "boolean.value not {}, got={}",
                expected_value, boolean.value
            );
        }
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

    fn test_identifier(exp: &ast::Expression, value: String) -> bool {
        let ident = match exp {
            ast::Expression::Identifier(is) => is,
            _ => {
                eprintln!("exp not ast::Expression::Idenfitier. got {:?}", exp);
                return false;
            }
        };

        if ident.value == value {
            eprintln!("ident.value not {}. got={}", value, ident.value);
            return false;
        }

        if ident.token_literal() != value {
            eprintln!(
                "ident.token_literal not {}. got={}",
                value,
                ident.token_literal()
            );
            return false;
        }

        return true;
    }

    fn test_boolean_literal(expr: &ast::Expression, value: bool) -> bool {
        let bo = match expr {
            ast::Expression::Boolean(b) => b,
            _ => {
                eprintln!("expr is not a Boolean, got={:?}", expr);
                return false;
            }
        };

        if bo.value != value {
            eprintln!("bo.value not {}, got={}", value, bo.value);
            return false;
        }

        if bo.token.literal != value.to_string() {
            eprintln!("bo.token.literal not {}, got={}", value, bo.token.literal);
            return false;
        }

        true
    }

    fn test_literal_expression(exp: &ast::Expression, expected: ExpectedLiteral) -> bool {
        match expected {
            ExpectedLiteral::Int(v) => test_integer_literal(&exp, v),
            ExpectedLiteral::Str(v) => test_identifier(exp, v),
            ExpectedLiteral::Boolean(v) => test_boolean_literal(&exp, v),
            _ => {
                eprintln!("type of exp not handled. got={:?}", exp);
                return false;
            }
        }
    }

    fn test_infix_expression(
        exp: &ast::Expression,
        left: ExpectedLiteral,
        operator: String,
        right: ExpectedLiteral,
    ) -> bool {
        let op_exp = match exp {
            ast::Expression::Infix(ie) => ie,
            _ => {
                eprintln!("exp is not ast::Expression::Infix. got={:?}", exp);
                return false;
            }
        };

        if !test_literal_expression(&*op_exp.left, left) {
            return false;
        }

        if op_exp.operator != operator {
            eprintln!("exp.operator is not {}, got={}", operator, op_exp.operator);
            return false;
        }

        if !test_literal_expression(&*op_exp.right, right) {
            return false;
        }

        return true;
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let prefix_tests: Vec<(&str, &str, ExpectedLiteral)> = vec![
            ("!5;", "!", ExpectedLiteral::Int(5i64)),
            ("-15;", "-", ExpectedLiteral::Int(15i64)),
            ("-15;", "-", ExpectedLiteral::Int(15i64)),
            ("!true;", "!", ExpectedLiteral::Boolean(true)),
            ("!false;", "!", ExpectedLiteral::Boolean(false)),
        ];

        for (input, operator, expected_value) in prefix_tests {
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

            let _ = match expected_value {
                ExpectedLiteral::Int(ei) => test_integer_literal(&exp.right, ei),
                ExpectedLiteral::Boolean(eb) => test_boolean_literal(&exp.right, eb),
                _ => false,
            };

            // test_integer_literal(&exp.right, expected_value);
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let infix_tests: Vec<(&str, ExpectedLiteral, &str, ExpectedLiteral)> = vec![
            (
                "5 + 5;",
                ExpectedLiteral::Int(5),
                "+",
                ExpectedLiteral::Int(5),
            ),
            (
                "5 - 5;",
                ExpectedLiteral::Int(5),
                "-",
                ExpectedLiteral::Int(5),
            ),
            (
                "5 * 5;",
                ExpectedLiteral::Int(5),
                "*",
                ExpectedLiteral::Int(5),
            ),
            (
                "5 / 5;",
                ExpectedLiteral::Int(5),
                "/",
                ExpectedLiteral::Int(5),
            ),
            (
                "5 > 5;",
                ExpectedLiteral::Int(5),
                ">",
                ExpectedLiteral::Int(5),
            ),
            (
                "5 < 5;",
                ExpectedLiteral::Int(5),
                "<",
                ExpectedLiteral::Int(5),
            ),
            (
                "5 == 5;",
                ExpectedLiteral::Int(5),
                "==",
                ExpectedLiteral::Int(5),
            ),
            (
                "5 != 5;",
                ExpectedLiteral::Int(5),
                "!=",
                ExpectedLiteral::Int(5),
            ),
            (
                "true == true;",
                ExpectedLiteral::Boolean(true),
                "==",
                ExpectedLiteral::Boolean(true),
            ),
            (
                "true != false;",
                ExpectedLiteral::Boolean(true),
                "!=",
                ExpectedLiteral::Boolean(false),
            ),
            (
                "false == false;",
                ExpectedLiteral::Boolean(false),
                "==",
                ExpectedLiteral::Boolean(false),
            ),
        ];

        for (input, left_value, operator, right_value) in infix_tests {
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

            if !test_infix_expression(
                &stmt.expression.as_ref().unwrap(),
                left_value,
                String::from(operator),
                right_value,
            ) {
                panic!("test_infix_expression failed for input: {}", input);
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
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
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

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";
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
            Some(ast::Expression::If(ie)) => ie,
            _ => panic!(
                "stmt.expression is not an IfExpression, got={:?}",
                stmt.expression
            ),
        };

        if !test_infix_expression(
            &exp.condition,
            ExpectedLiteral::Str("x".to_string()),
            "<".to_string(),
            ExpectedLiteral::Str("y".to_string()),
        ) {
            return;
        }

        assert_eq!(
            exp.consequence.statements.len(),
            1,
            "consequence does not contain 1 statement, got={}",
            exp.consequence.statements.len()
        );

        let consequence = match &exp.consequence.statements[0] {
            Statement::Expression(es) => es,
            _ => panic!(
                "consequence.statements[0] is not an ExpressionStatement, got={:?}",
                exp.consequence.statements[0]
            ),
        };

        if !test_identifier(consequence.expression.as_ref().unwrap(), "x".to_string()) {
            return;
        }

        assert!(
            exp.alternative.is_none(),
            "exp.alternative was not None, got={:?}",
            exp.alternative
        );
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";
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
            Some(ast::Expression::If(ie)) => ie,
            _ => panic!(
                "stmt.expression is not an IfExpression, got={:?}",
                stmt.expression
            ),
        };

        if !test_infix_expression(
            &exp.condition,
            ExpectedLiteral::Str("x".to_string()),
            "<".to_string(),
            ExpectedLiteral::Str("y".to_string()),
        ) {
            return;
        }

        assert_eq!(
            exp.consequence.statements.len(),
            1,
            "consequence does not contain 1 statement, got={}",
            exp.consequence.statements.len()
        );

        let consequence = match &exp.consequence.statements[0] {
            Statement::Expression(es) => es,
            _ => panic!(
                "consequence.statements[0] is not an ExpressionStatement, got={:?}",
                exp.consequence.statements[0]
            ),
        };

        if !test_identifier(consequence.expression.as_ref().unwrap(), "x".to_string()) {
            return;
        }

        let alt = match &exp.alternative {
            Some(a) => a,
            None => panic!("exp.alternative is None"),
        };

        assert_eq!(
            alt.statements.len(),
            1,
            "alternative does not contain 1 statement, got={}",
            alt.statements.len()
        );

        let alternative = match &alt.statements[0] {
            Statement::Expression(es) => es,
            _ => panic!(
                "alt.statements[0] is not an ExpressionStatement, got={:?}",
                alt.statements[0]
            ),
        };

        if !test_identifier(alternative.expression.as_ref().unwrap(), "y".to_string()) {
            return;
        }
    }
    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) { x + y; }";
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

        let function = match &stmt.expression {
            Some(ast::Expression::Function(fl)) => fl,
            _ => panic!(
                "stmt.expression is not a FunctionLiteral, got={:?}",
                stmt.expression
            ),
        };

        assert_eq!(
            function.parameters.len(),
            2,
            "function literal parameters wrong, want 2, got={}",
            function.parameters.len()
        );

        test_literal_expression(
            &ast::Expression::Identifier(function.parameters[0].clone()),
            ExpectedLiteral::Str("x".to_string()),
        );
        test_literal_expression(
            &ast::Expression::Identifier(function.parameters[1].clone()),
            ExpectedLiteral::Str("y".to_string()),
        );

        assert_eq!(
            function.body.statements.len(),
            1,
            "function.body.statements does not have 1 statement, got={}",
            function.body.statements.len()
        );

        let body_stmt = match &function.body.statements[0] {
            Statement::Expression(es) => es,
            _ => panic!(
                "function body stmt is not an ExpressionStatement, got={:?}",
                function.body.statements[0]
            ),
        };

        test_infix_expression(
            body_stmt.expression.as_ref().unwrap(),
            ExpectedLiteral::Str("x".to_string()),
            "+".to_string(),
            ExpectedLiteral::Str("y".to_string()),
        );
    }

    #[test]
    fn test_function_parameter_parsing() {
        let tests = vec![
            ("fn() {};", vec![]),
            ("fn(x) {};", vec!["x"]),
            ("fn(x, y, z) {};", vec!["x", "y", "z"]),
        ];

        for (input, expected_params) in tests {
            let l = Lexer::new(input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);

            let stmt = match &program.statements[0] {
                Statement::Expression(es) => es,
                _ => panic!(
                    "program.statements[0] is not an ExpressionStatement, got={:?}",
                    program.statements[0]
                ),
            };

            let function = match &stmt.expression {
                Some(ast::Expression::Function(fl)) => fl,
                _ => panic!(
                    "stmt.expression is not a FunctionLiteral, got={:?}",
                    stmt.expression
                ),
            };

            assert_eq!(
                function.parameters.len(),
                expected_params.len(),
                "length parameters wrong, want {}, got={}",
                expected_params.len(),
                function.parameters.len()
            );

            for (i, ident) in expected_params.iter().enumerate() {
                test_literal_expression(
                    &ast::Expression::Identifier(function.parameters[i].clone()),
                    ExpectedLiteral::Str(ident.to_string()),
                );
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";
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
            Some(ast::Expression::Call(ce)) => ce,
            _ => panic!(
                "stmt.expression is not a CallExpression, got={:?}",
                stmt.expression
            ),
        };

        if !test_identifier(&exp.function, "add".to_string()) {
            return;
        }

        assert_eq!(
            exp.arguments.len(),
            3,
            "wrong length of arguments, got={}",
            exp.arguments.len()
        );

        test_literal_expression(&exp.arguments[0], ExpectedLiteral::Int(1));
        test_infix_expression(
            &exp.arguments[1],
            ExpectedLiteral::Int(2),
            "*".to_string(),
            ExpectedLiteral::Int(3),
        );
        test_infix_expression(
            &exp.arguments[2],
            ExpectedLiteral::Int(4),
            "+".to_string(),
            ExpectedLiteral::Int(5),
        );
    }

    #[test]
    fn test_string_literal_expression() {
        let input = "\"hello world\";";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        let stmt = match &program.statements[0] {
            Statement::Expression(es) => es,
            _ => panic!(
                "program.statements[0] is not an ExpressionStatement, got={:?}",
                program.statements[0]
            ),
        };

        let literal = match &stmt.expression {
            Some(Expression::String(sl)) => sl,
            _ => panic!(
                "stmt.expression is not a StringLiteral, got={:?}",
                stmt.expression
            ),
        };

        assert_eq!(
            literal.value, "hello world",
            "literal.value not 'hello world', got={}",
            literal.value
        );
    }

    #[test]
    fn test_parsing_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        let stmt = match &program.statements[0] {
            Statement::Expression(es) => es,
            _ => panic!(
                "program.statements[0] is not an ExpressionStatement, got={:?}",
                program.statements[0]
            ),
        };

        let array = match &stmt.expression {
            Some(Expression::Array(a)) => a,
            _ => panic!(
                "stmt.expression is not an ArrayLiteral, got={:?}",
                stmt.expression
            ),
        };

        assert_eq!(
            array.elements.len(),
            3,
            "len(array.elements) not 3, got={}",
            array.elements.len()
        );

        test_integer_literal(&array.elements[0], 1);
        test_infix_expression(
            &array.elements[1],
            ExpectedLiteral::Int(2),
            "*".to_string(),
            ExpectedLiteral::Int(2),
        );
        test_infix_expression(
            &array.elements[2],
            ExpectedLiteral::Int(3),
            "+".to_string(),
            ExpectedLiteral::Int(3),
        );
    }

    #[test]
    fn test_parsing_index_expressions() {
        let input = "myArray[1 + 1]";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        let stmt = match &program.statements[0] {
            Statement::Expression(es) => es,
            _ => panic!(
                "program.statements[0] is not an ExpressionStatement, got={:?}",
                program.statements[0]
            ),
        };

        let index_exp = match &stmt.expression {
            Some(Expression::Index(ie)) => ie,
            _ => panic!(
                "stmt.expression is not an IndexExpression, got={:?}",
                stmt.expression
            ),
        };

        if !test_identifier(&index_exp.left, "myArray".to_string()) {
            return;
        }

        if !test_infix_expression(
            &index_exp.index,
            ExpectedLiteral::Int(1),
            "+".to_string(),
            ExpectedLiteral::Int(1),
        ) {
            return;
        }
    }

    #[test]
    fn test_parsing_hash_literals_string_keys() {
        let input = r#"{"one": 1, "two": 2, "three": 3}"#;
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        let stmt = match &program.statements[0] {
            Statement::Expression(es) => es,
            _ => panic!(
                "program.statements[0] is not an ExpressionStatement, got={:?}",
                program.statements[0]
            ),
        };

        let hash = match &stmt.expression {
            Some(Expression::Hash(h)) => h,
            _ => panic!(
                "stmt.expression is not a HashLiteral, got={:?}",
                stmt.expression
            ),
        };

        assert_eq!(
            hash.pairs.len(),
            3,
            "hash.pairs has wrong length, got={}",
            hash.pairs.len()
        );

        let expected = vec![("one", 1i64), ("two", 2i64), ("three", 3i64)];

        for (key, value) in &hash.pairs {
            let literal = match key {
                Expression::String(s) => s,
                _ => panic!("key is not a StringLiteral, got={:?}", key),
            };

            let expected_value = expected
                .iter()
                .find(|(k, _)| *k == literal.value)
                .map(|(_, v)| *v)
                .unwrap_or_else(|| panic!("no expected value for key={}", literal.value));

            test_integer_literal(value, expected_value);
        }
    }

    #[test]
    fn test_parsing_empty_hash_literal() {
        let input = "{}";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        let stmt = match &program.statements[0] {
            Statement::Expression(es) => es,
            _ => panic!(
                "program.statements[0] is not an ExpressionStatement, got={:?}",
                program.statements[0]
            ),
        };

        let hash = match &stmt.expression {
            Some(Expression::Hash(h)) => h,
            _ => panic!(
                "stmt.expression is not a HashLiteral, got={:?}",
                stmt.expression
            ),
        };

        assert_eq!(
            hash.pairs.len(),
            0,
            "hash.pairs has wrong length, got={}",
            hash.pairs.len()
        );
    }

    #[test]
    fn test_parsing_hash_literals_integer_keys() {
        let input = "{1: 1, 2: 2, 3: 3}";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        let stmt = match &program.statements[0] {
            Statement::Expression(es) => es,
            _ => panic!(
                "program.statements[0] is not an ExpressionStatement, got={:?}",
                program.statements[0]
            ),
        };

        let hash = match &stmt.expression {
            Some(Expression::Hash(h)) => h,
            _ => panic!(
                "stmt.expression is not a HashLiteral, got={:?}",
                stmt.expression
            ),
        };

        assert_eq!(
            hash.pairs.len(),
            3,
            "hash.pairs has wrong length, got={}",
            hash.pairs.len()
        );

        let expected = vec![(1i64, 1i64), (2, 2), (3, 3)];

        for ((key, value), (expected_key, expected_value)) in hash.pairs.iter().zip(expected.iter())
        {
            let literal = match key {
                Expression::Integer(i) => i,
                _ => panic!("key is not an IntegerLiteral, got={:?}", key),
            };
            assert_eq!(literal.value, *expected_key);
            test_integer_literal(value, *expected_value);
        }
    }

    #[test]
    fn test_parsing_hash_literals_boolean_keys() {
        let input = "{true: 1, false: 2}";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        let stmt = match &program.statements[0] {
            Statement::Expression(es) => es,
            _ => panic!(
                "program.statements[0] is not an ExpressionStatement, got={:?}",
                program.statements[0]
            ),
        };

        let hash = match &stmt.expression {
            Some(Expression::Hash(h)) => h,
            _ => panic!(
                "stmt.expression is not a HashLiteral, got={:?}",
                stmt.expression
            ),
        };

        assert_eq!(
            hash.pairs.len(),
            2,
            "hash.pairs has wrong length, got={}",
            hash.pairs.len()
        );

        let expected = vec![(true, 1i64), (false, 2)];

        for ((key, value), (expected_key, expected_value)) in hash.pairs.iter().zip(expected.iter())
        {
            let literal = match key {
                Expression::Boolean(b) => b,
                _ => panic!("key is not a Boolean, got={:?}", key),
            };
            assert_eq!(literal.value, *expected_key);
            test_integer_literal(value, *expected_value);
        }
    }

    #[test]
    fn test_parsing_hash_literals_with_expressions() {
        let input = r#"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"#;
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        let stmt = match &program.statements[0] {
            Statement::Expression(es) => es,
            _ => panic!(
                "program.statements[0] is not an ExpressionStatement, got={:?}",
                program.statements[0]
            ),
        };

        let hash = match &stmt.expression {
            Some(Expression::Hash(h)) => h,
            _ => panic!(
                "stmt.expression is not a HashLiteral, got={:?}",
                stmt.expression
            ),
        };

        assert_eq!(
            hash.pairs.len(),
            3,
            "hash.pairs has wrong length, got={}",
            hash.pairs.len()
        );

        // map of key -> (left, operator, right)
        let expected: Vec<(&str, ExpectedLiteral, &str, ExpectedLiteral)> = vec![
            ("one", ExpectedLiteral::Int(0), "+", ExpectedLiteral::Int(1)),
            (
                "two",
                ExpectedLiteral::Int(10),
                "-",
                ExpectedLiteral::Int(8),
            ),
            (
                "three",
                ExpectedLiteral::Int(15),
                "/",
                ExpectedLiteral::Int(5),
            ),
        ];

        for (key, value) in &hash.pairs {
            let literal = match key {
                Expression::String(s) => s,
                _ => panic!("key is not a StringLiteral, got={:?}", key),
            };

            let test = expected
                .iter()
                .find(|(k, _, _, _)| *k == literal.value)
                .unwrap_or_else(|| panic!("no test found for key={}", literal.value));

            test_infix_expression(value, test.1.clone(), test.2.to_string(), test.3.clone());
        }
    }
}
