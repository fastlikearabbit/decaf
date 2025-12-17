/// Decaf Parser

use crate::parser::ast::*;
use crate::parser::token::Token;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub message: String,
    pub position: usize,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Parse error at token {}: {}", self.position, self.message)
    }
}

enum DeclType {
    Field(FieldDecl),
    Method(MethodDecl),
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

/// Operator precedence for Pratt parsing
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    None = 0,
    Or = 1,          // ||
    And = 2,         // &&
    Equality = 3,    // == !=
    Comparison = 4,  // < > <= >=
    Term = 5,        // + -
    Factor = 6,      // * / %
    Unary = 7,       // - ! int() long()
    Call = 8,        // method calls, array access
    Primary = 9,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut program = Program::new();

        while self.check(&Token::Import) {
            program.imports.push(self.parse_import()?);
        }

        while !self.is_at_end() {
            match self.parse_field_or_method()? {
                DeclType::Field(field) => program.fields.push(field),
                DeclType::Method(method) => program.methods.push(method),
            }
        }

        Ok(program)
    }

    fn parse_import(&mut self) -> Result<ImportDecl, ParseError> {
        self.expect(&Token::Import)?;
        let name = self.expect_identifier()?;
        self.expect(&Token::Semicolon)?;

        Ok(ImportDecl { name })
    }

    fn parse_field_or_method(&mut self) -> Result<DeclType, ParseError> {
        let return_type = if self.match_token(&Token::Void) {
            None
        } else {
            Some(self.parse_type()?)
        };

        let name = self.expect_identifier()?;

        if self.check(&Token::LeftParen) {
            Ok(DeclType::Method(self.parse_method_rest(return_type, name)?))
        } else if self.check(&Token::LeftBracket) {
            if return_type.is_none() {
                return Err(self.error("void is not valid for field declarations"));
            }
            Ok(DeclType::Field(self.parse_array_field_rest(return_type.unwrap(), name)?))
        } else if self.check(&Token::Comma) || self.check(&Token::Semicolon) {
            if return_type.is_none() {
                return Err(self.error("void is not valid for field declarations"));
            }
            Ok(DeclType::Field(self.parse_scalar_field_rest(return_type.unwrap(), name)?))
        } else {
            Err(self.error(&format!("Expected '(', '[', ',' or ';' after identifier, found {:?}", self.peek())))
        }
    }

    fn parse_method_rest(&mut self, return_type: Option<Type>, name: Identifier) -> Result<MethodDecl, ParseError> {
        self.expect(&Token::LeftParen)?;

        let mut parameters = Vec::new();
        if !self.check(&Token::RightParen) {
            loop {
                let param_type = self.parse_type()?;
                let param_name = self.expect_identifier()?;
                parameters.push(Parameter {
                    param_type,
                    name: param_name,
                });

                if !self.match_token(&Token::Comma) {
                    break;
                }
            }
        }

        self.expect(&Token::RightParen)?;
        let body = self.parse_block()?;

        Ok(MethodDecl::new(return_type, name, parameters, body))
    }

    fn parse_scalar_field_rest(&mut self, field_type: Type, first_name: Identifier) -> Result<FieldDecl, ParseError> {
        let mut names = vec![first_name];

        while self.match_token(&Token::Comma) {
            names.push(self.expect_identifier()?);
        }

        self.expect(&Token::Semicolon)?;

        Ok(FieldDecl::ScalarField { field_type, names })
    }

    fn parse_array_field_rest(&mut self, field_type: Type, first_name: Identifier) -> Result<FieldDecl, ParseError> {
        let mut arrays = Vec::new();

        self.expect(&Token::LeftBracket)?;
        let size = self.expect_int_literal()?;
        self.expect(&Token::RightBracket)?;
        arrays.push(ArrayFieldDecl {
            name: first_name,
            size,
        });

        while self.match_token(&Token::Comma) {
            let name = self.expect_identifier()?;
            self.expect(&Token::LeftBracket)?;
            let size = self.expect_int_literal()?;
            self.expect(&Token::RightBracket)?;
            arrays.push(ArrayFieldDecl { name, size });
        }

        self.expect(&Token::Semicolon)?;

        Ok(FieldDecl::ArrayField { field_type, arrays })
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        if self.match_token(&Token::Int) {
            Ok(Type::Int)
        } else if self.match_token(&Token::Long) {
            Ok(Type::Long)
        } else if self.match_token(&Token::Bool) {
            Ok(Type::Bool)
        } else {
            Err(self.error(&format!("Expected type (int, long, or bool), found {:?}", self.peek())))
        }
    }

    fn parse_block(&mut self) -> Result<Block, ParseError> {
        self.expect(&Token::LeftBrace)?;

        let mut block = Block::new();

        while self.is_type_start() {
            block.field_decls.push(self.parse_block_field_decl()?);
        }

        while !self.check(&Token::RightBrace) && !self.is_at_end() {
            block.statements.push(self.parse_statement()?);
        }

        self.expect(&Token::RightBrace)?;

        Ok(block)
    }

    fn parse_block_field_decl(&mut self) -> Result<FieldDecl, ParseError> {
        let field_type = self.parse_type()?;
        let name = self.expect_identifier()?;

        if self.check(&Token::LeftBracket) {
            self.parse_array_field_rest(field_type, name)
        } else {
            self.parse_scalar_field_rest(field_type, name)
        }
    }

    fn is_type_start(&self) -> bool {
        matches!(self.peek(), Token::Int | Token::Long | Token::Bool)
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.peek() {
            Token::If => self.parse_if_statement(),
            Token::For => self.parse_for_statement(),
            Token::While => self.parse_while_statement(),
            Token::Return => self.parse_return_statement(),
            Token::Break => {
                self.advance();
                self.expect(&Token::Semicolon)?;
                Ok(Statement::Break)
            }
            Token::Continue => {
                self.advance();
                self.expect(&Token::Semicolon)?;
                Ok(Statement::Continue)
            }
            _ => {
                self.parse_assignment_or_method_call()
            }
        }
    }

    fn parse_if_statement(&mut self) -> Result<Statement, ParseError> {
        self.expect(&Token::If)?;
        self.expect(&Token::LeftParen)?;
        let condition = self.parse_expression()?;
        self.expect(&Token::RightParen)?;

        let then_block = self.parse_block()?;

        let else_block = if self.match_token(&Token::Else) {
            Some(self.parse_block()?)
        } else {
            None
        };

        Ok(Statement::If {
            condition,
            then_block,
            else_block,
        })
    }

    fn parse_for_statement(&mut self) -> Result<Statement, ParseError> {
        self.expect(&Token::For)?;
        self.expect(&Token::LeftParen)?;

        let init_id = self.expect_identifier()?;
        self.expect(&Token::Assign)?;
        let init_expr = self.parse_expression()?;
        self.expect(&Token::Semicolon)?;

        let condition = self.parse_expression()?;
        self.expect(&Token::Semicolon)?;

        let update = self.parse_for_update()?;
        self.expect(&Token::RightParen)?;

        let body = self.parse_block()?;

        Ok(Statement::For {
            init_id,
            init_expr,
            condition,
            update,
            body,
        })
    }

    fn parse_for_update(&mut self) -> Result<ForUpdate, ParseError> {
        let location = self.parse_location()?;
        let expr = self.parse_assign_expr()?;

        Ok(ForUpdate { location, expr })
    }

    fn parse_while_statement(&mut self) -> Result<Statement, ParseError> {
        self.expect(&Token::While)?;
        self.expect(&Token::LeftParen)?;
        let condition = self.parse_expression()?;
        self.expect(&Token::RightParen)?;

        let body = self.parse_block()?;

        Ok(Statement::While { condition, body })
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParseError> {
        self.expect(&Token::Return)?;

        let expr = if self.check(&Token::Semicolon) {
            None
        } else {
            Some(self.parse_expression()?)
        };

        self.expect(&Token::Semicolon)?;

        Ok(Statement::Return(expr))
    }

    fn parse_assignment_or_method_call(&mut self) -> Result<Statement, ParseError> {
        if let Token::Identifier(_) = self.peek() {
            let checkpoint = self.current;
            let id = self.expect_identifier()?;

            if self.check(&Token::LeftParen) {
                self.current = checkpoint; // backtrack
                let call = self.parse_method_call()?;
                self.expect(&Token::Semicolon)?;
                return Ok(Statement::MethodCall(call));
            } else {
                self.current = checkpoint;
            }
        }

        let location = self.parse_location()?;
        let expr = self.parse_assign_expr()?;
        self.expect(&Token::Semicolon)?;

        Ok(Statement::Assignment { location, expr })
    }

    fn parse_location(&mut self) -> Result<Location, ParseError> {
        let name = self.expect_identifier()?;

        if self.match_token(&Token::LeftBracket) {
            let index = self.parse_expression()?;
            self.expect(&Token::RightBracket)?;
            Ok(Location::Array {
                name,
                index: Box::new(index),
            })
        } else {
            Ok(Location::Scalar(name))
        }
    }

    fn parse_assign_expr(&mut self) -> Result<AssignExpr, ParseError> {
        if self.match_token(&Token::Increment) {
            Ok(AssignExpr::Increment(IncrementOp::Inc))
        } else if self.match_token(&Token::Decrement) {
            Ok(AssignExpr::Increment(IncrementOp::Dec))
        } else {
            let op = self.parse_assign_op()?;
            let expr = self.parse_expression()?;
            Ok(AssignExpr::Assign {
                op,
                expr,
            })
        }
    }

    fn parse_assign_op(&mut self) -> Result<AssignOp, ParseError> {
        if self.match_token(&Token::Assign) {
            Ok(AssignOp::Eq)
        } else if self.match_token(&Token::PlusAssign) {
            Ok(AssignOp::AddEq)
        } else if self.match_token(&Token::MinusAssign) {
            Ok(AssignOp::SubEq)
        } else if self.match_token(&Token::StarAssign) {
            Ok(AssignOp::MulEq)
        } else if self.match_token(&Token::SlashAssign) {
            Ok(AssignOp::DivEq)
        } else if self.match_token(&Token::PercentAssign) {
            Ok(AssignOp::ModEq)
        } else {
            Err(self.error(&format!("Expected assignment operator, found {:?}", self.peek())))
        }
    }

    fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        self.parse_expression_bp(Precedence::None)
    }

    fn parse_expression_bp(&mut self, min_bp: Precedence) -> Result<Expr, ParseError> {
        let mut left = self.parse_prefix()?;

        while !self.is_at_end() {
            let op_bp = self.infix_binding_power();

            if op_bp <= min_bp {
                break;
            }

            left = self.parse_infix(left, op_bp)?;
        }

        Ok(left)
    }

    fn parse_prefix(&mut self) -> Result<Expr, ParseError> {
        match self.peek() {
            Token::Minus => {
                self.advance();
                let expr = self.parse_expression_bp(Precedence::Unary)?;
                Ok(Expr::Unary {
                    op: UnaryOp::Neg,
                    expr: Box::new(expr),
                })
            }
            Token::LogicalNot => {
                self.advance();
                let expr = self.parse_expression_bp(Precedence::Unary)?;
                Ok(Expr::Unary {
                    op: UnaryOp::Not,
                    expr: Box::new(expr),
                })
            }

            Token::Int => {
                self.advance();
                self.expect(&Token::LeftParen)?;
                let expr = self.parse_expression()?;
                self.expect(&Token::RightParen)?;
                Ok(Expr::IntCast(Box::new(expr)))
            }
            Token::Long => {
                self.advance();
                self.expect(&Token::LeftParen)?;
                let expr = self.parse_expression()?;
                self.expect(&Token::RightParen)?;
                Ok(Expr::LongCast(Box::new(expr)))
            }

            Token::Len => {
                self.advance();
                self.expect(&Token::LeftParen)?;
                let id = self.expect_identifier()?;
                self.expect(&Token::RightParen)?;
                Ok(Expr::Len(id))
            }

            Token::LeftParen => {
                self.advance();
                let expr = self.parse_expression()?;
                self.expect(&Token::RightParen)?;
                Ok(expr)
            }

            Token::IntLiteral(_) | Token::LongLiteral(_) | Token::CharLiteral(_) |
            Token::True | Token::False => {
                Ok(Expr::Literal(self.parse_literal()?))
            }

            Token::Identifier(_) => {
                let checkpoint = self.current;
                let id = self.expect_identifier()?;

                if self.check(&Token::LeftParen) {
                    self.current = checkpoint;
                    Ok(Expr::MethodCall(self.parse_method_call()?))
                } else if self.check(&Token::LeftBracket) {
                    self.expect(&Token::LeftBracket)?;
                    let index = self.parse_expression()?;
                    self.expect(&Token::RightBracket)?;
                    Ok(Expr::Location(Location::Array {
                        name: id,
                        index: Box::new(index),
                    }))
                } else {
                    Ok(Expr::Location(Location::Scalar(id)))
                }
            }

            _ => Err(self.error(&format!("Expected expression, found {:?}", self.peek()))),
        }
    }

    fn parse_infix(&mut self, left: Expr, op_bp: Precedence) -> Result<Expr, ParseError> {
        let op = self.parse_binary_op()?;
        let right = self.parse_expression_bp(op_bp)?;

        Ok(Expr::Binary {
            left: Box::new(left),
            op,
            right: Box::new(right),
        })
    }

    fn infix_binding_power(&self) -> Precedence {
        match self.peek() {
            Token::LogicalOr => Precedence::Or,
            Token::LogicalAnd => Precedence::And,
            Token::Equal | Token::NotEqual => Precedence::Equality,
            Token::LessThan | Token::GreaterThan |
            Token::LessThanEqual | Token::GreaterThanEqual => Precedence::Comparison,
            Token::Plus | Token::Minus => Precedence::Term,
            Token::Star | Token::Slash | Token::Percent => Precedence::Factor,
            _ => Precedence::None,
        }
    }

    fn parse_binary_op(&mut self) -> Result<BinOp, ParseError> {
        let token = self.peek().clone();
        self.advance();

        match token {
            Token::Plus => Ok(BinOp::Add),
            Token::Minus => Ok(BinOp::Sub),
            Token::Star => Ok(BinOp::Mul),
            Token::Slash => Ok(BinOp::Div),
            Token::Percent => Ok(BinOp::Mod),
            Token::LessThan => Ok(BinOp::Lt),
            Token::GreaterThan => Ok(BinOp::Gt),
            Token::LessThanEqual => Ok(BinOp::Le),
            Token::GreaterThanEqual => Ok(BinOp::Ge),
            Token::Equal => Ok(BinOp::Eq),
            Token::NotEqual => Ok(BinOp::Ne),
            Token::LogicalAnd => Ok(BinOp::And),
            Token::LogicalOr => Ok(BinOp::Or),
            _ => Err(self.error(&format!("Expected binary operator, found {:?}", token))),
        }
    }

    fn parse_literal(&mut self) -> Result<Literal, ParseError> {
        match self.peek().clone() {
            Token::IntLiteral(s) => {
                self.advance();
                let value = self.parse_int_value(&s)?;
                Ok(Literal::Int(value))
            }
            Token::LongLiteral(s) => {
                self.advance();
                let value = self.parse_long_value(&s)?;
                Ok(Literal::Long(value))
            }
            Token::CharLiteral(c) => {
                self.advance();
                Ok(Literal::Char(c))
            }
            Token::True => {
                self.advance();
                Ok(Literal::Bool(true))
            }
            Token::False => {
                self.advance();
                Ok(Literal::Bool(false))
            }
            _ => Err(self.error(&format!("Expected literal, found {:?}", self.peek()))),
        }
    }

    fn parse_int_value(&self, s: &str) -> Result<i32, ParseError> {
        if s.starts_with("0x") || s.starts_with("0X") {
            i32::from_str_radix(&s[2..], 16)
                .map_err(|_| self.error(&format!("Invalid hex integer: {}", s)))
        } else {
            s.parse::<i32>()
                .map_err(|_| self.error(&format!("Invalid integer: {}", s)))
        }
    }

    fn parse_long_value(&self, s: &str) -> Result<i64, ParseError> {
        if s.starts_with("0x") || s.starts_with("0X") {
            i64::from_str_radix(&s[2..], 16)
                .map_err(|_| self.error(&format!("Invalid hex long: {}", s)))
        } else {
            s.parse::<i64>()
                .map_err(|_| self.error(&format!("Invalid long: {}", s)))
        }
    }

    fn parse_method_call(&mut self) -> Result<MethodCall, ParseError> {
        let name = self.expect_identifier()?;
        self.expect(&Token::LeftParen)?;

        let mut arguments = Vec::new();
        if !self.check(&Token::RightParen) {
            loop {
                arguments.push(self.parse_extern_arg()?);

                if !self.match_token(&Token::Comma) {
                    break;
                }
            }
        }

        self.expect(&Token::RightParen)?;

        Ok(MethodCall { name, arguments })
    }

    fn parse_extern_arg(&mut self) -> Result<ExternArg, ParseError> {
        if let Token::StringLiteral(s) = self.peek().clone() {
            self.advance();
            Ok(ExternArg::StringLiteral(s))
        } else {
            Ok(ExternArg::Expr(self.parse_expression()?))
        }
    }

    fn peek(&self) -> &Token {
        if self.is_at_end() {
            &Token::Semicolon
        } else {
            &self.tokens[self.current]
        }
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.tokens[self.current - 1].clone()
    }

    fn check(&self, token: &Token) -> bool {
        if self.is_at_end() {
            false
        } else {
            std::mem::discriminant(self.peek()) == std::mem::discriminant(token)
        }
    }

    fn match_token(&mut self, token: &Token) -> bool {
        if self.check(token) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, token: &Token) -> Result<(), ParseError> {
        if self.check(token) {
            self.advance();
            Ok(())
        } else {
            Err(self.error(&format!("Expected {:?}, found {:?}", token, self.peek())))
        }
    }

    fn expect_identifier(&mut self) -> Result<Identifier, ParseError> {
        if let Token::Identifier(name) = self.peek().clone() {
            self.advance();
            Ok(Identifier::new(name))
        } else {
            Err(self.error(&format!("Expected identifier, found {:?}", self.peek())))
        }
    }

    fn expect_int_literal(&mut self) -> Result<i32, ParseError> {
        if let Token::IntLiteral(s) = self.peek().clone() {
            self.advance();
            self.parse_int_value(&s)
        } else {
            Err(self.error(&format!("Expected integer literal, found {:?}", self.peek())))
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }

    fn error(&self, message: &str) -> ParseError {
        ParseError {
            message: message.to_string(),
            position: self.current,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::lexer::Lexer;

    fn parse(input: &str) -> Result<Program, ParseError> {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        parser.parse_program()
    }

    #[test]
    fn test_parse_import() {
        let input = "import printf;";
        let program = parse(input).unwrap();
        assert_eq!(program.imports.len(), 1);
        assert_eq!(program.imports[0].name.0, "printf");
    }

    #[test]
    fn test_parse_field_declarations() {
        let input = r"
            int x;
            int a, b, c;
            int arr[10];
            long larr[5];
        ";
        let program = parse(input).unwrap();
        assert_eq!(program.fields.len(), 4);
    }

    #[test]
    fn test_parse_simple_method() {
        let input = r"
            void main() {
                int x;
                x = 5;
            }
        ";
        let program = parse(input).unwrap();
        assert_eq!(program.methods.len(), 1);
        assert_eq!(program.methods[0].name.0, "main");
        assert!(program.methods[0].is_void());
    }

    #[test]
    fn test_parse_method_with_parameters() {
        let input = r"
            int add(int a, int b) {
                return a + b;
            }
        ";
        let program = parse(input).unwrap();
        assert_eq!(program.methods.len(), 1);
        assert_eq!(program.methods[0].parameters.len(), 2);
    }

    #[test]
    fn test_parse_if_statement() {
        let input = r"
            void main() {
                if (x > 5) {
                    y = 10;
                }
            }
        ";
        let program = parse(input).unwrap();
        assert!(matches!(
            program.methods[0].body.statements[0],
            Statement::If { .. }
        ));
    }

    #[test]
    fn test_parse_if_else_statement() {
        let input = r"
            void main() {
                if (x > 5) {
                    y = 10;
                } else {
                    y = 0;
                }
            }
        ";
        let program = parse(input).unwrap();
        if let Statement::If { else_block, .. } = &program.methods[0].body.statements[0] {
            assert!(else_block.is_some());
        } else {
            panic!("Expected if statement");
        }
    }

    #[test]
    fn test_parse_for_loop() {
        let input = r"
            void main() {
                int i;
                for (i = 0; i < 10; i++) {
                    x = x + 1;
                }
            }
        ";
        let program = parse(input).unwrap();
        assert!(matches!(
            program.methods[0].body.statements[0],
            Statement::For { .. }
        ));
    }

    #[test]
    fn test_parse_while_loop() {
        let input = r"
            void main() {
                while (x > 0) {
                    x--;
                }
            }
        ";
        let program = parse(input).unwrap();
        assert!(matches!(
            program.methods[0].body.statements[0],
            Statement::While { .. }
        ));
    }

    #[test]
    fn test_parse_expressions() {
        let input = r"
            void main() {
                int x;
                x = 1 + 2 * 3;
                x = (1 + 2) * 3;
                x = -5;
                x = !true;
            }
        ";
        let program = parse(input).unwrap();
        assert_eq!(program.methods[0].body.statements.len(), 4);
    }

    #[test]
    fn test_parse_array_access() {
        let input = r"
            void main() {
                int arr[10];
                arr[5] = 42;
                x = arr[0];
            }
        ";
        let program = parse(input).unwrap();
        assert_eq!(program.methods[0].body.statements.len(), 2);
    }

    #[test]
    fn test_parse_method_call() {
        let input = r#"
            import printf;

            void main() {
                printf("Hello, world!");
            }
        "#;
        let program = parse(input).unwrap();
        assert!(matches!(
            program.methods[0].body.statements[0],
            Statement::MethodCall(_)
        ));
    }

    #[test]
    fn test_parse_type_casts() {
        let input = r"
            void main() {
                long x;
                int y;
                x = long(y);
                y = int(x);
            }
        ";
        let program = parse(input).unwrap();
        assert_eq!(program.methods[0].body.statements.len(), 2);
    }

    #[test]
    fn test_parse_len_operator() {
        let input = r"
            void main() {
                int arr[10];
                int size;
                size = len(arr);
            }
        ";
        let program = parse(input).unwrap();
        if let Statement::Assignment { expr, .. } = &program.methods[0].body.statements[0] {
            if let AssignExpr::Assign { expr, .. } = expr {
                assert!(matches!(*expr, Expr::Len(_)));
            }
        }
    }

    #[test]
    fn test_parse_compound_assignments() {
        let input = r"
            void main() {
                int x;
                x += 5;
                x -= 3;
                x *= 2;
                x /= 4;
                x %= 3;
                x++;
                x--;
            }
        ";
        let program = parse(input).unwrap();
        assert_eq!(program.methods[0].body.statements.len(), 7);
    }

    #[test]
    fn test_parse_complex_expression() {
        let input = r"
            void main() {
                bool result;
                result = (x > 0 && y < 10) || (z == 5 && w != 3);
            }
        ";
        let program = parse(input).unwrap();
        assert_eq!(program.methods[0].body.statements.len(), 1);
    }

    #[test]
    fn test_parse_complete_program() {
        let input = r"
            import printf;

            int numbers[10];
            int count;

            void swap(int i, int j) {
                int temp;
                temp = numbers[i];
                numbers[i] = numbers[j];
                numbers[j] = temp;
            }

            void main() {
                int i;
                count = len(numbers);
                for (i = 0; i < count; i++) {
                    numbers[i] = i * 2;
                }
            }
        ";
        let program = parse(input).unwrap();
        assert_eq!(program.imports.len(), 1);
        assert_eq!(program.fields.len(), 2);
        assert_eq!(program.methods.len(), 2);
        assert!(program.has_main());
    }

    #[test]
    fn test_operator_precedence() {
        let input = r"
            void main() {
                int x;
                x = 1 + 2 * 3;  // Should be 1 + (2 * 3) = 7
            }
        ";
        let program = parse(input).unwrap();

        if let Statement::Assignment { expr, .. } = &program.methods[0].body.statements[0] {
            if let AssignExpr::Assign { expr, .. } = expr {
                if let Expr::Binary { left, op, right } = &*expr {
                    assert_eq!(*op, BinOp::Add);
                    assert!(matches!(**left, Expr::Literal(Literal::Int(1))));
                    if let Expr::Binary { op, .. } = &**right {
                        assert_eq!(*op, BinOp::Mul);
                    } else {
                        panic!("Expected multiplication on right side");
                    }
                } else {
                    panic!("Expected binary expression");
                }
            }
        }
    }

    #[test]
    fn test_error_missing_semicolon() {
        let input = r"
            void main() {
                int x
            }
        ";
        let result = parse(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_error_invalid_type() {
        let input = r"
            string x;
        ";
        let result = parse(input);
        assert!(result.is_err());
    }
}