use std::fmt;
use crate::parser::token::Token;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexError {
    pub message: String,
    pub line: usize,
    pub column: usize,
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Lexical error at line {}, column {}: {}",
               self.line, self.column, self.message)
    }
}

pub struct Lexer {
    input: Vec<char>,
    current: usize,
    line: usize,
    column: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Lexer {
            input: input.chars().collect(),
            current: 0,
            line: 1,
            column: 1,
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();

        while !self.is_at_end() {
            self.skip_whitespace_and_comments()?;

            if self.is_at_end() {
                break;
            }

            tokens.push(self.next_token()?);
        }

        Ok(tokens)
    }

    fn next_token(&mut self) -> Result<Token, LexError> {
        let c = self.peek();

        match c {
            '(' => {
                self.advance();
                Ok(Token::LeftParen)
            }
            ')' => {
                self.advance();
                Ok(Token::RightParen)
            }
            '{' => {
                self.advance();
                Ok(Token::LeftBrace)
            }
            '}' => {
                self.advance();
                Ok(Token::RightBrace)
            }
            '[' => {
                self.advance();
                Ok(Token::LeftBracket)
            }
            ']' => {
                self.advance();
                Ok(Token::RightBracket)
            }
            ',' => {
                self.advance();
                Ok(Token::Comma)
            }
            ';' => {
                self.advance();
                Ok(Token::Semicolon)
            }

            '+' => {
                self.advance();
                if self.match_char('=') {
                    Ok(Token::PlusAssign)
                } else if self.match_char('+') {
                    Ok(Token::Increment)
                } else {
                    Ok(Token::Plus)
                }
            }
            '-' => {
                self.advance();
                if self.match_char('=') {
                    Ok(Token::MinusAssign)
                } else if self.match_char('-') {
                    Ok(Token::Decrement)
                } else {
                    Ok(Token::Minus)
                }
            }
            '*' => {
                self.advance();
                if self.match_char('=') {
                    Ok(Token::StarAssign)
                } else {
                    Ok(Token::Star)
                }
            }
            '/' => {
                self.advance();
                if self.match_char('=') {
                    Ok(Token::SlashAssign)
                } else {
                    Ok(Token::Slash)
                }
            }
            '%' => {
                self.advance();
                if self.match_char('=') {
                    Ok(Token::PercentAssign)
                } else {
                    Ok(Token::Percent)
                }
            }
            '=' => {
                self.advance();
                if self.match_char('=') {
                    Ok(Token::Equal)
                } else {
                    Ok(Token::Assign)
                }
            }
            '<' => {
                self.advance();
                if self.match_char('=') {
                    Ok(Token::LessThanEqual)
                } else {
                    Ok(Token::LessThan)
                }
            }
            '>' => {
                self.advance();
                if self.match_char('=') {
                    Ok(Token::GreaterThanEqual)
                } else {
                    Ok(Token::GreaterThan)
                }
            }
            '!' => {
                self.advance();
                if self.match_char('=') {
                    Ok(Token::NotEqual)
                } else {
                    Ok(Token::LogicalNot)
                }
            }
            '&' => {
                self.advance();
                if self.match_char('&') {
                    Ok(Token::LogicalAnd)
                } else {
                    Err(self.error("Expected '&&', found single '&'"))
                }
            }
            '|' => {
                self.advance();
                if self.match_char('|') {
                    Ok(Token::LogicalOr)
                } else {
                    Err(self.error("Expected '||', found single '|'"))
                }
            }

            '"' => self.scan_string_literal(),

            '\'' => self.scan_char_literal(),

            '0'..='9' => self.scan_number(),

            'a'..='z' | 'A'..='Z' | '_' => self.scan_identifier_or_keyword(),

            _ => Err(self.error(&format!("Unexpected character: '{}'", c))),
        }
    }

    fn scan_identifier_or_keyword(&mut self) -> Result<Token, LexError> {
        let start = self.current;

        while !self.is_at_end() {
            let c = self.peek();
            if c.is_alphanumeric() || c == '_' {
                self.advance();
            } else {
                break;
            }
        }

        let text: String = self.input[start..self.current].iter().collect();

        let token = match text.as_str() {
            "if" => Token::If,
            "bool" => Token::Bool,
            "break" => Token::Break,
            "import" => Token::Import,
            "continue" => Token::Continue,
            "else" => Token::Else,
            "false" => Token::False,
            "for" => Token::For,
            "while" => Token::While,
            "int" => Token::Int,
            "long" => Token::Long,
            "return" => Token::Return,
            "len" => Token::Len,
            "true" => Token::True,
            "void" => Token::Void,
            _ => Token::Identifier(text),
        };

        Ok(token)
    }

    fn scan_number(&mut self) -> Result<Token, LexError> {
        let start = self.current;

        if self.peek() == '0' && self.peek_next() == Some('x') {
            self.advance();
            self.advance();

            if !self.is_hex_digit(self.peek()) {
                return Err(self.error("Expected hex digit after '0x'"));
            }

            while !self.is_at_end() && self.is_hex_digit(self.peek()) {
                self.advance();
            }
        } else {
            while !self.is_at_end() && self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        let is_long = if !self.is_at_end() && self.peek() == 'L' {
            self.advance();
            true
        } else {
            false
        };

        let literal: String = self.input[start..self.current].iter().collect();

        if is_long {
            let num_str = literal.trim_end_matches('L').to_string();
            Ok(Token::LongLiteral(num_str))
        } else {
            Ok(Token::IntLiteral(literal))
        }
    }

    fn scan_string_literal(&mut self) -> Result<Token, LexError> {
        self.advance();

        let mut result = String::new();

        while !self.is_at_end() && self.peek() != '"' {
            let c = self.peek();

            if c == '\\' {
                self.advance();
                if self.is_at_end() {
                    return Err(self.error("Unterminated string literal"));
                }

                let escaped = self.peek();
                self.advance();

                match escaped {
                    'n' => result.push('\n'),
                    't' => result.push('\t'),
                    'r' => result.push('\r'),
                    'f' => result.push('\x0C'), // form feed
                    '\\' => result.push('\\'),
                    '"' => result.push('"'),
                    '\'' => result.push('\''),
                    _ => return Err(self.error(&format!("Invalid escape sequence: \\{}", escaped))),
                }
            } else {
                if c as u32 >= 32 && c as u32 <= 126 {
                    result.push(c);
                    self.advance();
                } else {
                    return Err(self.error(&format!("Invalid character in string: {:?}", c)));
                }
            }
        }

        if self.is_at_end() {
            return Err(self.error("Unterminated string literal"));
        }

        self.advance();

        Ok(Token::StringLiteral(result))
    }

    fn scan_char_literal(&mut self) -> Result<Token, LexError> {
        self.advance();

        if self.is_at_end() {
            return Err(self.error("Unterminated character literal"));
        }

        let c = self.peek();
        let result_char = if c == '\\' {
            self.advance();
            if self.is_at_end() {
                return Err(self.error("Unterminated character literal"));
            }

            let escaped = self.peek();
            self.advance();

            match escaped {
                'n' => '\n',
                't' => '\t',
                'r' => '\r',
                'f' => '\x0C',
                '\\' => '\\',
                '"' => '"',
                '\'' => '\'',
                _ => return Err(self.error(&format!("Invalid escape sequence: \\{}", escaped))),
            }
        } else {
            if c as u32 >= 32 && c as u32 <= 126 {
                self.advance();
                c
            } else {
                return Err(self.error(&format!("Invalid character literal: {:?}", c)));
            }
        };

        if self.is_at_end() || self.peek() != '\'' {
            return Err(self.error("Unterminated character literal"));
        }

        self.advance();

        Ok(Token::CharLiteral(result_char))
    }

    fn skip_whitespace_and_comments(&mut self) -> Result<(), LexError> {
        loop {
            if self.is_at_end() {
                return Ok(());
            }

            let c = self.peek();

            match c {
                ' ' | '\t' | '\r' => {
                    self.advance();
                }
                '\n' => {
                    self.advance();
                }
                '/' => {
                    if self.peek_next() == Some('/') {
                        self.skip_line_comment();
                    } else if self.peek_next() == Some('*') {
                        self.skip_block_comment()?;
                    } else {
                        return Ok(());
                    }
                }
                _ => return Ok(()),
            }
        }
    }

    fn skip_line_comment(&mut self) {
        while !self.is_at_end() && self.peek() != '\n' {
            self.advance();
        }
    }

    fn skip_block_comment(&mut self) -> Result<(), LexError> {
        self.advance();
        self.advance();

        while !self.is_at_end() {
            if self.peek() == '*' && self.peek_next() == Some('/') {
                self.advance();
                self.advance();
                return Ok(());
            }
            self.advance();
        }

        Err(self.error("Unterminated block comment"))
    }

    fn is_hex_digit(&self, c: char) -> bool {
        c.is_ascii_hexdigit()
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.input[self.current]
        }
    }

    fn peek_next(&self) -> Option<char> {
        if self.current + 1 >= self.input.len() {
            None
        } else {
            Some(self.input[self.current + 1])
        }
    }

    fn advance(&mut self) -> char {
        let c = self.input[self.current];
        self.current += 1;

        if c == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }

        c
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.peek() != expected {
            false
        } else {
            self.advance();
            true
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.input.len()
    }

    fn error(&self, message: &str) -> LexError {
        LexError {
            message: message.to_string(),
            line: self.line,
            column: self.column,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keywords() {
        let input = "if bool break import continue else false for while int long return len true void";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens.len(), 15);
        assert_eq!(tokens[0], Token::If);
        assert_eq!(tokens[1], Token::Bool);
        assert_eq!(tokens[14], Token::Void);
    }

    #[test]
    fn test_operators() {
        let input = "+ - * / % = += -= *= /= %=";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens[0], Token::Plus);
        assert_eq!(tokens[5], Token::Assign);
        assert_eq!(tokens[6], Token::PlusAssign);
    }

    #[test]
    fn test_comparison_operators() {
        let input = "< > <= >= == != && || !";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens[0], Token::LessThan);
        assert_eq!(tokens[2], Token::LessThanEqual);
        assert_eq!(tokens[4], Token::Equal);
        assert_eq!(tokens[6], Token::LogicalAnd);
    }

    #[test]
    fn test_increment_decrement() {
        let input = "++ --";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens[0], Token::Increment);
        assert_eq!(tokens[1], Token::Decrement);
    }

    #[test]
    fn test_identifiers() {
        let input = "foo bar_baz MyClass _underscore abc123";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();

        match &tokens[0] {
            Token::Identifier(s) => assert_eq!(s, "foo"),
            _ => panic!("Expected identifier"),
        }
        match &tokens[1] {
            Token::Identifier(s) => assert_eq!(s, "bar_baz"),
            _ => panic!("Expected identifier"),
        }
    }

    #[test]
    fn test_numbers() {
        let input = "42 123 0xFF 0x1A3 999L 0xDEADBEEFL";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();

        match &tokens[0] {
            Token::IntLiteral(s) => assert_eq!(s, "42"),
            _ => panic!("Expected int literal"),
        }
        match &tokens[2] {
            Token::IntLiteral(s) => assert_eq!(s, "0xFF"),
            _ => panic!("Expected hex int literal"),
        }
        match &tokens[4] {
            Token::LongLiteral(s) => assert_eq!(s, "999"),
            _ => panic!("Expected long literal"),
        }
        match &tokens[5] {
            Token::LongLiteral(s) => assert_eq!(s, "0xDEADBEEF"),
            _ => panic!("Expected hex long literal"),
        }
    }

    #[test]
    fn test_string_literals() {
        let input = r#""hello" "world\n" "tab\there""#;
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();

        match &tokens[0] {
            Token::StringLiteral(s) => assert_eq!(s, "hello"),
            _ => panic!("Expected string literal"),
        }
        match &tokens[1] {
            Token::StringLiteral(s) => assert_eq!(s, "world\n"),
            _ => panic!("Expected string literal"),
        }
    }

    #[test]
    fn test_char_literals() {
        let input = r#"'a' '\n' '\t' '\'' '\"'"#;
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();

        match tokens[0] {
            Token::CharLiteral(c) => assert_eq!(c, 'a'),
            _ => panic!("Expected char literal"),
        }
        match tokens[1] {
            Token::CharLiteral(c) => assert_eq!(c, '\n'),
            _ => panic!("Expected char literal"),
        }
    }

    #[test]
    fn test_comments() {
        let input = r#"
        // This is a line comment
        int x; // another comment
        /* This is a
           block comment */
        bool y;
        "#;
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens[0], Token::Int);
        assert_eq!(tokens[2], Token::Semicolon);
        assert_eq!(tokens[3], Token::Bool);
    }

    #[test]
    fn test_simple_program() {
        let input = r#"
        import printf;

        int[10] arr;

        void main() {
            int i;
            for (i = 0; i < 10; i++) {
                arr[i] = i * 2;
            }
        }
        "#;
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens[0], Token::Import);
        match &tokens[1] {
            Token::Identifier(s) => assert_eq!(s, "printf"),
            _ => panic!("Expected identifier"),
        }
        assert_eq!(tokens[2], Token::Semicolon);
    }

    #[test]
    fn test_error_unterminated_string() {
        let input = r#""unterminated"#;
        let mut lexer = Lexer::new(input);
        let result = lexer.tokenize();
        assert!(result.is_err());
    }

    #[test]
    fn test_error_invalid_escape() {
        let input = r#""\x""#;
        let mut lexer = Lexer::new(input);
        let result = lexer.tokenize();
        assert!(result.is_err());
    }
}