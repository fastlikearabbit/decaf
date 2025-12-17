#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    If,
    Bool,
    Break,
    Import,
    Continue,
    Else,
    False,
    For,
    While,
    Int,
    Long,
    Return,
    Len,
    True,
    Void,

    Plus,
    Minus,
    Star,
    Slash,
    Percent,

    Assign,
    PlusAssign,
    MinusAssign,
    StarAssign,
    SlashAssign,
    PercentAssign,

    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,

    Equal,
    NotEqual,

    LogicalAnd,
    LogicalOr,
    LogicalNot,

    Increment,
    Decrement,

    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,

    Comma,
    Semicolon,

    IntLiteral(String),
    LongLiteral(String),

    CharLiteral(char),
    StringLiteral(String),

    Identifier(String),
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::If => write!(f, "if"),
            Token::Bool => write!(f, "bool"),
            Token::Break => write!(f, "break"),
            Token::Import => write!(f, "import"),
            Token::Continue => write!(f, "continue"),
            Token::Else => write!(f, "else"),
            Token::False => write!(f, "false"),
            Token::For => write!(f, "for"),
            Token::While => write!(f, "while"),
            Token::Int => write!(f, "int"),
            Token::Long => write!(f, "long"),
            Token::Return => write!(f, "return"),
            Token::Void => write!(f, "void"),

            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Percent => write!(f, "%"),

            Token::Assign => write!(f, "="),
            Token::PlusAssign => write!(f, "+="),
            Token::MinusAssign => write!(f, "-="),
            Token::StarAssign => write!(f, "*="),
            Token::SlashAssign => write!(f, "/="),
            Token::PercentAssign => write!(f, "%="),

            Token::LessThan => write!(f, "<"),
            Token::GreaterThan => write!(f, ">"),
            Token::LessThanEqual => write!(f, "<="),
            Token::GreaterThanEqual => write!(f, ">="),

            Token::Equal => write!(f, "=="),
            Token::NotEqual => write!(f, "!="),

            Token::LogicalAnd => write!(f, "&&"),
            Token::LogicalOr => write!(f, "||"),
            Token::LogicalNot => write!(f, "!"),

            Token::Increment => write!(f, "++"),
            Token::Decrement => write!(f, "--"),

            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::LeftBrace => write!(f, "{{"),
            Token::RightBrace => write!(f, "}}"),
            Token::LeftBracket => write!(f, "["),
            Token::RightBracket => write!(f, "]"),

            Token::Comma => write!(f, ","),
            Token::Semicolon => write!(f, ";"),

            Token::IntLiteral(literal) => write!(f, "IntLiteral: {}", literal),
            Token::LongLiteral(literal) => write!(f, "LongLiteral: {}", literal),

            Token::CharLiteral(literal) => write!(f, "CharLiteral: {}", literal),
            Token::StringLiteral(literal) => write!(f, "StringLiteral: {}", literal),

            Token::Identifier(literal) => write!(f, "Identifier: {}", literal),
        }
    }
}
