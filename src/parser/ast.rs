/// Abstract Syntax Tree for the Decaf Language

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Long,
    Bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i32),
    Long(i64),
    Char(char),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Identifier(pub String);

#[derive(Debug, Clone)]
pub struct Program {
    pub imports: Vec<ImportDecl>,
    pub fields: Vec<FieldDecl>,
    pub methods: Vec<MethodDecl>,
}

#[derive(Debug, Clone)]
pub struct ImportDecl {
    pub name: Identifier,
}

#[derive(Debug, Clone)]
pub enum FieldDecl {
    ScalarField {
        field_type: Type,
        names: Vec<Identifier>,
    },
    ArrayField {
        field_type: Type,
        arrays: Vec<ArrayFieldDecl>,
    },
}

#[derive(Debug, Clone)]
pub struct ArrayFieldDecl {
    pub name: Identifier,
    pub size: i32,
}

#[derive(Debug, Clone)]
pub struct MethodDecl {
    pub return_type: Option<Type>, // None for void
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub param_type: Type,
    pub name: Identifier,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub field_decls: Vec<FieldDecl>,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Assignment {
        location: Location,
        expr: AssignExpr,
    },
    MethodCall(MethodCall),
    If {
        condition: Expr,
        then_block: Block,
        else_block: Option<Block>,
    },
    For {
        init_id: Identifier,
        init_expr: Expr,
        condition: Expr,
        update: ForUpdate,
        body: Block,
    },
    While {
        condition: Expr,
        body: Block,
    },
    Return(Option<Expr>),
    Break,
    Continue,
}

#[derive(Debug, Clone)]
pub struct ForUpdate {
    pub location: Location,
    pub expr: AssignExpr,
}

#[derive(Debug, Clone)]
pub enum AssignExpr {
    Assign {
        op: AssignOp,
        expr: Expr,
    },
    Increment(IncrementOp),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignOp {
    Eq,          // =
    AddEq,       // +=
    SubEq,       // -=
    MulEq,       // *=
    DivEq,       // /=
    ModEq,       // %=
}

#[derive(Debug, Clone, PartialEq)]
pub enum IncrementOp {
    Inc,  // ++
    Dec,  // --
}

#[derive(Debug, Clone)]
pub enum Location {
    Scalar(Identifier),
    Array {
        name: Identifier,
        index: Box<Expr>,
    },
}

#[derive(Debug, Clone)]
pub enum Expr {
    Location(Location),
    MethodCall(MethodCall),
    Literal(Literal),
    IntCast(Box<Expr>),
    LongCast(Box<Expr>),
    Len(Identifier),
    Binary {
        left: Box<Expr>,
        op: BinOp,
        right: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    // Arithmetic operators
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    // Relational operators
    Lt,
    Gt,
    Le,
    Ge,
    // Equality operators
    Eq,
    Ne,
    // Conditional operators
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Neg,  // -
    Not,  // !
}

#[derive(Debug, Clone)]
pub struct MethodCall {
    pub name: Identifier,
    pub arguments: Vec<ExternArg>,
}

#[derive(Debug, Clone)]
pub enum ExternArg {
    Expr(Expr),
    StringLiteral(String),
}