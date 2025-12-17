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

impl Program {
    pub fn new() -> Self {
        Program {
            imports: Vec::new(),
            fields: Vec::new(),
            methods: Vec::new(),
        }
    }

    pub fn find_method(&self, name: &str) -> Option<&MethodDecl> {
        self.methods.iter().find(|m| m.name.0 == name)
    }

    pub fn has_main(&self) -> bool {
        self.find_method("main").is_some()
    }
}

impl MethodDecl {
    pub fn new(
        return_type: Option<Type>,
        name: Identifier,
        parameters: Vec<Parameter>,
        body: Block,
    ) -> Self {
        MethodDecl {
            return_type,
            name,
            parameters,
            body,
        }
    }

    pub fn is_void(&self) -> bool {
        self.return_type.is_none()
    }
}

impl Block {
    pub fn new() -> Self {
        Block {
            field_decls: Vec::new(),
            statements: Vec::new(),
        }
    }

    pub fn with_statements(statements: Vec<Statement>) -> Self {
        Block {
            field_decls: Vec::new(),
            statements,
        }
    }
}

impl Expr {
    pub fn binary(left: Expr, op: BinOp, right: Expr) -> Self {
        Expr::Binary {
            left: Box::new(left),
            op,
            right: Box::new(right),
        }
    }

    pub fn unary(op: UnaryOp, expr: Expr) -> Self {
        Expr::Unary {
            op,
            expr: Box::new(expr),
        }
    }

    pub fn int_lit(value: i32) -> Self {
        Expr::Literal(Literal::Int(value))
    }

    pub fn long_lit(value: i64) -> Self {
        Expr::Literal(Literal::Long(value))
    }

    pub fn bool_lit(value: bool) -> Self {
        Expr::Literal(Literal::Bool(value))
    }

    pub fn char_lit(value: char) -> Self {
        Expr::Literal(Literal::Char(value))
    }
}

impl BinOp {
    pub fn is_arithmetic(&self) -> bool {
        matches!(
            self,
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod
        )
    }

    pub fn is_relational(&self) -> bool {
        matches!(self, BinOp::Lt | BinOp::Gt | BinOp::Le | BinOp::Ge)
    }

    pub fn is_equality(&self) -> bool {
        matches!(self, BinOp::Eq | BinOp::Ne)
    }

    pub fn is_conditional(&self) -> bool {
        matches!(self, BinOp::And | BinOp::Or)
    }
}

impl Identifier {
    pub fn new(name: String) -> Self {
        Identifier(name)
    }
}

impl Location {
    pub fn scalar(name: String) -> Self {
        Location::Scalar(Identifier::new(name))
    }

    pub fn array(name: String, index: Expr) -> Self {
        Location::Array {
            name: Identifier::new(name),
            index: Box::new(index),
        }
    }
}