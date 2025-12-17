//! IR, or intermediate representation for Decaf

use std::fmt;
use crate::parser::ast::Type;

pub use super::temp::{Temp, TempAllocator};
pub use super::label::{Label, LabelAllocator};

#[derive(Debug, Clone)]
pub struct Program {
    pub globals: Vec<GlobalDecl>,
    pub methods: Vec<Method>,
    pub temp_allocator: TempAllocator,
    pub label_allocator: LabelAllocator,
}

#[derive(Debug, Clone)]
pub enum GlobalDecl {
    Scalar {
        name: String,
        var_type: IrType,
    },
    Array {
        name: String,
        elem_type: IrType,
        size: i32,
    },
}

#[derive(Debug, Clone)]
pub struct Method {
    pub name: String,
    pub return_type: Option<IrType>,
    pub params: Vec<Param>,
    pub locals: Vec<LocalDecl>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub param_type: IrType,
    pub temp: Temp,
}

#[derive(Debug, Clone)]
pub enum LocalDecl {
    Scalar {
        name: String,
        var_type: IrType,
        temp: Temp,
    },
    Array {
        name: String,
        elem_type: IrType,
        size: i32,
        base_temp: Temp,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum IrType {
    Int,
    Long,
    Bool,
}

impl From<Type> for IrType {
    fn from(t: Type) -> Self {
        match t {
            Type::Int => IrType::Int,
            Type::Long => IrType::Long,
            Type::Bool => IrType::Bool,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Label(Label),
    Move {
        dest: Temp,
        source: Operand,
    },
    BinOp {
        dest: Temp,
        left: Operand,
        op: BinOp,
        right: Operand,
    },
    UnOp {
        dest: Temp,
        op: UnOp,
        operand: Operand,
    },
    Cast {
        dest: Temp,
        from_type: IrType,
        to_type: IrType,
        operand: Operand,
    },
    ArrayLoad {
        dest: Temp,
        array_base: String,
        index: Operand,
        elem_type: IrType,
    },
    ArrayStore {
        array_base: String,
        index: Operand,
        source: Operand,
        elem_type: IrType,
    },
    Jump {
        target: Label,
    },
    CondJump {
        condition: Operand,
        relop: RelOp,
        target: Label,
    },
    Call {
        dest: Option<Temp>,
        method: String,
        args: Vec<Operand>,
    },
    CallExtern {
        dest: Temp,
        method: String,
        args: Vec<ExternArg>,
    },
    Return {
        value: Option<Operand>,
    },
    Nop,
}

#[derive(Debug, Clone)]
pub enum Operand {
    IntConst(i32),
    LongConst(i64),
    BoolConst(bool),
    Temp(Temp),
    Global(String),
}

#[derive(Debug, Clone)]
pub enum ExternArg {
    Operand(Operand),
    StringLiteral(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Lt,
    Gt,
    Le,
    Ge,
    Eq,
    Ne,
    And,
    Or,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RelOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "# Globals")?;
        for global in &self.globals {
            writeln!(f, "{}", global)?;
        }
        writeln!(f)?;

        for method in &self.methods {
            writeln!(f, "{}", method)?;
            writeln!(f)?;
        }
        Ok(())
    }
}

impl fmt::Display for GlobalDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GlobalDecl::Scalar { name, var_type } => {
                write!(f, "global {} {}", var_type, name)
            }
            GlobalDecl::Array { name, elem_type, size } => {
                write!(f, "global {}[{}] {}", elem_type, size, name)
            }
        }
    }
}

impl fmt::Display for Method {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "method {} {}(",
               self.return_type.map_or("void".to_string(), |t| format!("{}", t)),
               self.name)?;
        for (i, param) in self.params.iter().enumerate() {
            if i > 0 { write!(f, ", ")?; }
            write!(f, "{} {} -> {}", param.param_type, param.name, param.temp)?;
        }
        writeln!(f, ") {{")?;

        if !self.locals.is_empty() {
            writeln!(f, "  # Locals")?;
            for local in &self.locals {
                writeln!(f, "  {}", local)?;
            }
            writeln!(f)?;
        }

        for stmt in &self.body {
            writeln!(f, "  {}", stmt)?;
        }
        writeln!(f, "}}")
    }
}

impl fmt::Display for LocalDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LocalDecl::Scalar { name, var_type, temp } => {
                write!(f, "local {} {} -> {}", var_type, name, temp)
            }
            LocalDecl::Array { name, elem_type, size, base_temp } => {
                write!(f, "local {}[{}] {} -> {}", elem_type, size, name, base_temp)
            }
        }
    }
}

impl fmt::Display for IrType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            IrType::Int => write!(f, "int"),
            IrType::Long => write!(f, "long"),
            IrType::Bool => write!(f, "bool"),
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Label(label) => write!(f, "{}:", label),
            Statement::Move { dest, source } => write!(f, "{} = {}", dest, source),
            Statement::BinOp { dest, left, op, right } => {
                write!(f, "{} = {} {} {}", dest, left, op, right)
            }
            Statement::UnOp { dest, op, operand } => {
                write!(f, "{} = {} {}", dest, op, operand)
            }
            Statement::Cast { dest, from_type, to_type, operand } => {
                write!(f, "{} = {}({}: {})", dest, to_type, operand, from_type)
            }
            Statement::ArrayLoad { dest, array_base, index, elem_type } => {
                write!(f, "{} = {}[{}] ({})", dest, array_base, index, elem_type)
            }
            Statement::ArrayStore { array_base, index, source, elem_type } => {
                write!(f, "{}[{}] = {} ({})", array_base, index, source, elem_type)
            }
            Statement::Jump { target } => write!(f, "goto {}", target),
            Statement::CondJump { condition, relop, target } => {
                write!(f, "if {} {} 0 goto {}", condition, relop, target)
            }
            Statement::Call { dest, method, args } => {
                if let Some(d) = dest {
                    write!(f, "{} = ", d)?;
                }
                write!(f, "call {}(", method)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            Statement::CallExtern { dest, method, args } => {
                write!(f, "{} = call_extern {}(", dest, method)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            Statement::Return { value } => {
                if let Some(v) = value {
                    write!(f, "return {}", v)
                } else {
                    write!(f, "return")
                }
            }
            Statement::Nop => write!(f, "nop"),
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operand::IntConst(i) => write!(f, "{}", i),
            Operand::LongConst(l) => write!(f, "{}L", l),
            Operand::BoolConst(b) => write!(f, "{}", if *b { "true" } else { "false" }),
            Operand::Temp(t) => write!(f, "{}", t),
            Operand::Global(g) => write!(f, "@{}", g),
        }
    }
}

impl fmt::Display for ExternArg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExternArg::Operand(op) => write!(f, "{}", op),
            ExternArg::StringLiteral(s) => write!(f, "\"{}\"", s),
        }
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Mod => write!(f, "%"),
            BinOp::Lt => write!(f, "<"),
            BinOp::Gt => write!(f, ">"),
            BinOp::Le => write!(f, "<="),
            BinOp::Ge => write!(f, ">="),
            BinOp::Eq => write!(f, "=="),
            BinOp::Ne => write!(f, "!="),
            BinOp::And => write!(f, "&&"),
            BinOp::Or => write!(f, "||"),
        }
    }
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnOp::Neg => write!(f, "-"),
            UnOp::Not => write!(f, "!"),
        }
    }
}

impl fmt::Display for RelOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RelOp::Eq => write!(f, "=="),
            RelOp::Ne => write!(f, "!="),
            RelOp::Lt => write!(f, "<"),
            RelOp::Le => write!(f, "<="),
            RelOp::Gt => write!(f, ">"),
            RelOp::Ge => write!(f, ">="),
        }
    }
}