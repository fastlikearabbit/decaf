use std::collections::{HashMap, HashSet};
use crate::parser::ast::Type;
use crate::semantic_analyzer::analyzer::{SemanticError, SemanticErrorKind};

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolType {
    Variable(Type),
    Array { element_type: Type, size: i32 },
    Method { return_type: Option<Type>, param_types: Vec<Type> },
    Import,
}

#[derive(Debug, Clone)]
pub struct Scope {
    symbols: HashMap<String, SymbolType>,
    parent: Option<usize>, // index into the scope stack
}

impl Scope {
    pub fn new(parent: Option<usize>) -> Self {
        Scope {
            symbols: HashMap::new(),
            parent,
        }
    }

    pub fn declare(&mut self, name: String, symbol_type: SymbolType) -> Result<(), SemanticError> {
        if self.symbols.contains_key(&name) {
            return Err(SemanticError::new(SemanticErrorKind::DuplicateIdentifier {
                name,
                scope: "current".to_string(),
            }));
        }
        self.symbols.insert(name, symbol_type);
        Ok(())
    }

    pub fn lookup(&self, name: &str) -> Option<&SymbolType> {
        self.symbols.get(name)
    }
}

pub struct SymbolTable {
    scopes: Vec<Scope>,
    current_scope: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        let global_scope = Scope::new(None);
        SymbolTable {
            scopes: vec![global_scope],
            current_scope: 0,
        }
    }

    pub fn declare(&mut self, name: String, symbol_type: SymbolType) -> Result<(), SemanticError> {
        self.scopes[self.current_scope].declare(name, symbol_type)
    }

    pub fn lookup(&self, name: &str) -> Option<&SymbolType> {
        let mut scope_idx = Some(self.current_scope);

        while let Some(idx) = scope_idx {
            if let Some(symbol) = self.scopes[idx].lookup(name) {
                return Some(symbol);
            }
            scope_idx = self.scopes[idx].parent;
        }

        None
    }

    pub fn exists_in_current_scope(&self, name: &str) -> bool {
        self.scopes[self.current_scope].lookup(name).is_some()
    }

    pub fn enter_scope(&mut self) {
        let parent = self.current_scope;
        let new_scope = Scope::new(Some(parent));
        self.scopes.push(new_scope);
        self.current_scope = self.scopes.len() - 1;
    }

    pub fn exit_scope(&mut self) {
        if let Some(parent) = self.scopes[self.current_scope].parent {
            self.current_scope = parent;
        }
    }

    pub fn current_scope_identifiers(&self) -> HashSet<String> {
        self.scopes[self.current_scope]
            .symbols
            .keys()
            .cloned()
            .collect()
    }
}