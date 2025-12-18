use crate::parser::ast::*;
use crate::semantic_analyzer::symbol_table::*;

use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq)]
pub enum SemanticErrorKind {
    DuplicateIdentifier {
        name: String,
        scope: String,
    },
    UndeclaredIdentifier {
        name: String,
    },
    NoMainMethod,
    MainMethodInvalidSignature,
}

#[derive(Debug, Clone)]
pub struct SemanticError {
    pub kind: SemanticErrorKind,
    pub message: String,
}

impl SemanticError {
    pub fn new(kind: SemanticErrorKind) -> Self {
        let message = match &kind {
            SemanticErrorKind::DuplicateIdentifier { name, scope } => {
                format!("Identifier '{}' is declared more than once in {} scope", name, scope)
            }
            SemanticErrorKind::UndeclaredIdentifier { name } => {
                format!("Identifier '{}' is used before it is declared", name)
            }
            SemanticErrorKind::NoMainMethod => {
                "Program must contain a 'main' method with type void and no parameters".to_string()
            }
            SemanticErrorKind::MainMethodInvalidSignature => {
                "'main' method must have type void and take no parameters".to_string()
            }
        };

        SemanticError { kind, message }
    }
}

pub struct SemanticAnalyzer {
    symbol_table: SymbolTable,
    errors: Vec<SemanticError>,
    declared_methods: HashSet<String>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        SemanticAnalyzer {
            symbol_table: SymbolTable::new(),
            errors: Vec::new(),
            declared_methods: HashSet::new(),
        }
    }

    pub fn analyze(&mut self, program: &Program) -> Result<(), Vec<SemanticError>> {
        self.errors.clear();
        self.declared_methods.clear();

        self.analyze_imports_and_fields(program);
        self.check_main_method(program);
        self.analyze_methods_sequentially(program);

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    fn analyze_imports_and_fields(&mut self, program: &Program) {
        for import in &program.imports {
            if let Err(e) = self.symbol_table.declare(
                import.name.0.clone(),
                SymbolType::Import,
            ) {
                self.errors.push(e);
            }
        }

        for field in &program.fields {
            self.check_field_decl(field);
        }
    }

    fn analyze_methods_sequentially(&mut self, program: &Program) {
        for method in &program.methods {
            let param_types = method.parameters.iter()
                .map(|p| p.param_type.clone())
                .collect();

            if let Err(e) = self.symbol_table.declare(
                method.name.0.clone(),
                SymbolType::Method {
                    return_type: method.return_type.clone(),
                    param_types,
                },
            ) {
                self.errors.push(e);
            }

            self.declared_methods.insert(method.name.0.clone());
            self.analyze_method(method);
        }
    }

    fn check_field_decl(&mut self, field: &FieldDecl) {
        match field {
            FieldDecl::ScalarField { field_type, names } => {
                for name in names {
                    if let Err(e) = self.symbol_table.declare(
                        name.0.clone(),
                        SymbolType::Variable(field_type.clone()),
                    ) {
                        self.errors.push(e);
                    }
                }
            }
            FieldDecl::ArrayField { field_type, arrays } => {
                for array in arrays {
                    if let Err(e) = self.symbol_table.declare(
                        array.name.0.clone(),
                        SymbolType::Array {
                            element_type: field_type.clone(),
                            size: array.size,
                        },
                    ) {
                        self.errors.push(e);
                    }
                }
            }
        }
    }

    fn check_main_method(&mut self, program: &Program) {
        let main_method = program.methods.iter().find(|m| m.name.0 == "main");

        match main_method {
            None => {
                self.errors.push(SemanticError::new(SemanticErrorKind::NoMainMethod));
            }
            Some(method) => {
                if method.return_type.is_some() || !method.parameters.is_empty() {
                    self.errors.push(SemanticError::new(
                        SemanticErrorKind::MainMethodInvalidSignature
                    ));
                }
            }
        }
    }

    fn analyze_method(&mut self, method: &MethodDecl) {
        self.symbol_table.enter_scope();

        let param_names: HashSet<String> = method.parameters.iter()
            .map(|p| p.name.0.clone())
            .collect();

        for param in &method.parameters {
            if let Err(e) = self.symbol_table.declare(
                param.name.0.clone(),
                SymbolType::Variable(param.param_type.clone()),
            ) {
                self.errors.push(e);
            }
        }

        self.analyze_method_body(&method.body, &param_names);

        self.symbol_table.exit_scope();
    }

    fn analyze_method_body(&mut self, block: &Block, param_names: &HashSet<String>) {
        self.symbol_table.enter_scope();

        for field in &block.field_decls {
            match field {
                FieldDecl::ScalarField { field_type, names } => {
                    for name in names {
                        if param_names.contains(&name.0) {
                            self.errors.push(SemanticError::new(
                                SemanticErrorKind::DuplicateIdentifier {
                                    name: name.0.clone(),
                                    scope: "method".to_string(),
                                }
                            ));
                        }
                        if let Err(e) = self.symbol_table.declare(
                            name.0.clone(),
                            SymbolType::Variable(field_type.clone()),
                        ) {
                            self.errors.push(e);
                        }
                    }
                }
                FieldDecl::ArrayField { field_type, arrays } => {
                    for array in arrays {
                        if param_names.contains(&array.name.0) {
                            self.errors.push(SemanticError::new(
                                SemanticErrorKind::DuplicateIdentifier {
                                    name: array.name.0.clone(),
                                    scope: "method".to_string(),
                                }
                            ));
                        }
                        if let Err(e) = self.symbol_table.declare(
                            array.name.0.clone(),
                            SymbolType::Array {
                                element_type: field_type.clone(),
                                size: array.size,
                            },
                        ) {
                            self.errors.push(e);
                        }
                    }
                }
            }
        }

        for stmt in &block.statements {
            self.analyze_statement(stmt);
        }

        self.symbol_table.exit_scope();
    }

    fn analyze_block(&mut self, block: &Block) {
        self.symbol_table.enter_scope();

        for field in &block.field_decls {
            self.check_field_decl(field);
        }

        for stmt in &block.statements {
            self.analyze_statement(stmt);
        }

        self.symbol_table.exit_scope();
    }

    fn analyze_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Assignment { location, expr } => {
                self.check_location(location);
                self.analyze_assign_expr(expr);
            }
            Statement::MethodCall(call) => {
                self.check_method_call(call);
            }
            Statement::If { condition, then_block, else_block } => {
                self.check_expr(condition);
                self.analyze_block(then_block);
                if let Some(else_blk) = else_block {
                    self.analyze_block(else_blk);
                }
            }
            Statement::For { init_id, init_expr, condition, update, body } => {
                self.check_identifier(&init_id.0);
                self.check_expr(init_expr);
                self.check_expr(condition);
                self.check_location(&update.location);
                self.analyze_assign_expr(&update.expr);
                self.analyze_block(body);
            }
            Statement::While { condition, body } => {
                self.check_expr(condition);
                self.analyze_block(body);
            }
            Statement::Return(expr_opt) => {
                if let Some(expr) = expr_opt {
                    self.check_expr(expr);
                }
            }
            Statement::Break | Statement::Continue => {}
        }
    }

    fn analyze_assign_expr(&mut self, assign_expr: &AssignExpr) {
        match assign_expr {
            AssignExpr::Assign { expr, .. } => {
                self.check_expr(expr);
            }
            AssignExpr::Increment(_) => {}
        }
    }

    fn check_location(&mut self, location: &Location) {
        match location {
            Location::Scalar(id) => {
                self.check_identifier(&id.0);
            }
            Location::Array { name, index } => {
                self.check_identifier(&name.0);
                self.check_expr(index);
            }
        }
    }

    fn check_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Location(loc) => {
                self.check_location(loc);
            }
            Expr::MethodCall(call) => {
                self.check_method_call(call);
            }
            Expr::Literal(_) => {}
            Expr::IntCast(e) | Expr::LongCast(e) => {
                self.check_expr(e);
            }
            Expr::Len(id) => {
                self.check_identifier(&id.0);
            }
            Expr::Binary { left, right, .. } => {
                self.check_expr(left);
                self.check_expr(right);
            }
            Expr::Unary { expr, .. } => {
                self.check_expr(expr);
            }
        }
    }

    fn check_method_call(&mut self, call: &MethodCall) {
        match self.symbol_table.lookup(&call.name.0) {
            Some(SymbolType::Import) => {
            }
            Some(SymbolType::Method { .. }) => {
                if !self.declared_methods.contains(&call.name.0) {
                    self.errors.push(SemanticError::new(
                        SemanticErrorKind::UndeclaredIdentifier {
                            name: call.name.0.clone(),
                        }
                    ));
                }
            }
            _ => {
                self.errors.push(SemanticError::new(
                    SemanticErrorKind::UndeclaredIdentifier {
                        name: call.name.0.clone(),
                    }
                ));
            }
        }

        for arg in &call.arguments {
            match arg {
                ExternArg::Expr(e) => self.check_expr(e),
                ExternArg::StringLiteral(_) => {}
            }
        }
    }

    fn check_identifier(&mut self, name: &str) {
        if self.symbol_table.lookup(name).is_none() {
            self.errors.push(SemanticError::new(
                SemanticErrorKind::UndeclaredIdentifier {
                    name: name.to_string(),
                }
            ));
        }
    }

    pub fn get_errors(&self) -> &[SemanticError] {
        &self.errors
    }
}