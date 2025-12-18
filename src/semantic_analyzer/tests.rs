use crate::{
    parser::ast::*,
    semantic_analyzer::analyzer::*
};

#[test]
fn test_rule1_duplicate_global_variable() {
    let program = crate::parser::ast::Program {
        imports: vec![],
        fields: vec![
            FieldDecl::ScalarField {
                field_type: Type::Int,
                names: vec![
                    Identifier("x".to_string()),
                    Identifier("y".to_string()),
                ],
            },
            FieldDecl::ScalarField {
                field_type: Type::Bool,
                names: vec![
                    Identifier("x".to_string()), // duplicate
                ],
            },
        ],
        methods: vec![
            MethodDecl {
                return_type: None,
                name: Identifier("main".to_string()),
                parameters: vec![],
                body: Block {
                    field_decls: vec![],
                    statements: vec![],
                },
            },
        ],
    };

    let mut analyzer = SemanticAnalyzer::new();
    let result = analyzer.analyze(&program);

    assert!(result.is_err());
    let errors = result.unwrap_err();
    assert_eq!(errors.len(), 1);
    assert!(matches!(
        errors[0].kind,
        SemanticErrorKind::DuplicateIdentifier { .. }
    ));
}

#[test]
fn test_rule1_duplicate_import() {
    let program = Program {
        imports: vec![
            ImportDecl {
                name: Identifier("printf".to_string()),
            },
            ImportDecl {
                name: Identifier("printf".to_string()), // duplicate!
            },
        ],
        fields: vec![],
        methods: vec![
            MethodDecl {
                return_type: None,
                name: Identifier("main".to_string()),
                parameters: vec![],
                body: Block {
                    field_decls: vec![],
                    statements: vec![],
                },
            },
        ],
    };

    let mut analyzer = SemanticAnalyzer::new();
    let result = analyzer.analyze(&program);

    assert!(result.is_err());
    let errors = result.unwrap_err();
    assert_eq!(errors.len(), 1);
    assert!(matches!(
        errors[0].kind,
        SemanticErrorKind::DuplicateIdentifier { .. }
    ));
}

#[test]
fn test_rule1_duplicate_method_name() {
    let program = Program {
        imports: vec![],
        fields: vec![],
        methods: vec![
            MethodDecl {
                return_type: Some(Type::Int),
                name: Identifier("foo".to_string()),
                parameters: vec![],
                body: Block {
                    field_decls: vec![],
                    statements: vec![Statement::Return(Some(Expr::Literal(Literal::Int(0))))],
                },
            },
            MethodDecl {
                return_type: Some(Type::Int),
                name: Identifier("foo".to_string()), // duplicate!
                parameters: vec![],
                body: Block {
                    field_decls: vec![],
                    statements: vec![Statement::Return(Some(Expr::Literal(Literal::Int(1))))],
                },
            },
            MethodDecl {
                return_type: None,
                name: Identifier("main".to_string()),
                parameters: vec![],
                body: Block {
                    field_decls: vec![],
                    statements: vec![],
                },
            },
        ],
    };

    let mut analyzer = SemanticAnalyzer::new();
    let result = analyzer.analyze(&program);

    assert!(result.is_err());
    let errors = result.unwrap_err();
    assert_eq!(errors.len(), 1);
    assert!(matches!(
        errors[0].kind,
        SemanticErrorKind::DuplicateIdentifier { .. }
    ));
}

#[test]
fn test_rule1_duplicate_method_parameters() {
    let program = Program {
        imports: vec![],
        fields: vec![],
        methods: vec![
            MethodDecl {
                return_type: Some(Type::Int),
                name: Identifier("add".to_string()),
                parameters: vec![
                    Parameter {
                        param_type: Type::Int,
                        name: Identifier("x".to_string()),
                    },
                    Parameter {
                        param_type: Type::Int,
                        name: Identifier("x".to_string()), // duplicate!
                    },
                ],
                body: Block {
                    field_decls: vec![],
                    statements: vec![Statement::Return(Some(Expr::Literal(Literal::Int(0))))],
                },
            },
            MethodDecl {
                return_type: None,
                name: Identifier("main".to_string()),
                parameters: vec![],
                body: Block {
                    field_decls: vec![],
                    statements: vec![],
                },
            },
        ],
    };

    let mut analyzer = SemanticAnalyzer::new();
    let result = analyzer.analyze(&program);

    assert!(result.is_err());
    let errors = result.unwrap_err();
    assert_eq!(errors.len(), 1);
    assert!(matches!(
        errors[0].kind,
        SemanticErrorKind::DuplicateIdentifier { .. }
    ));
}

#[test]
fn test_rule1_duplicate_local_variables() {
    let program = Program {
        imports: vec![],
        fields: vec![],
        methods: vec![
            MethodDecl {
                return_type: None,
                name: Identifier("main".to_string()),
                parameters: vec![],
                body: Block {
                    field_decls: vec![
                        FieldDecl::ScalarField {
                            field_type: Type::Int,
                            names: vec![
                                Identifier("x".to_string()),
                            ],
                        },
                        FieldDecl::ScalarField {
                            field_type: Type::Bool,
                            names: vec![
                                Identifier("x".to_string()), // duplicate in same block!
                            ],
                        },
                    ],
                    statements: vec![],
                },
            },
        ],
    };

    let mut analyzer = SemanticAnalyzer::new();
    let result = analyzer.analyze(&program);

    assert!(result.is_err());
    let errors = result.unwrap_err();
    assert_eq!(errors.len(), 1);
    assert!(matches!(
        errors[0].kind,
        SemanticErrorKind::DuplicateIdentifier { .. }
    ));
}

#[test]
fn test_rule2_use_before_declaration_in_expression() {
    let program = Program {
        imports: vec![],
        fields: vec![],
        methods: vec![
            MethodDecl {
                return_type: None,
                name: Identifier("main".to_string()),
                parameters: vec![],
                body: Block {
                    field_decls: vec![
                        FieldDecl::ScalarField {
                            field_type: Type::Int,
                            names: vec![Identifier("x".to_string())],
                        },
                    ],
                    statements: vec![
                        Statement::Assignment {
                            location: Location::Scalar(Identifier("x".to_string())),
                            expr: AssignExpr::Assign {
                                op: AssignOp::Eq,
                                expr: Expr::Location(Location::Scalar(
                                    Identifier("y".to_string()) // y not declared!
                                )),
                            },
                        },
                    ],
                },
            },
        ],
    };

    let mut analyzer = SemanticAnalyzer::new();
    let result = analyzer.analyze(&program);

    assert!(result.is_err());
    let errors = result.unwrap_err();
    assert_eq!(errors.len(), 1);
    assert!(matches!(
        errors[0].kind,
        SemanticErrorKind::UndeclaredIdentifier { .. }
    ));
}

#[test]
fn test_rule2_method_call_before_declaration() {
    let program = Program {
        imports: vec![],
        fields: vec![],
        methods: vec![
            MethodDecl {
                return_type: None,
                name: Identifier("main".to_string()),
                parameters: vec![],
                body: Block {
                    field_decls: vec![],
                    statements: vec![
                        Statement::MethodCall(MethodCall {
                            name: Identifier("helper".to_string()), // called before declaration
                            arguments: vec![],
                        }),
                    ],
                },
            },
            MethodDecl {
                return_type: None,
                name: Identifier("helper".to_string()),
                parameters: vec![],
                body: Block {
                    field_decls: vec![],
                    statements: vec![],
                },
            },
        ],
    };

    let mut analyzer = SemanticAnalyzer::new();
    let result = analyzer.analyze(&program);

    assert!(result.is_err());
    let errors = result.unwrap_err();
    assert_eq!(errors.len(), 1);
    assert!(matches!(
        errors[0].kind,
        SemanticErrorKind::UndeclaredIdentifier { .. }
    ));
}

#[test]
fn test_valid_program_with_shadowing() {
    // Shadowing is allowed in nested scopes
    let program = Program {
        imports: vec![],
        fields: vec![
            FieldDecl::ScalarField {
                field_type: Type::Int,
                names: vec![Identifier("x".to_string())],
            },
        ],
        methods: vec![
            MethodDecl {
                return_type: None,
                name: Identifier("main".to_string()),
                parameters: vec![],
                body: Block {
                    field_decls: vec![
                        FieldDecl::ScalarField {
                            field_type: Type::Bool,
                            names: vec![Identifier("x".to_string())], // shadows global x
                        },
                    ],
                    statements: vec![],
                },
            },
        ],
    };

    let mut analyzer = SemanticAnalyzer::new();
    let result = analyzer.analyze(&program);
    assert!(result.is_ok());
}

#[test]
fn test_no_main_method() {
    let program = Program {
        imports: vec![],
        fields: vec![],
        methods: vec![
            MethodDecl {
                return_type: Some(Type::Int),
                name: Identifier("helper".to_string()),
                parameters: vec![],
                body: Block {
                    field_decls: vec![],
                    statements: vec![Statement::Return(Some(Expr::Literal(Literal::Int(0))))],
                },
            },
        ],
    };

    let mut analyzer = SemanticAnalyzer::new();
    let result = analyzer.analyze(&program);

    assert!(result.is_err());
    let errors = result.unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        SemanticErrorKind::NoMainMethod
    )));
}

#[test]
fn test_main_method_invalid_signature() {
    let program = Program {
        imports: vec![],
        fields: vec![],
        methods: vec![
            MethodDecl {
                return_type: Some(Type::Int), // should be void
                name: Identifier("main".to_string()),
                parameters: vec![],
                body: Block {
                    field_decls: vec![],
                    statements: vec![Statement::Return(Some(Expr::Literal(Literal::Int(0))))],
                },
            },
        ],
    };

    let mut analyzer = SemanticAnalyzer::new();
    let result = analyzer.analyze(&program);

    assert!(result.is_err());
    let errors = result.unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        SemanticErrorKind::MainMethodInvalidSignature
    )));
}

#[test]
fn test_valid_recursive_method() {
    let program = Program {
        imports: vec![],
        fields: vec![],
        methods: vec![
            MethodDecl {
                return_type: Some(Type::Int),
                name: Identifier("factorial".to_string()),
                parameters: vec![
                    Parameter {
                        param_type: Type::Int,
                        name: Identifier("n".to_string()),
                    },
                ],
                body: Block {
                    field_decls: vec![],
                    statements: vec![
                        Statement::Return(Some(
                            Expr::MethodCall(MethodCall {
                                name: Identifier("factorial".to_string()), // recursive call
                                arguments: vec![
                                    ExternArg::Expr(Expr::Location(Location::Scalar(
                                        Identifier("n".to_string())
                                    ))),
                                ],
                            })
                        )),
                    ],
                },
            },
            MethodDecl {
                return_type: None,
                name: Identifier("main".to_string()),
                parameters: vec![],
                body: Block {
                    field_decls: vec![],
                    statements: vec![],
                },
            },
        ],
    };

    let mut analyzer = SemanticAnalyzer::new();
    let result = analyzer.analyze(&program);

    assert!(result.is_ok());
}

#[test]
fn test_array_access_undeclared() {
    let program = Program {
        imports: vec![],
        fields: vec![],
        methods: vec![
            MethodDecl {
                return_type: None,
                name: Identifier("main".to_string()),
                parameters: vec![],
                body: Block {
                    field_decls: vec![],
                    statements: vec![
                        Statement::Assignment {
                            location: Location::Array {
                                name: Identifier("arr".to_string()), // arr not declared!
                                index: Box::new(Expr::Literal(Literal::Int(0))),
                            },
                            expr: AssignExpr::Assign {
                                op: AssignOp::Eq,
                                expr: Expr::Literal(Literal::Int(42)),
                            },
                        },
                    ],
                },
            },
        ],
    };

    let mut analyzer = SemanticAnalyzer::new();
    let result = analyzer.analyze(&program);

    assert!(result.is_err());
    let errors = result.unwrap_err();
    assert_eq!(errors.len(), 1);
    assert!(matches!(
        errors[0].kind,
        SemanticErrorKind::UndeclaredIdentifier { .. }
    ));
}

#[test]
fn test_main_with_parameters() {
    let program = Program {
        imports: vec![],
        fields: vec![],
        methods: vec![
            MethodDecl {
                return_type: None, // correct: void
                name: Identifier("main".to_string()),
                parameters: vec![
                    Parameter {
                        param_type: Type::Int,
                        name: Identifier("argc".to_string()),
                    },
                ], // wrong: should have no parameters
                body: Block {
                    field_decls: vec![],
                    statements: vec![],
                },
            },
        ],
    };

    let mut analyzer = SemanticAnalyzer::new();
    let result = analyzer.analyze(&program);

    assert!(result.is_err());
    let errors = result.unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        SemanticErrorKind::MainMethodInvalidSignature
    )));
}

#[test]
fn test_methods_after_main_are_legal() {
    let program = Program {
        imports: vec![],
        fields: vec![],
        methods: vec![
            MethodDecl {
                return_type: None,
                name: Identifier("main".to_string()),
                parameters: vec![],
                body: Block {
                    field_decls: vec![],
                    statements: vec![],
                },
            },
            MethodDecl {
                return_type: None,
                name: Identifier("unreachable".to_string()),
                parameters: vec![],
                body: Block {
                    field_decls: vec![],
                    statements: vec![],
                },
            },
        ],
    };

    let mut analyzer = SemanticAnalyzer::new();
    let result = analyzer.analyze(&program);

    assert!(result.is_ok());
}