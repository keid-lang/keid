use super::{ast::*, *};
use crate::{
    common::{CompilerError, Result},
    compiler_error_loc,
};

pub enum ConvertResult {
    Ok(KeidModuleNode),
    Err(Vec<CompilerError>),
}

struct AstConverter<'a> {
    module_id: usize,
    lookup_items: &'a Vec<LookupItem>,
}

static mut TYPE_ID_NONCE: usize = 0;

pub enum StaticExpr {
    Integer(i64),
    String(String),
    Bool(bool),
}

impl StaticExpr {
    pub fn parse(expr: &Token<Expr>) -> Result<StaticExpr> {
        Ok(match &expr.token {
            Expr::BoolLit(val) => StaticExpr::Bool(*val),
            Expr::SignedIntLit(val) => StaticExpr::Integer(*val),
            Expr::StringLit(val) => StaticExpr::String(val.clone()),
            _ => return Err(compiler_error_loc!(&expr.loc, "Invalid dynamic expression (must be a static literal)")),
        })
    }
}

#[derive(Debug, Clone)]
enum DeclParent {
    Class {
        associated_type_names: Vec<String>,
        name: String,
        generic_defs: Vec<GenericDefNode>,
    },
    InterfaceImpl {
        associated_types: Vec<AssociatedTypeNode>,
        generic_defs: Vec<GenericDefNode>,
        interface: GenericIdentifier,
        class: GenericIdentifier,
    },
}

impl<'a> AstConverter<'a> {
    fn lookup_classlike(&self, resolved_name: &str, same_namespace: &str) -> Option<String> {
        if self.lookup_items.iter().any(|item| item.name == resolved_name && !matches!(item.ty, LookupItemType::Typedef(_))) {
            Some(resolved_name.to_owned())
        } else if self.lookup_items.iter().any(|item| item.name == same_namespace && !matches!(item.ty, LookupItemType::Typedef(_))) {
            Some(same_namespace.to_owned())
        } else {
            None
        }
    }

    fn lookup_typedef(&self, resolved_name: &str, same_namespace: &str) -> Option<ComplexType> {
        match self.lookup_items.iter().find(|item| item.name == resolved_name || item.name == same_namespace) {
            Some(item) => match &item.ty {
                LookupItemType::Typedef(ty) => Some(ty.clone()),
                _ => None,
            },
            None => None,
        }
    }

    fn get_all_type_names(&self) -> Vec<&String> {
        self.lookup_items.iter().map(|item| &item.name).collect()
    }

    fn resolve_type(&self, ns: &str, original_type: &Qualifier, dst: &KeidModuleNode) -> Result<String> {
        let full_name = original_type.to_string();

        if full_name == "This" {
            return Ok(full_name);
        }

        let import_map = crate::func::utils::get_import_map_with(&dst.imports, self.get_all_type_names());
        let resolved_name = import_map.get(&full_name).unwrap_or(&full_name).clone();
        let same_namespace = format!("{}::{}", ns, full_name);

        if let Some(class_name) = self.lookup_classlike(&resolved_name, &same_namespace) {
            Ok(class_name)
        } else {
            Err(compiler_error_loc!(&original_type.get_location(), "[ER5] Could not resolve type `{}`", resolved_name))
        }
    }

    fn get_type(
        &self,
        original_type: QualifiedType,
        parent: Option<&DeclParent>,
        dst: &KeidModuleNode,
        namespace_name: String,
    ) -> Result<ComplexType> {
        if original_type.complex.to_string() == "This" {
            return Ok(original_type.complex);
        }

        let mut full_name = original_type.complex.get_root_type().to_string();

        // remove generic information from the name
        // disgusting hack that "just works"
        // lol
        if let Some(pos) = full_name.chars().position(|ch| ch == '<') {
            full_name = full_name[0..pos].to_owned();
        }

        if let Some(parent) = parent {
            match parent {
                DeclParent::Class {
                    generic_defs,
                    ..
                }
                | DeclParent::InterfaceImpl {
                    generic_defs,
                    ..
                } => {
                    if generic_defs.iter().any(|g| g.name == full_name) {
                        return Ok(original_type.complex);
                    }
                }
            }
            match parent {
                DeclParent::Class {
                    associated_type_names,
                    ..
                } => {
                    if associated_type_names.iter().any(|t| t == &full_name) {
                        return Ok(original_type.complex);
                    }
                }
                DeclParent::InterfaceImpl {
                    associated_types,
                    ..
                } => {
                    if let Some(assoc) = associated_types.iter().find(|t| t.name == full_name) {
                        return Ok(assoc.ty.clone());
                    }
                }
            }
        }

        let original_complex = original_type.complex.clone();
        let concrete_type = match original_complex.get_root_type() {
            BasicType::Function(ft) => {
                let resolved_type = BasicType::Function(FunctionType {
                    params: ft
                        .params
                        .iter()
                        .map(|param| {
                            self.get_type(
                                QualifiedType {
                                    complex: param.clone(),
                                    loc: original_type.loc.clone(),
                                },
                                parent,
                                dst,
                                namespace_name.clone(),
                            )
                        })
                        .collect::<Result<Vec<ComplexType>>>()?,
                    return_type: Box::new(self.get_type(
                        QualifiedType {
                            complex: *ft.return_type,
                            loc: original_type.loc.clone(),
                        },
                        parent,
                        dst,
                        namespace_name.clone(),
                    )?),
                    varargs: ft.varargs,
                })
                .to_complex();

                original_complex.replace_root(resolved_type)
            }
            BasicType::Object(ident) => {
                let import_map = crate::func::utils::get_import_map_with(&dst.imports, self.get_all_type_names());
                let resolved_type = {
                    let resolved_name = import_map.get(&full_name).unwrap_or(&full_name).clone();
                    let same_namespace = format!("{}::{}", namespace_name, full_name);
                    let generic_args = ident
                        .generic_args
                        .iter()
                        .map(|arg| {
                            self.get_type(
                                QualifiedType {
                                    complex: arg.clone(),
                                    loc: original_type.loc.clone(),
                                },
                                parent,
                                dst,
                                namespace_name.clone(),
                            )
                        })
                        .collect::<Result<Vec<ComplexType>>>()?;

                    if let Some(class_name) = self.lookup_classlike(&resolved_name, &same_namespace) {
                        BasicType::Object(GenericIdentifier::from_name_with_args(&class_name, &generic_args)).to_complex()
                    } else if let Some(typedef) = self.lookup_typedef(&resolved_name, &same_namespace) {
                        typedef
                    } else {
                        return Err(compiler_error_loc!(&original_type.loc, "[ER6] Could not resolve type `{}`", resolved_name));
                    }
                };

                original_complex.replace_root(resolved_type)
            }
            _ => original_complex,
        };

        Ok(concrete_type)
    }

    fn parse_accessor_decl(
        &self,
        ns: &str,
        acc: AccessorDecl,
        class: Option<&DeclParent>,
        dst: &mut KeidModuleNode,
    ) -> Result<AccessorNode> {
        let value_type = QualifiedType {
            loc: acc.value_type.loc.clone(),
            complex: self.get_type(acc.value_type.clone(), class, dst, ns.to_owned())?,
        };

        let function_id = self.parse_func_decl(
            ns,
            FunctionDecl {
                attributes: Vec::new(),
                function_type: FunctionContextType::Instance,
                modifiers: acc.modifiers,
                name: vec![Token {
                    loc: acc.name.loc,
                    token: Identifier(format!("__get_{}", acc.name.token.0)),
                }],
                generics: None,
                return_type: match &acc.accessor_type {
                    AccessorType::Getter => Some(value_type.clone()),
                    AccessorType::Setter(_) => None,
                },
                params: match acc.accessor_type {
                    AccessorType::Getter => Vec::new(),
                    AccessorType::Setter(ref name) => vec![NamedParameter {
                        name: name.clone(),
                        param_type: value_type.clone(),
                    }],
                },
                body: acc.body,
                varargs: Varargs::None,
            },
            class,
            dst,
        )?;

        Ok(AccessorNode {
            name: acc.name.token.0,
            value_type: value_type.complex,
            accessor_type: match acc.accessor_type {
                AccessorType::Getter => AccessorNodeType::Getter,
                AccessorType::Setter(tkn) => AccessorNodeType::Setter(tkn.token.0),
            },
            function_id,
        })
    }

    fn parse_func_decl(&self, ns: &str, func: FunctionDecl, class: Option<&DeclParent>, dst: &mut KeidModuleNode) -> Result<usize> {
        let function_type = if class.is_some() && !func.modifiers.contains(&FunctionModifier::Static) {
            FunctionContextType::Instance
        } else {
            FunctionContextType::Static
        };
        let base_name = {
            let base_name = Qualifier(func.name.clone()).to_string();
            if let Some(class) = class {
                match class {
                    DeclParent::InterfaceImpl {
                        class,
                        interface,
                        ..
                    } => {
                        format!("{}::{}#__impl#{}", interface.name, base_name, class.to_string())
                    }
                    DeclParent::Class {
                        name,
                        ..
                    } => format!("{}::{}", name, base_name),
                }
            } else {
                base_name
            }
        };
        let external_name = 'external_name: {
            if func.modifiers.contains(&FunctionModifier::Extern) {
                for attribute in func.attributes {
                    let attribute_type = self.resolve_type(ns, &attribute.attribute_type, dst)?;
                    if attribute_type == "core::runtime::ExternalFunction" {
                        let static_expr = StaticExpr::parse(&attribute.params[0])?;
                        match static_expr {
                            StaticExpr::String(val) => break 'external_name val,
                            _ => panic!(),
                        }
                    } else {
                        todo!("parsing attributes")
                    }
                }
                let external_name = base_name.split("::").last().unwrap().to_owned();
                match external_name.as_str() {
                    "main" => "keid.main()".to_string(),
                    _ => external_name,
                }
            } else {
                // TODO: name mangling
                base_name.to_string()
            }
        };
        let mut func_generic_defs: Vec<GenericDefNode> =
            func.generics.unwrap_or_default().into_iter().map(|def| GenericDefNode::from_ast(&def)).collect();
        let mut this_node = None;

        if function_type == FunctionContextType::Instance && let Some(class) = class {
            match class {
                DeclParent::Class { generic_defs, .. } | DeclParent::InterfaceImpl { generic_defs, .. } => {
                    let additional_defs: Vec<GenericDefNode> = generic_defs
                        .clone()
                        .into_iter()
                        .filter(|def| !func_generic_defs.iter().map(|d| &d.name).any(|x| x == &def.name))
                        .collect();
                    func_generic_defs.extend(additional_defs.clone());

                    match class {
                        DeclParent::Class { name, .. } => {
                            this_node = Some(
                                BasicType::Object(GenericIdentifier::from_name_with_args(name, &additional_defs
                                .iter()
                                .map(|def| BasicType::Object(GenericIdentifier::from_name(&def.name)).to_complex())
                                .collect::<Vec<_>>()))
                                .to_complex()
                            );
                        },
                        DeclParent::InterfaceImpl { class, .. } => {
                            this_node = Some(BasicType::Object(class.clone()).to_complex());
                        },
                    }
                }
            }
        }

        let parent = match class.cloned() {
            Some(DeclParent::Class {
                name,
                associated_type_names,
                ..
            }) => DeclParent::Class {
                name,
                associated_type_names,
                generic_defs: func_generic_defs.clone(),
            },
            Some(DeclParent::InterfaceImpl {
                associated_types,
                interface,
                class,
                ..
            }) => DeclParent::InterfaceImpl {
                associated_types,
                generic_defs: func_generic_defs.clone(),
                interface,
                class,
            },
            None => DeclParent::Class {
                name: String::new(),
                associated_type_names: Vec::new(),
                generic_defs: func_generic_defs.clone(),
            },
        };

        let mut params: Vec<ParameterNode> = func
            .params
            .into_iter()
            .map(|param| {
                Ok(ParameterNode {
                    name: param.name.token.0,
                    ty: self.get_type(param.param_type, Some(&parent), dst, ns.to_owned())?,
                })
            })
            .collect::<Result<_>>()?;
        if let Some(this_node) = this_node {
            params.insert(
                0,
                ParameterNode {
                    name: "this".to_owned(),
                    ty: this_node,
                },
            );
        }

        let return_type =
            func.return_type.map(|ty| self.get_type(ty, Some(&parent), dst, ns.to_owned())).unwrap_or(Ok(BasicType::Void.to_complex()))?;

        let id = dst.functions.len();
        dst.functions.push(FunctionNode {
            module_id: self.module_id,
            id,
            function_type,
            base_name,
            namespace_name: ns.to_owned(),
            modifiers: func.modifiers,
            external_name,
            generic_defs: func_generic_defs,
            params,
            return_type,
            body: func.body,
            varargs: func.varargs,
        });

        Ok(id)
    }

    fn parse_field_decl(&self, field: Let, parent: Option<&DeclParent>, dst: &KeidModuleNode, ns: String) -> Result<FieldNode> {
        let name = match parent {
            Some(_) => field.name.token.0.clone(),
            None => format!("{}::{}", ns, field.name.token.0),
        };
        Ok(FieldNode {
            external_name: if field.is_extern {
                field.name.token.0
            } else {
                name.clone()
            },
            is_const: field.is_const,
            is_extern: field.is_extern,
            name,
            ty: self.get_type(
                match field.var_type {
                    Some(ty) => ty,
                    None => return Err(compiler_error_loc!(&field.name.loc, "Expecting field type")),
                },
                parent,
                dst,
                ns,
            )?,
            initial_value: field.initial_value,
        })
    }

    fn parse_typedef_decl(&self, typedef: TypedefDecl) -> TypedefNode {
        TypedefNode {
            base_name: Qualifier(typedef.name).to_string(),
            generic_defs: Vec::new(),
            target_type: typedef.target_type.complex,
        }
    }

    fn parse_interface_impl(&self, ns: &str, interface_impl: InterfaceImpl, dst: &mut KeidModuleNode) -> Result<()> {
        let generic_defs: Vec<GenericDefNode> = interface_impl
            .generics
            .map(|decls| decls.into_iter().map(|decl| GenericDefNode::from_ast(&decl)).collect())
            .unwrap_or_default();
        let decl_parent = DeclParent::Class {
            name: String::new(),
            generic_defs: generic_defs.clone(),
            associated_type_names: interface_impl.associated_types.iter().map(|assoc| assoc.name.token.0.clone()).collect(),
        };

        let interface_generics = interface_impl
            .interface_generic_args
            .map(|decl| {
                decl.args.into_iter().map(|part| self.get_type(part, Some(&decl_parent), dst, ns.to_owned())).collect::<Result<Vec<_>>>()
            })
            .unwrap_or_else(|| Ok(Vec::new()))?;
        let target_generics = interface_impl
            .target_generic_args
            .map(|decl| {
                decl.args.into_iter().map(|part| self.get_type(part, Some(&decl_parent), dst, ns.to_owned())).collect::<Result<Vec<_>>>()
            })
            .unwrap_or_else(|| Ok(Vec::new()))?;

        let associated_types: Vec<AssociatedTypeNode> = interface_impl
            .associated_types
            .into_iter()
            .map(|assoc| {
                Ok(AssociatedTypeNode {
                    name: assoc.name.token.0.clone(),
                    ty: self.get_type(assoc.ty, Some(&decl_parent), dst, ns.to_owned())?,
                })
            })
            .collect::<Result<Vec<_>>>()?;

        let decl_parent = DeclParent::InterfaceImpl {
            generic_defs: generic_defs.clone(),
            class: GenericIdentifier::from_name_with_args(&self.resolve_type(ns, &interface_impl.target_name, dst)?, &target_generics),
            interface: GenericIdentifier::from_name_with_args(
                &self.resolve_type(ns, &interface_impl.interface_name, dst)?,
                &interface_generics,
            ),
            associated_types: associated_types.clone(),
        };

        let start = dst.functions.len();
        let len = interface_impl.functions.len() + interface_impl.accessors.len();
        for function in interface_impl.functions {
            self.parse_func_decl(ns, function, Some(&decl_parent), dst)?;
        }

        let accessors = interface_impl
            .accessors
            .into_iter()
            .map(|acc| self.parse_accessor_decl(ns, acc, Some(&decl_parent), dst))
            .collect::<Result<Vec<_>>>()?;

        let interface_name = match self
            .get_type(QualifiedType::from_qualifier(&interface_impl.interface_name), Some(&decl_parent), dst, ns.to_owned())?
            .get_root_type()
        {
            BasicType::Object(ident) => ident.name,
            _ => unreachable!(),
        };

        let target_name = match self
            .get_type(QualifiedType::from_qualifier(&interface_impl.target_name), Some(&decl_parent), dst, ns.to_owned())?
            .get_root_type()
        {
            BasicType::Object(ident) => ident.name,
            _ => unreachable!(),
        };

        let interface_id = match self.lookup_items.iter().find(|item| item.name == interface_name) {
            Some(interface) => interface.id,
            None => {
                return Err(compiler_error_loc!(
                    &interface_impl.interface_name.get_location(),
                    "Could not resolve interface type `{}`",
                    interface_name
                ))
            }
        };

        dst.interface_impls.push(InterfaceImplNode {
            module_id: self.module_id,
            associated_types,
            interface_id,
            interface_name,
            interface_generics,
            target_name,
            target_generics,
            functions: (start..start + len).collect(),
            accessors,
            generic_defs,
        });

        Ok(())
    }

    fn parse_enum_decl(&self, ns: &str, enm: EnumDecl, dst: &mut KeidModuleNode) -> Result<()> {
        let base_name = Qualifier(enm.name).to_string();
        let generic_defs: Vec<GenericDefNode> =
            enm.generics.map(|decls| decls.into_iter().map(|decl| GenericDefNode::from_ast(&decl)).collect()).unwrap_or_default();
        let decl_parent = DeclParent::Class {
            name: base_name.clone(),
            generic_defs: generic_defs.clone(),
            associated_type_names: Vec::new(),
        };

        let mut elements = Vec::new();
        for element_decl in enm.elements {
            elements.push(EnumElementNode {
                name: element_decl.name.token.0,
                data: element_decl.data.map_or(Ok(None), |decl| {
                    decl.fields
                        .into_iter()
                        .map(|field| {
                            Ok(AnonymousStructField {
                                name: field.name.token.0,
                                ty: self.get_type(
                                    field.ty.ok_or(compiler_error_loc!(&field.name.loc, "Enum associated data member must have a type"))?,
                                    Some(&decl_parent),
                                    dst,
                                    ns.to_owned(),
                                )?,
                            })
                        })
                        .collect::<Result<_>>()
                        .map(Some)
                })?,
            });
        }

        dst.enums.push(EnumNode {
            module_id: self.module_id,
            id: self
                .lookup_items
                .iter()
                .find_map(|class| {
                    if class.name == base_name {
                        Some(class.id)
                    } else {
                        None
                    }
                })
                .unwrap(),
            base_name,
            generic_defs,
            elements,
        });

        Ok(())
    }

    fn parse_class_decl(&self, ns: &str, class: ClassDecl, dst: &mut KeidModuleNode) -> Result<()> {
        let base_name = Qualifier(class.name).to_string();
        let generic_defs: Vec<GenericDefNode> =
            class.generics.map(|decls| decls.into_iter().map(|decl| GenericDefNode::from_ast(&decl)).collect()).unwrap_or_default();
        let decl_parent = DeclParent::Class {
            name: base_name.clone(),
            generic_defs: generic_defs.clone(),
            associated_type_names: class.associated_type_names.into_iter().map(|name| name.token.0).collect(),
        };

        let superclass = if base_name == "core::object::Object" {
            None // if we're parsing the definition for the Object class, then the superclass is None
        } else {
            let interface_generics = class
                .superclass_generic_args
                .map(|decl| {
                    decl.args
                        .into_iter()
                        .map(|part| self.get_type(part, Some(&decl_parent), dst, ns.to_owned()))
                        .collect::<Result<Vec<_>>>()
                })
                .unwrap_or_else(|| Ok(Vec::new()))?;
            Some(match class.superclass_name {
                Some(superclass_name) => GenericIdentifier::from_name_with_args(&superclass_name.to_string(), &interface_generics),
                None => GenericIdentifier::from_name("core::object::Object"), // if no superclass was specified, the default is the Object class
            })
        };

        let fields = class
            .fields
            .into_iter()
            .map(|decl| self.parse_field_decl(decl, Some(&decl_parent), dst, ns.to_owned()))
            .collect::<Result<_>>()?;

        let start = dst.functions.len();
        let mut length = class.methods.len();

        if class.ty == ClassType::Class {
            let this_ty = BasicType::Object(GenericIdentifier::from_name_with_args(
                &base_name,
                &generic_defs.iter().map(|def| BasicType::Object(GenericIdentifier::from_name(&def.name)).to_complex()).collect::<Vec<_>>(),
            ))
            .to_complex();

            length += 1;
            dst.functions.push(FunctionNode {
                module_id: self.module_id,
                id: dst.functions.len(),
                modifiers: vec![FunctionModifier::Internal],
                base_name: format!("{}::keid.destructor", base_name),
                namespace_name: ns.to_owned(),
                function_type: FunctionContextType::Instance,
                external_name: format!("{}::keid.destructor()", base_name),
                generic_defs: generic_defs.clone(),
                params: vec![ParameterNode {
                    name: "this".to_owned(),
                    ty: this_ty,
                }],
                return_type: BasicType::Void.to_complex(),
                body: None,
                varargs: Varargs::None,
            });
        }

        for method in class.methods {
            self.parse_func_decl(ns, method, Some(&decl_parent), dst)?;
        }
        let accessors =
            class.accessors.into_iter().map(|acc| self.parse_accessor_decl(ns, acc, Some(&decl_parent), dst)).collect::<Result<_>>()?;
        dst.classes.push(ClassNode {
            superclass,
            class_type: class.ty,
            module_id: self.module_id,
            id: self
                .lookup_items
                .iter()
                .find_map(|class| {
                    if class.name == base_name {
                        Some(class.id)
                    } else {
                        None
                    }
                })
                .unwrap(),
            base_name,
            generic_defs,
            fields,
            functions: (start..start + length).collect(),
            accessors,
            constructor: class.constructor,
            destructor: class.destructor,
        });

        Ok(())
    }

    fn convert(self, program: KeidFile) -> ConvertResult {
        let mut root_node = KeidModuleNode {
            imports: Vec::new(),
            classes: Vec::new(),
            functions: Vec::new(),
            typedefs: Vec::new(),
            interface_impls: Vec::new(),
            fields: Vec::new(),
            enums: Vec::new(),
            namespace: program.namespace.to_string(),
        };

        // get all imports first
        for import in program.imports {
            root_node.imports.push(ImportNode {
                module: import.to_string(),
            });
        }

        let mut errors = Vec::new();
        let mut interface_impls = Vec::new();

        let ns_name = program.namespace.to_string();
        for function in program.functions {
            if let Err(e) = self.parse_func_decl(&ns_name, function, None, &mut root_node) {
                errors.push(e);
            }
        }

        for class in program.classes {
            if let Err(e) = self.parse_class_decl(&ns_name, class, &mut root_node) {
                errors.push(e);
            }
        }

        for enm in program.enums {
            if let Err(e) = self.parse_enum_decl(&ns_name, enm, &mut root_node) {
                errors.push(e);
            }
        }

        for typedef in program.typedefs {
            self.parse_typedef_decl(typedef);
        }

        for interface_impl in program.interface_impls {
            interface_impls.push((ns_name.clone(), interface_impl));
        }

        for field in program.fields {
            match self.parse_field_decl(field, None, &mut root_node, program.namespace.to_string()) {
                Ok(field) => root_node.fields.push(field),
                Err(e) => errors.push(e),
            }
        }

        // this happens in a separate step because all interfaces need to be analyzed
        // before the interface implementations can be parsed
        for (ns_name, interface_impl) in interface_impls {
            if let Err(e) = self.parse_interface_impl(&ns_name, interface_impl, &mut root_node) {
                errors.push(e);
            }
        }

        if !errors.is_empty() {
            ConvertResult::Err(errors)
        } else {
            ConvertResult::Ok(root_node)
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LookupItemType {
    Class,
    Typedef(ComplexType),
    Interface,
    Struct,
    Enum,
}

#[derive(Debug)]
pub struct LookupItem {
    pub name: String,
    pub ty: LookupItemType,
    pub id: usize,
}

impl LookupItem {
    pub fn new(name: &str, ty: LookupItemType, id: usize) -> LookupItem {
        LookupItem {
            name: name.to_owned(),
            ty,
            id,
        }
    }
}

pub fn reset() {
    unsafe {
        TYPE_ID_NONCE = 0;
    }
}

fn get_next_id() -> usize {
    unsafe {
        let tmp = TYPE_ID_NONCE;
        TYPE_ID_NONCE += 1;
        tmp
    }
}

pub fn ast_to_type_list(program: &KeidFile) -> Vec<LookupItem> {
    let mut items = Vec::new();
    for class in &program.classes {
        items.push(LookupItem::new(&Qualifier(class.name.clone()).to_string(), LookupItemType::Class, get_next_id()));
    }

    for typedef in &program.typedefs {
        items.push(LookupItem::new(
            &Qualifier(typedef.name.clone()).to_string(),
            LookupItemType::Typedef(typedef.target_type.complex.clone()),
            get_next_id(),
        ));
    }

    for attribute in &program.attributes {
        items.push(LookupItem::new(&Qualifier(attribute.name.clone()).to_string(), LookupItemType::Class, get_next_id()));
    }

    for en in &program.enums {
        items.push(LookupItem::new(&Qualifier(en.name.clone()).to_string(), LookupItemType::Enum, get_next_id()));
    }

    items
}

pub fn ast_to_keid_module_node(program: KeidFile, module_id: usize, class_list: &Vec<LookupItem>) -> ConvertResult {
    AstConverter {
        module_id,
        lookup_items: class_list,
    }
    .convert(program)
}
