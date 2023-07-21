use bincode::*;

use crate::tree::{
    ast::{FunctionModifier, FunctionType, Varargs},
    AccessorNodeType, ClassType, FunctionNode, GenericDefNode, KeidModuleNode,
};

use super::{types::ComplexType, GenericIdentifier};

#[derive(Debug, Encode, Decode)]
pub struct KeidPackageData {
    pub types: Vec<TypeDecl>,
    pub typedefs: Vec<TypedefDecl>,
    pub functions: Vec<FunctionDecl>,
    pub interface_impls: Vec<InterfaceImplDecl>,
}

impl KeidPackageData {
    pub fn new(module: &KeidModuleNode) -> KeidPackageData {
        let mut kpkg = KeidPackageData {
            types: Vec::with_capacity(module.classes.len()),
            typedefs: Vec::with_capacity(module.typedefs.len()),
            functions: Vec::new(),
            interface_impls: Vec::with_capacity(module.interface_impls.len()),
        };
        for class in &module.classes {
            let mut ty_decl = TypeDecl {
                kind: match class.class_type {
                    ClassType::Class => TypeDeclKind::Class,
                    ClassType::Struct => TypeDeclKind::Struct,
                    ClassType::Interface => TypeDeclKind::Interface,
                    ClassType::Enum => continue,
                },
                name: class.base_name.clone(),
                generic_defs: class.generic_defs.clone(),
                fields: Vec::with_capacity(class.fields.len()),
                methods: Vec::with_capacity(class.functions.len()),
                accessors: Vec::with_capacity(class.accessors.len()),
            };
            for field in &class.fields {
                ty_decl.fields.push(FieldDecl {
                    name: field.name.clone(),
                    ty: field.ty.clone(),
                });
            }
            for function_id in &class.functions {
                let method = &module.functions[*function_id];
                let func_decl = FunctionDecl::new(method);
                ty_decl.methods.push(func_decl);
            }
            for accessor in &class.accessors {
                ty_decl.accessors.push(AccessorDecl {
                    modifiers: Vec::new(),
                    kind: match accessor.accessor_type {
                        AccessorNodeType::Getter => AccessorKind::Getter,
                        AccessorNodeType::Setter(_) => AccessorKind::Setter,
                    },
                    name: accessor.name.clone(),
                    value_type: accessor.value_type.clone(),
                })
            }
            kpkg.types.push(ty_decl);
        }
        kpkg
    }
}

#[derive(Debug, Encode, Decode)]
pub struct TypedefDecl {
    pub name: String,
    pub ty: ComplexType,
}

#[derive(Debug, Encode, Decode)]
pub enum TypeDeclKind {
    Class,
    Struct,
    Interface,
}

#[derive(Debug, Encode, Decode)]
pub struct TypeDecl {
    pub kind: TypeDeclKind,
    pub name: String,
    pub generic_defs: Vec<GenericDefNode>,
    pub fields: Vec<FieldDecl>,
    pub methods: Vec<FunctionDecl>,
    pub accessors: Vec<AccessorDecl>,
}

#[derive(Debug, Encode, Decode, Clone, PartialEq)]
pub struct FieldDecl {
    pub name: String,
    pub ty: ComplexType,
}

#[derive(Debug, Encode, Decode)]
pub struct AccessorDecl {
    pub modifiers: Vec<FunctionModifier>,
    pub kind: AccessorKind,
    pub name: String,
    pub value_type: ComplexType,
}

#[derive(Debug, Encode, Decode)]
pub struct FunctionDecl {
    pub modifiers: Vec<FunctionModifier>,
    pub kind: FunctionKind,
    pub name: String,
    pub generic_defs: Vec<GenericDefNode>,
    pub params: Vec<ParamDecl>,
    pub return_type: ComplexType,
    pub varargs: Varargs,
}

impl FunctionDecl {
    pub fn new(method: &FunctionNode) -> FunctionDecl {
        let mut decl = FunctionDecl {
            modifiers: method.modifiers.clone(),
            kind: match method.function_type {
                FunctionType::Instance => FunctionKind::Instance,
                FunctionType::Static => FunctionKind::Static,
            },
            name: method.base_name.clone(),
            generic_defs: method.generic_defs.clone(),
            params: Vec::with_capacity(method.params.len()),
            return_type: method.return_type.clone(),
            varargs: method.varargs,
        };
        for param in &method.params {
            decl.params.push(ParamDecl {
                name: param.name.clone(),
                param_type: param.ty.clone(),
            });
        }
        decl
    }
}

#[derive(Debug, Encode, Decode)]
pub struct ParamDecl {
    pub name: String,
    pub param_type: ComplexType,
}

#[derive(Debug, Encode, Decode, PartialEq)]
pub enum AccessorKind {
    Getter,
    Setter,
}

#[derive(Debug, Encode, Decode, PartialEq)]
pub enum FunctionKind {
    Instance,
    Static,
}

#[derive(Debug, Encode, Decode)]
pub struct InterfaceImplDecl {
    pub generic_defs: Vec<GenericDefNode>,
    pub interface: GenericIdentifier,
    pub target: GenericIdentifier,
    pub functions: Vec<FunctionDecl>,
    pub accessors: Vec<AccessorDecl>,
}
