use std::fmt::Write;

mod parser;
use anyhow::anyhow;
use bincode::{Decode, Encode};
pub use parser::*;

pub mod ast;

use self::ast::*;
use crate::{
    common::{types::*, *},
    compiler::{
        llvm::{OpaqueFunctionType, OpaqueType},
        Compiler,
    },
    func::utils,
};

pub trait GenericNode {
    type Output;

    fn create_impl(&self, type_provider: &TypeProvider, generic_args: &[ComplexType]) -> anyhow::Result<Self::Output>;
}

pub fn extract_type(
    type_provider: &TypeProvider,
    ty: ComplexType,
    generics: &[GenericDefNode],
    values: &[ComplexType],
) -> anyhow::Result<ComplexType> {
    let (value, interfaces) = 'block: {
        match ty.get_root_type() {
            BasicType::Object(ident) => {
                let (pos, interfaces) = match generics.iter().enumerate().find_map(|(pos, g)| {
                    if g.name == ident.name {
                        Some((pos, g.interfaces.clone()))
                    } else {
                        None
                    }
                }) {
                    Some(generic) => generic,
                    _ => break 'block (ty, Vec::new()),
                };
                (ty.replace_root(values[pos].clone()), interfaces)
            }
            _ => (ty, Vec::new()),
        }
    };
    let mapped_root = match value.get_root_type() {
        BasicType::Object(ident) => {
            let extracted_generics: Vec<ComplexType> = ident
                .generic_args
                .into_iter()
                .map(|arg| extract_type(type_provider, arg, generics, values))
                .collect::<anyhow::Result<_>>()?;
            let result = BasicType::Object(GenericIdentifier::from_name_with_args(&ident.name, &extracted_generics)).to_complex();
            for interface in interfaces {
                let interface_type = BasicType::Object(interface).to_complex();
                if !type_provider.is_assignable_to(&result, &interface_type) {
                    return Err(anyhow!(
                        "Type `{}` is not assignable to generic type requirement `{}`",
                        result.to_string(),
                        interface_type.to_string()
                    ));
                }
            }
            result
        }
        root => root.to_complex(),
    };
    Ok(value.replace_root(mapped_root))
}

#[derive(Debug)]
pub struct KeidModuleNode {
    // All `import` declarations in the file. This is used to resolve types to their qualified name.
    pub imports: Vec<ImportNode>,

    /// All classes, structs, and interfaces in the program, including their signature, fields,
    /// and references to their function implementations in [`ProgramNode::functions`].
    pub classes: Vec<ClassNode>,

    /// All functions in the program, including those in classes.
    pub functions: Vec<FunctionNode>,

    /// All `type X = Y` declarations.
    pub typedefs: Vec<TypedefNode>,

    /// All interface implementations in the program.
    pub interface_impls: Vec<InterfaceImplNode>,

    /// All namespace-level (i.e. static) fields in the program.
    pub fields: Vec<FieldNode>,

    /// All enums in the program.
    pub enums: Vec<EnumNode>,

    /// The namespace declared in the module.
    pub namespace: String,
}

#[derive(Debug, Clone)]
pub struct ImportNode {
    pub module: String,
}

#[derive(Debug)]
pub struct TypedefNode {
    pub base_name: String,
    pub generic_defs: Vec<GenericDefNode>,
    pub target_type: ComplexType,
}

impl GenericNode for TypedefNode {
    type Output = TypedefImplNode;

    fn create_impl(&self, _: &TypeProvider, generic_args: &[ComplexType]) -> anyhow::Result<Self::Output> {
        if generic_args.len() != generic_args.len() {
            panic!();
        }

        let mut full_name = self.base_name.clone();
        if !generic_args.is_empty() {
            write!(&mut full_name, "<{}>", utils::iter_join(generic_args)).unwrap();
        }

        Ok(TypedefImplNode {
            full_name,
            generic_impls: Vec::new(),
            target_type: self.target_type.clone(),
        })
    }
}

#[derive(Debug, Clone)]
pub struct AssociatedTypeNode {
    pub name: String,
    pub ty: ComplexType,
}

#[derive(Debug)]
pub struct InterfaceImplNode {
    pub module_id: usize,
    pub interface_id: usize,
    pub interface_name: String,
    pub interface_generics: Vec<ComplexType>,
    pub target_name: String,
    pub target_generics: Vec<ComplexType>,
    pub functions: Vec<usize>,
    pub accessors: Vec<AccessorNode>,
    pub generic_defs: Vec<GenericDefNode>,
    pub associated_types: Vec<AssociatedTypeNode>,
}

#[derive(Debug, Clone)]
pub struct ResolvedInterfaceImplNode {
    pub module_id: usize,
    pub interface_id: usize,
    pub target_name: String,
    pub generic_impls: Vec<ComplexType>,
    pub interface_generic_impls: Vec<ComplexType>,
    pub target_generic_impls: Vec<ComplexType>,
}

impl GenericNode for InterfaceImplNode {
    type Output = ResolvedInterfaceImplNode;

    fn create_impl(&self, type_provider: &TypeProvider, generic_args: &[ComplexType]) -> anyhow::Result<Self::Output> {
        if generic_args.len() != generic_args.len() {
            panic!();
        }

        let mut interface_generic_impls = Vec::new();
        for interface_generic in &self.interface_generics {
            interface_generic_impls.push(extract_type(type_provider, interface_generic.clone(), &self.generic_defs, generic_args)?);
        }

        let mut target_generic_impls = Vec::new();
        for target_generic in &self.target_generics {
            target_generic_impls.push(extract_type(type_provider, target_generic.clone(), &self.generic_defs, generic_args)?);
        }

        Ok(ResolvedInterfaceImplNode {
            module_id: self.module_id,
            interface_id: self.interface_id,
            target_name: self.target_name.clone(),
            generic_impls: generic_args.to_vec(),
            interface_generic_impls,
            target_generic_impls,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypedefImplNode {
    pub full_name: String,
    pub generic_impls: Vec<ComplexType>,
    pub target_type: ComplexType,
}

#[derive(Debug, Clone)]
pub struct FieldNode {
    pub is_extern: bool,
    pub is_const: bool,
    pub name: String,
    pub external_name: String,
    pub ty: ComplexType,
    pub initial_value: Option<Token<Expr>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResolvedFieldNode {
    pub name: String,
    pub ty: ComplexType,
}

#[derive(Debug, Clone)]
pub enum AccessorNodeType {
    Getter,
    Setter(String),
}

#[derive(Debug, Clone)]
pub struct AccessorNode {
    pub name: String,
    pub value_type: ComplexType,
    pub accessor_type: AccessorNodeType,
    pub function_id: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ClassType {
    Class,
    Struct,
    Interface,
    Enum,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumElementNode {
    pub name: String,
    pub data: Option<Vec<AnonymousStructField>>,
}

#[derive(Debug)]
pub struct EnumNode {
    /// The unique ID the module the enum is defined in.
    pub module_id: usize,
    /// The unique ID of the enum node within its file.
    pub id: usize,
    /// The fully qualified name of the enum, without any generic parameters.
    pub base_name: String,
    /// The names of all the generic arguments defined by the enum.
    pub generic_defs: Vec<GenericDefNode>,
    /// All of the elements defined by the enum.
    pub elements: Vec<EnumElementNode>,
}

/// An implementation of a enum type, with only concrete types in its implementation.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResolvedEnumNode {
    pub module_id: usize,
    pub source_id: usize,

    /// The full name of the enum, including the generic parameters, if any.
    pub full_name: String,

    /// The concrete types passed as the generic parameters to the enum.
    pub generic_impls: Vec<ComplexType>,
    pub elements: Vec<EnumElementNode>,
}

impl ResolvedEnumNode {
    pub fn as_complex_type(&self, src: &EnumNode) -> ComplexType {
        BasicType::Object(GenericIdentifier::from_name_with_args(&src.base_name, &self.generic_impls)).to_complex()
    }
}

impl GenericNode for EnumNode {
    type Output = ResolvedEnumNode;

    fn create_impl(&self, type_provider: &TypeProvider, generic_args: &[ComplexType]) -> anyhow::Result<Self::Output> {
        if generic_args.len() != generic_args.len() {
            panic!();
        }

        let mut full_name = self.base_name.clone();
        if !generic_args.is_empty() {
            write!(&mut full_name, "<{}>", utils::iter_join(generic_args)).unwrap();
        }

        let mut elements = Vec::new();
        for element_decl in &self.elements {
            let data = element_decl.data.clone().map(|data| {
                data.into_iter()
                    .map(|field| {
                        Ok(AnonymousStructField {
                            name: field.name,
                            ty: extract_type(type_provider, field.ty, &self.generic_defs, generic_args)?,
                        })
                    })
                    .collect::<anyhow::Result<Vec<_>>>()
            });
            elements.push(EnumElementNode {
                name: element_decl.name.clone(),
                data: match data {
                    Some(data) => Some(data?),
                    None => None,
                },
            });
        }

        Ok(ResolvedEnumNode {
            source_id: self.id,
            module_id: self.module_id,
            generic_impls: generic_args.to_vec(),
            full_name,
            elements,
        })
    }
}

#[derive(Debug)]
pub struct ClassNode {
    /// The unique ID the module the class is defined in.
    pub module_id: usize,
    /// The unique ID of the class node within its file.
    pub id: usize,
    /// The fully qualified name of the class, without any generic parameters.
    pub base_name: String,
    /// The names of all the generic arguments defined by the class.
    pub generic_defs: Vec<GenericDefNode>,
    /// The name and generic arguments for the superclass, or `None` if this class node represents the `object` root hierarchy node.
    pub superclass: Option<GenericIdentifier>,
    /// All of the functions defined by the class.
    pub functions: Vec<usize>,
    /// All of the accessors defined by the class.
    pub accessors: Vec<AccessorNode>,
    /// All of the fields defined by the class.
    pub fields: Vec<FieldNode>,
    /// The code for the class' constructor, if one is present.
    pub constructor: Option<Vec<Token<Statement>>>,
    /// The code for the class' destructor, if one is present.
    pub destructor: Option<Vec<Token<Statement>>>,
    pub class_type: ClassType,
}

impl GenericNode for ClassNode {
    type Output = ResolvedClassNode;

    fn create_impl(&self, type_provider: &TypeProvider, generic_args: &[ComplexType]) -> anyhow::Result<Self::Output> {
        if generic_args.len() != generic_args.len() {
            panic!();
        }

        let mut full_name = self.base_name.clone();
        if !generic_args.is_empty() {
            write!(&mut full_name, "<{}>", utils::iter_join(generic_args)).unwrap();
        }

        let mut fields = Vec::new();
        for field_decl in &self.fields {
            fields.push(ResolvedFieldNode {
                name: field_decl.name.clone(),
                ty: extract_type(type_provider, field_decl.ty.clone(), &self.generic_defs, generic_args)?,
            });
        }

        Ok(ResolvedClassNode {
            source_id: self.id,
            module_id: self.module_id,
            superclass: self.superclass.clone(),
            generic_impls: generic_args.to_vec(),
            full_name,
            fields,
            class_type: self.class_type,
        })
    }
}

/// An implementation of a class type, with only concrete types in its implementation.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResolvedClassNode {
    pub module_id: usize,
    pub source_id: usize,

    /// The full name of the class, including the generic parameters, if any.
    pub full_name: String,

    /// The concrete types passed as the generic parameters to the function.
    pub generic_impls: Vec<ComplexType>,

    pub superclass: Option<GenericIdentifier>,

    /// The types of the fields declared in the class,
    pub fields: Vec<ResolvedFieldNode>,
    pub class_type: ClassType,
}

impl ResolvedClassNode {
    pub fn as_complex_type(&self, src: &ClassNode) -> ComplexType {
        BasicType::Object(GenericIdentifier::from_name_with_args(&src.base_name, &self.generic_impls)).to_complex()
    }
}

impl IntoOpaqueType for ResolvedClassNode {
    type Output = OpaqueType;

    fn as_llvm_type(&self, cpl: &Compiler) -> OpaqueType {
        cpl.context.get_abi_class_data_type(cpl, self)
    }
}

#[derive(Debug, Clone)]
pub struct ParameterNode {
    pub name: String,
    pub ty: ComplexType,
}

#[derive(Debug, Clone, Default, Encode, Decode)]
pub struct GenericDefNode {
    pub name: String,
    pub interfaces: Vec<GenericIdentifier>,
}

/// An abstraction of an actual function implementation, including its generic parameters.
#[derive(Debug)]
pub struct FunctionNode {
    pub module_id: usize,
    pub id: usize,

    pub modifiers: Vec<FunctionModifier>,

    /// The name of the function as callable from within Keid, excluding generic arguments.
    /// For example, `std::io::File::open`, `core::collections::PositionalStorage::set_element`, etc
    pub base_name: String,

    pub namespace_name: String,

    pub function_type: FunctionContextType,

    /// The actual name of the function in the resulting compiled binary.
    /// For example, `std::impl::mem::malloc` could be the `callable_name`,
    /// whilst `malloc` is the external name (e.g. the name provided to the linker).
    pub external_name: String,

    /// The generic parameters of the function.
    pub generic_defs: Vec<GenericDefNode>,

    /// The names and types of the parameters of the function.
    pub params: Vec<ParameterNode>,

    /// The return type of the funtion.
    pub return_type: ComplexType,

    /// The body (statements) of the function, if any exist.
    pub body: Option<Vec<Token<Statement>>>,

    pub varargs: Varargs,
}

impl GenericNode for FunctionNode {
    type Output = ResolvedFunctionNode;

    fn create_impl(&self, type_provider: &TypeProvider, generic_args: &[ComplexType]) -> anyhow::Result<ResolvedFunctionNode> {
        if self.generic_defs.len() != generic_args.len() {
            panic!("generic defs: {:?}\ngeneric args: {:?}", self.generic_defs, generic_args);
        }

        let mut callable_name = self.base_name.clone();
        let mut external_name = self.external_name.clone();
        if !generic_args.is_empty() {
            write!(&mut callable_name, "<{}>", utils::iter_join(generic_args)).unwrap();
            write!(&mut external_name, "<{}>", utils::iter_join(generic_args)).unwrap();
        }

        let params: Vec<ComplexType> = self
            .params
            .iter()
            .map(|param| extract_type(type_provider, param.ty.clone(), &self.generic_defs, generic_args))
            .collect::<anyhow::Result<_>>()?;

        Ok(ResolvedFunctionNode {
            module_id: self.module_id,
            source_id: self.id,
            external_name: if callable_name != external_name {
                external_name
            } else {
                format!("{}({})", external_name, utils::iter_join(&params))
            },
            callable_name,
            generic_impls: generic_args.to_vec(),
            params,
            return_type: extract_type(type_provider, self.return_type.clone(), &self.generic_defs, generic_args)?,
            varargs: self.varargs,
        })
    }
}

/// An implementation of a function, with only concrete types in its signature.
#[derive(Debug, Clone)]
pub struct ResolvedFunctionNode {
    pub module_id: usize,
    pub source_id: usize,

    pub external_name: String,

    /// The entire callable name of the function, including generic arguments.
    pub callable_name: String,

    /// The concrete types passed as the generic parameters to the function.
    pub generic_impls: Vec<ComplexType>,

    /// The concrete types of the parameters to the function.
    pub params: Vec<ComplexType>,

    /// The concrete return type.
    pub return_type: ComplexType,

    pub varargs: Varargs,
}

impl ResolvedFunctionNode {
    pub fn externed(name: &str, params: &[ComplexType], varargs: Varargs, return_type: ComplexType) -> ResolvedFunctionNode {
        ResolvedFunctionNode {
            module_id: usize::MAX,
            source_id: usize::MAX,
            external_name: name.to_owned(),
            callable_name: name.to_owned(),
            generic_impls: Vec::new(),
            params: params.to_vec(),
            return_type,
            varargs,
        }
    }
}

impl IntoOpaqueType for ResolvedFunctionNode {
    type Output = OpaqueFunctionType;

    fn as_llvm_type(&self, cpl: &Compiler) -> Self::Output {
        let mut func_params: Vec<OpaqueType> = self
            .params
            .iter()
            .map(|param| {
                if param.is_struct(&cpl.type_provider) {
                    cpl.context.get_pointer_type(param.as_llvm_type(cpl))
                } else {
                    param.as_llvm_type(cpl)
                }
            })
            .collect::<Vec<OpaqueType>>();
        let return_type = if self.return_type.is_struct(&cpl.type_provider) {
            func_params.push(self.return_type.clone().to_reference().as_llvm_type(cpl));
            BasicType::Void.as_llvm_type(cpl)
        } else {
            self.return_type.as_llvm_type(cpl)
        };
        cpl.context.get_function_type(&func_params, self.varargs, return_type)
    }
}

impl ToString for ResolvedFunctionNode {
    fn to_string(&self) -> String {
        let mut params = self.params.iter().map(|param| param.to_string()).collect::<Vec<_>>();
        let len = params.len() - 1;
        match self.varargs {
            Varargs::Array => params[len] = format!("...{}", params[len]),
            Varargs::Native => params.push("...".to_string()),
            Varargs::None => (),
        }
        let param_str = params.join(", ");
        format!("`{}({}): {}`", self.callable_name, param_str, self.return_type.to_string())
    }
}
