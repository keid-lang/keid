use bincode::{Decode, Encode};

use super::{
    types::{BasicType, ComplexType},
    CompilerError,
};
use crate::common::Result;
use crate::func::utils::{self, iter_join};
use crate::tree;
use crate::tree::ast::*;
use crate::tree::*;
use crate::{compiler::llvm::OpaqueFunctionValue, compiler_error_loc};
use std::cell::RefCell;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Encode, Decode)]
pub struct GenericIdentifier {
    pub name: String,
    pub generic_args: Vec<ComplexType>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum NamespaceMemberType {
    Type,
    Member,
}

#[derive(Debug, PartialEq, Clone)]
pub struct NamespaceMember {
    pub member_type: NamespaceMemberType,
    pub name: String,
}

impl GenericIdentifier {
    pub fn from_name(name: &str) -> GenericIdentifier {
        GenericIdentifier {
            name: name.to_owned(),
            generic_args: Vec::new(),
        }
    }

    pub fn from_name_with_args(name: &str, generic_args: &[ComplexType]) -> GenericIdentifier {
        GenericIdentifier {
            name: name.to_owned(),
            generic_args: generic_args.to_vec(),
        }
    }

    pub fn from_complex_type(complex_type: &ComplexType) -> GenericIdentifier {
        let basic_type = complex_type.get_root_type();
        match basic_type {
            BasicType::Object(ident) => ident,
            _ => panic!("given type is not an object type: {:?}", basic_type),
        }
    }

    pub fn into_complex_type(self) -> ComplexType {
        BasicType::Object(self).to_complex()
    }
}

impl ToString for GenericIdentifier {
    fn to_string(&self) -> String {
        if self.generic_args.is_empty() {
            self.name.clone()
        } else {
            format!("{}<{}>", self.name, iter_join(&self.generic_args))
        }
    }
}

#[derive(Debug)]
struct QueuedFile {
    ast: KeidFile,
    module_id: usize,
}

struct CachedClassImpl {
    pub module_id: usize,
    pub class_id: usize,
    pub generics: Vec<ComplexType>,
}

pub struct TypeProvider {
    pub context_generics: HashMap<ComplexType, ComplexType>,
    queued_files: Vec<QueuedFile>,
    pub roots: Vec<KeidModuleNode>,
    compiled_functions: HashMap<String, OpaqueFunctionValue>,
    class_impls: RefCell<Vec<CachedClassImpl>>,
    resolved_interfaces: RefCell<Vec<GenericIdentifier>>,
}

impl TypeProvider {
    pub fn new() -> TypeProvider {
        TypeProvider {
            queued_files: Vec::new(),
            roots: Vec::new(),
            compiled_functions: HashMap::new(),
            context_generics: HashMap::new(),
            class_impls: RefCell::new(Vec::new()),
            resolved_interfaces: RefCell::new(Vec::new()),
        }
    }

    pub fn get_module_namespace(&self, module_id: usize) -> String {
        self.roots[module_id].namespace.clone()
    }

    pub fn get_resolved_interface_id(&self, name: &GenericIdentifier) -> usize {
        if let Some(id) = self
            .resolved_interfaces
            .borrow()
            .iter()
            .position(|x| x == name)
        {
            return id;
        }
        let mut resolved_interfaces = self.resolved_interfaces.borrow_mut();
        let id = resolved_interfaces.len();
        resolved_interfaces.push(name.clone());
        id
    }

    pub fn add_compiled_function(&mut self, external_name: &str, val: OpaqueFunctionValue) {
        if self.compiled_functions.contains_key(external_name) {
            panic!("function has already been compiled: {}", external_name);
        }
        self.compiled_functions
            .insert(external_name.to_owned(), val);
    }

    pub fn get_compiled_function(&self, external_name: &str) -> Option<OpaqueFunctionValue> {
        self.compiled_functions.get(external_name).cloned()
    }

    pub fn include_file(&mut self, ast: KeidFile, module_id: usize) {
        self.queued_files.push(QueuedFile { ast, module_id });
    }

    pub fn resolve_signatures(&mut self) -> Vec<(CompilerError, usize)> {
        let mut errors = Vec::new();
        let mut class_list = Vec::new();
        for file in &self.queued_files {
            class_list.extend(tree::ast_to_type_list(&file.ast));
        }

        let len = self.queued_files.len();
        for _ in 0..len {
            let file = self.queued_files.remove(0);

            let node = match tree::ast_to_keid_module_node(file.ast, file.module_id, &class_list) {
                ConvertResult::Ok(node) => node,
                ConvertResult::Err(e) => {
                    errors.extend(e.into_iter().map(|item| (item, file.module_id)));
                    continue;
                }
            };
            self.roots.push(node);
        }

        errors
    }

    pub fn get_impl_source_interface(&self, interface_impl: &InterfaceImplNode) -> &ClassNode {
        for root in &self.roots {
            for interface in &root.classes {
                if interface.class_type == ClassType::Interface
                    && interface.id == interface_impl.interface_id
                {
                    return interface;
                }
            }
        }
        panic!("given interface does not exist: {:#?}", interface_impl);
    }

    pub fn get_source_interface_impl(
        &self,
        resolved_interface_impl: &ResolvedInterfaceImplNode,
    ) -> &InterfaceImplNode {
        for root in &self.roots {
            for interface_impl in &root.interface_impls {
                if resolved_interface_impl.interface_id == interface_impl.interface_id
                    && resolved_interface_impl.target_name == interface_impl.target_name
                {
                    return interface_impl;
                }
            }
        }
        panic!(
            "given interface impl does not exist: {:#?}",
            resolved_interface_impl
        );
    }

    pub fn get_source_function(&self, func: &ResolvedFunctionNode) -> &FunctionNode {
        for root in &self.roots {
            for source_func in &root.functions {
                if source_func.id == func.source_id && source_func.module_id == func.module_id {
                    return source_func;
                }
            }
        }
        panic!("given function implementation does not exist: {:#?}", func);
    }

    pub fn get_source_class(&self, class: &ResolvedClassNode) -> &ClassNode {
        for root in &self.roots {
            for source_class in &root.classes {
                if source_class.id == class.source_id && source_class.module_id == class.module_id {
                    return source_class;
                }
            }
        }
        panic!("given class implementation does not exist");
    }

    pub fn get_source_enum(&self, enm: &ResolvedEnumNode) -> &EnumNode {
        for root in &self.roots {
            for source_enum in &root.enums {
                if source_enum.id == enm.source_id && source_enum.module_id == enm.module_id {
                    return source_enum;
                }
            }
        }
        panic!("given class implementation does not exist");
    }

    pub fn get_namespace_members(&self, namespace: &str) -> Vec<NamespaceMember> {
        let mut members = Vec::new();
        for root in &self.roots {
            for cls in &root.classes {
                if utils::get_type_namespace(&cls.base_name) == namespace {
                    members.push(NamespaceMember {
                        name: utils::get_type_leaf(&cls.base_name).to_owned(),
                        member_type: NamespaceMemberType::Type,
                    });
                }
            }
            for func in &root.functions {
                if utils::get_type_namespace(&func.base_name) == namespace {
                    members.push(NamespaceMember {
                        name: utils::get_type_leaf(&func.base_name).to_owned(),
                        member_type: NamespaceMemberType::Member,
                    });
                }
            }
            for enm in &root.enums {
                if utils::get_type_namespace(&enm.base_name) == namespace {
                    members.push(NamespaceMember {
                        name: utils::get_type_leaf(&enm.base_name).to_owned(),
                        member_type: NamespaceMemberType::Type,
                    });
                }
            }
        }
        members
    }

    pub fn get_module(&self, module_id: usize) -> &KeidModuleNode {
        &self.roots[module_id]
    }

    pub fn get_all_resolved_classes<'a>(&self) -> Vec<ResolvedClassNode> {
        let class_impls = self.class_impls.borrow();
        let mut impls = Vec::with_capacity(class_impls.len());
        for class_impl in class_impls.iter() {
            impls.push(
                self.get_class_node(class_impl.module_id, class_impl.class_id)
                    .unwrap()
                    .create_impl(self, &class_impl.generics)
                    .unwrap(),
            );
        }
        impls
    }

    pub fn get_class_node(&self, module_id: usize, class_id: usize) -> Option<&ClassNode> {
        for classes in self.roots.iter().map(|root| &root.classes) {
            for class in classes {
                if class.module_id == module_id && class.id == class_id {
                    return Some(class);
                }
            }
        }
        None
    }

    pub fn get_function_node(&self, module_id: usize, function_id: usize) -> Option<&FunctionNode> {
        for functions in self.roots.iter().map(|root| &root.functions) {
            for function in functions {
                if function.module_id == module_id && function.id == function_id {
                    return Some(function);
                }
            }
        }
        None
    }

    /// Returns the generic-implemented class, with its generic types and subsequent fields typed concretely.
    /// The types of all fields are also resolved using the resolution context of the declaring file.
    pub fn get_class_by_name(&self, object_type: &GenericIdentifier) -> Option<ResolvedClassNode> {
        for classes in self.roots.iter().map(|root| &root.classes) {
            for class in classes {
                if class.base_name == object_type.name {
                    if class.generic_defs.len() != object_type.generic_args.len() {
                        return None;
                    }

                    {
                        let mut class_impls = self.class_impls.borrow_mut();
                        class_impls.push(CachedClassImpl {
                            module_id: class.module_id,
                            class_id: class.id,
                            generics: object_type.generic_args.clone(),
                        });
                    }

                    return Some(class.create_impl(self, &object_type.generic_args).unwrap());
                }
            }
        }

        None
    }

    pub fn get_static_field_by_name(&self, field_name: &str) -> Option<ComplexType> {
        for fields in self.roots.iter().map(|root| &root.fields) {
            for field in fields {
                if field.name == field_name {
                    return Some(field.ty.clone());
                }
            }
        }

        None
    }

    pub fn get_enum_by_name(&self, declaring_type: &GenericIdentifier) -> Option<ResolvedEnumNode> {
        for enums in self.roots.iter().map(|root| &root.enums) {
            for enm in enums {
                if enm.base_name == declaring_type.name {
                    if enm.generic_defs.len() != declaring_type.generic_args.len() {
                        return None;
                    }

                    return Some(enm.create_impl(self, &declaring_type.generic_args).unwrap());
                }
            }
        }

        None
    }

    pub fn get_typedef_by_name(&self, object_type: &GenericIdentifier) -> Option<TypedefImplNode> {
        for typedefs in self.roots.iter().map(|root| &root.typedefs) {
            for typedef in typedefs {
                if typedef.base_name == object_type.name {
                    if typedef.generic_defs.len() != object_type.generic_args.len() {
                        return None;
                    }

                    return Some(
                        typedef
                            .create_impl(self, &object_type.generic_args)
                            .unwrap(),
                    );
                }
            }
        }

        None
    }

    pub fn has_any_function_by_name(&self, name: &str) -> bool {
        for functions in self.roots.iter().map(|root| &root.functions) {
            for function in functions {
                if function.base_name == name {
                    return true;
                }
            }
        }
        false
    }

    pub fn get_functions_by_name(
        &self,
        object_type: &GenericIdentifier,
    ) -> Vec<anyhow::Result<ResolvedFunctionNode>> {
        let mut results = Vec::new();
        for functions in self.roots.iter().map(|root| &root.functions) {
            for function in functions {
                if function.base_name == object_type.name {
                    if function.generic_defs.len() != object_type.generic_args.len() {
                        continue;
                    }

                    results.push(function.create_impl(self, &object_type.generic_args));
                }
            }
        }
        results
    }

    pub fn get_function_by_name(
        &self,
        object_type: &GenericIdentifier,
        args: &[ComplexType],
    ) -> Option<ResolvedFunctionNode> {
        for functions in self.roots.iter().map(|root| &root.functions) {
            for function in functions {
                if function.base_name == object_type.name {
                    if function.generic_defs.len() != object_type.generic_args.len() {
                        return None;
                    }

                    if function.params.len() != args.len() {
                        return None;
                    }

                    for i in 0..args.len() {
                        let function_param = match extract_type(
                            self,
                            function.params[i].ty.clone(),
                            &function.generic_defs,
                            &object_type.generic_args,
                        ) {
                            Ok(param) => param,
                            Err(_) => return None,
                        };
                        if !self.is_assignable_to(&args[i], &function_param) {
                            return None;
                        }
                    }

                    return Some(
                        match function.create_impl(self, &object_type.generic_args) {
                            Ok(param) => param,
                            Err(_) => return None,
                        },
                    );
                }
            }
        }

        None
    }

    pub fn is_assignable_to(&self, child: &ComplexType, parent: &ComplexType) -> bool {
        if child == parent {
            return true;
        }

        match (child, parent) {
            (ComplexType::Basic(BasicType::Unknown), _)
            | (_, ComplexType::Basic(BasicType::Unknown)) => return true,
            (
                ComplexType::Basic(BasicType::Object(child_ident)),
                ComplexType::Basic(BasicType::Object(parent_ident)),
            ) => {
                let mut child_ident = child_ident;
                loop {
                    if child_ident == parent_ident {
                        return true;
                    }

                    let child_class = match self.get_class_by_name(&child_ident) {
                        Some(class) => class,
                        None => match self.get_enum_by_name(&child_ident) {
                            Some(_) => return false,
                            None => {
                                panic!(
                                    "class does not exist: {:?} (parent = {:?})",
                                    child_ident,
                                    parent.to_string()
                                )
                            }
                        },
                    };
                    let resolved_interface_impls = self.get_resolved_interface_impls(child_ident);
                    for resolved_interface_impl in resolved_interface_impls {
                        let interface_impl =
                            self.get_source_interface_impl(&resolved_interface_impl);
                        let interface = self.get_impl_source_interface(interface_impl);
                        if parent_ident
                            == &GenericIdentifier::from_name_with_args(
                                &interface.base_name,
                                &resolved_interface_impl.interface_generic_impls,
                            )
                        {
                            // the parent is an interface type that is implemented for the child class
                            return true;
                        }
                    }

                    let source_class = self.get_source_class(&child_class);
                    match &source_class.superclass {
                        Some(superclass) => {
                            child_ident = superclass;
                        }
                        None => return false,
                    }
                }
            }
            (_, ComplexType::Nullable(item)) => {
                child == &ComplexType::Basic(BasicType::Null) || child == &**item || child == parent
            }
            _ => false,
        }
    }

    pub fn get_declaring_class(&self, func: &FunctionNode) -> Option<&ClassNode> {
        for classes in self.roots.iter().map(|root| &root.classes) {
            for class in classes {
                if class.module_id == func.module_id && class.functions.contains(&func.id) {
                    return Some(class);
                }
            }
        }
        None
    }

    pub fn get_resolved_interface_impls(
        &self,
        type_root: &GenericIdentifier,
    ) -> Vec<ResolvedInterfaceImplNode> {
        let mut impls = Vec::new();
        for interface_impls in self.roots.iter().map(|root| &root.interface_impls) {
            for interface_impl in interface_impls {
                if interface_impl.target_name == type_root.name {
                    match interface_impl.create_impl(self, &type_root.generic_args) {
                        Ok(resolved_interface_impl) => impls.push(resolved_interface_impl),
                        Err(_) => (),
                    }
                }
            }
        }
        impls
    }
}

pub struct SourceTypeRetriever<'a> {
    import_map: &'a HashMap<String, String>,
    type_provider: &'a TypeProvider,
}

impl<'a> SourceTypeRetriever<'a> {
    pub fn new(
        import_map: &'a HashMap<String, String>,
        type_provider: &'a TypeProvider,
    ) -> SourceTypeRetriever<'a> {
        SourceTypeRetriever {
            import_map,
            type_provider,
        }
    }

    pub fn retrieve_type(
        &self,
        loc: &TokenLocation,
        original_type: ComplexType,
    ) -> Result<ComplexType> {
        let root_type = original_type.get_root_type();
        match root_type {
            BasicType::Object(ty) => {
                let absolute_name = self.import_map.get(&ty.name).unwrap_or(&ty.name);
                let generics_ident =
                    GenericIdentifier::from_name_with_args(absolute_name, &ty.generic_args);

                Ok(
                    match self.type_provider.get_class_by_name(&generics_ident) {
                        Some(_) => BasicType::Object(generics_ident).to_complex(),
                        None => {
                            self.type_provider
                                .get_typedef_by_name(&generics_ident)
                                .ok_or_else(|| {
                                    compiler_error_loc!(
                                        loc,
                                        "[ER1] Could not resolve type `{}`",
                                        generics_ident.to_string()
                                    )
                                })?
                                .target_type
                        }
                    },
                )
            }
            _ => Ok(original_type),
        }
    }
}
