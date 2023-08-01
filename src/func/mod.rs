use crate::{
    common::*,
    common::{types::*, GenericIdentifier},
    compiler::{llvm::*, *},
    compiler_error,
    tree::ast::*,
    tree::*,
};

mod state;
use anyhow::anyhow;
pub use state::*;

pub mod compilers;
use compilers::*;

mod destructor;
use destructor::*;

mod initializer;
use initializer::*;

use self::utils::{FunctionCompilerUtils, ImportedMember, ScopeChange};

pub mod utils;

#[derive(Debug, Clone)]
pub struct LocalVar {
    name: String,
    value: TypedValue,
}

pub struct FunctionCompiler<'a> {
    pub cpl: &'a mut Compiler,
    unit: CompilationUnit,
    func: &'a ResolvedFunctionNode,
    import_map: Vec<ImportedMember>,
    builder: InsnBuilder,
    llvm_func: Function,
    pub state: FunctionCompilerState,
}

impl<'a> FunctionCompiler<'a> {
    pub fn new(
        cpl: &'a mut Compiler,
        unit: CompilationUnit,
        func: &'a ResolvedFunctionNode,
        import_map: Vec<ImportedMember>,
        llvm_func: Function,
    ) -> FunctionCompiler<'a> {
        let builder = llvm_func.create_builder();

        FunctionCompiler {
            cpl,
            unit,
            func,
            import_map,
            builder,
            llvm_func,
            state: FunctionCompilerState::new(),
        }
    }

    pub fn consume(self) -> (Vec<CompilerError>, CompilationUnit) {
        (self.state.errors, self.unit)
    }

    pub fn compile(&mut self) {
        match self.compile_function_body() {
            Err(e) => self.state.errors.push(e),
            _ => (),
        }
    }

    fn loc(&mut self, loc: &TokenLocation) {
        self.state.current_token = loc.clone();
    }

    fn get_source_function(&self) -> &FunctionNode {
        self.cpl.type_provider.get_source_function(self.func)
    }

    pub fn emit(&self, insn: Insn) -> OpaqueValue {
        self.builder.emit(insn, 0, 0)
    }

    fn initialize_local_vars(&mut self) -> Result<()> {
        let local_vars = {
            let params = self.get_source_function().params.clone();
            let mut local_vars = Vec::with_capacity(params.len());
            for i in 0..params.len() {
                let var_ref = self.llvm_func.get_param(i as u32);
                let var_type = self.func.params[i].clone();

                // copy all parameters to the local function stack
                let local_copy = self.emit(Insn::Alloca(var_type.as_llvm_type(self.cpl)));
                let value = TypedValue::new(var_type.clone(), local_copy);
                self.copy(&TypedValue::new(var_type, var_ref), &value)?;

                // destructors cannot scope the `this` parameter since its reference count is already equal to zero at the point it's passed to the destructor
                if !self.get_source_function().modifiers.contains(&FunctionModifier::Internal) {
                    self.try_scope(&value)?;
                }

                local_vars.push(LocalVar {
                    name: params[i].name.clone(),
                    value,
                });
            }
            local_vars
        };
        self.state.get_current_block_mut().locals.extend(local_vars);

        Ok(())
    }

    fn initialize_body(&mut self) {
        let root_block = self.state.new_block(&mut self.builder);
        self.builder.append_block(&root_block.llvm_block);
        self.state.push_block(&self.builder, root_block);
        self.initialize_local_vars().unwrap();
    }

    fn get_hidden_function_names() -> Vec<&'static str> {
        vec![
            "core::array::copyFromPtr<uint8>",
            "core::array::fill<char>",
            "core::array::fill<uint8>",
            "core::collections::Iterator::__get_next#__impl#core::collections::Range",
            "core::collections::Range::create",
            "core::mem::Pointer::offset<uint8>",
            "core::mem::Pointer::to<uint8>",
            "core::string::String::fromUtf8",
            "core::runtime::pushStackFrame",
            "core::runtime::popStackFrame",
            "core::runtime::printStackFrames",
        ]
    }

    fn push_stack_frame(&mut self) -> Result<()> {
        if !Self::get_hidden_function_names().contains(&self.func.callable_name.as_str()) {
            let push_stack_frame_impl = self
                .cpl
                .type_provider
                .get_function_by_name(
                    &GenericIdentifier::from_name("core::runtime::pushStackFrame"),
                    &[BasicType::Object(GenericIdentifier::from_name("core::string::String")).to_complex()],
                )
                .unwrap();
            let push_stack_frame_ref = self.get_function_ref(&push_stack_frame_impl)?;
            let function_name = self.compile_string_literal_expr(&self.func.external_name)?;
            self.call_function(push_stack_frame_ref, &push_stack_frame_impl, &[function_name])?;
        }

        Ok(())
    }

    fn pop_stack_frame(&mut self) -> Result<()> {
        if !Self::get_hidden_function_names().contains(&self.func.callable_name.as_str()) {
            let pop_stack_frame_impl =
                self.cpl.type_provider.get_function_by_name(&GenericIdentifier::from_name("core::runtime::popStackFrame"), &[]).unwrap();
            let pop_stack_frame_ref = self.get_function_ref(&pop_stack_frame_impl)?;
            self.call_function(pop_stack_frame_ref, &pop_stack_frame_impl, &[])?;
        }

        Ok(())
    }

    fn compile_function_body(&mut self) -> Result<()> {
        self.cpl.type_provider.context_generics.clear();

        let source_defs = self.get_source_function().generic_defs.clone();
        for i in 0..source_defs.len() {
            let arg = self.func.generic_impls[i].clone();
            if source_defs[i].name == arg.to_string() {
                panic!("recursive context generics: {} = {:#?}\n\nfunc: {:#?}", source_defs[i].name, arg, self.func);
            }
            self.cpl
                .type_provider
                .context_generics
                .insert(BasicType::Object(GenericIdentifier::from_name(&source_defs[i].name)).to_complex(), arg);
        }

        let source = self.get_source_function();
        if let Some(body) = &source.body {
            // TODO: can this be done without clone()?
            let body = body.clone();

            self.initialize_body();
            self.push_stack_frame()?;

            if body.len() == 1 {
                match body[0].token.clone() {
                    Statement::ArrowExpr(expr) => {
                        let return_type = self.resolve_type(&self.func.return_type)?;
                        let expr = self.compile_expr(&expr, Some(&return_type))?;

                        self.assert_assignable_to(&expr.ty, &return_type)?;

                        self.pop_block()?;
                        self.pop_stack_frame()?;
                        self.emit(Insn::Ret(expr.val));

                        return Ok(());
                    }
                    _ => (),
                }
            }

            self.compile_block(&body);
            self.compile_implicit_return()?;
        } else if source.modifiers.contains(&FunctionModifier::Internal) {
            let name = source.base_name.clone();
            self.initialize_body();
            if name.ends_with("keid.destructor") {
                self.compile_destructor()?;
            } else if name == "keid.init" {
                self.compile_global_initializer()?;
            } else {
                panic!("unknown internal function: {}", name);
            }
        }

        self.state.block_stack.clear();

        Ok(())
    }

    fn compile_implicit_return(&mut self) -> Result<()> {
        if let Some(body) = self.get_source_function().body.as_ref() {
            if !body.last().map(|tkn| matches!(&tkn.token, Statement::Return(_))).unwrap_or(false) {
                // if the last token is not a return, an implicit one gets added
                if self.func.return_type == BasicType::Void.to_complex() {
                    self.pop_block()?;
                    self.pop_stack_frame()?;
                    self.emit(Insn::RetVoid);
                } else {
                    // TODO: fix this -- needs to actually determine whether the function always returns or not
                    // self.state.errors.push(compiler_error!(self, "Expecting `return` statement"));
                }
            }
        }
        Ok(())
    }

    fn pop_block(&mut self) -> Result<()> {
        let popped_block = self.state.block_stack.pop().expect("no block to pop");

        // unscope all variables declared in popped block
        for var in popped_block.locals {
            self.try_unscope(&var.value)?;
        }

        Ok(())
    }

    /// Returns a LocalVar as a pointer to the given identifier.
    fn resolve_ident(&mut self, ident: &Token<Identifier>) -> Result<LocalVar> {
        self.loc(&ident.loc);

        let mut all_locals = Vec::new();
        for i in (0..self.state.block_stack.len()).rev() {
            let locals = self.state.block_stack[i].locals.clone();
            for local in locals {
                all_locals.push(local);
            }
        }

        for local in &all_locals {
            if local.name == ident.token.0 {
                return Ok(local.clone());
            }
        }

        Err(compiler_error!(
            self,
            "No such identifier `{}`; help: local identifiers are {}",
            ident.token.0,
            utils::iter_join(&all_locals.iter().map(|local| format!("`{}`", local.name)).collect::<Vec<String>>())
        ))
    }

    fn resolve_interface_impl_function(
        &self,
        mut callable: ResolvedFunctionNode,
        interface_impl: &InterfaceImplNode,
    ) -> Result<ResolvedFunctionNode> {
        let mut generics = Vec::with_capacity(interface_impl.associated_types.len());
        let mut values = Vec::with_capacity(interface_impl.associated_types.len());
        for assoc in &interface_impl.associated_types {
            generics.push(GenericDefNode {
                name: assoc.name.clone(),
                interfaces: Vec::new(),
            });
            values.push(assoc.ty.clone());
        }

        callable.params = callable
            .params
            .into_iter()
            .map(|ty| crate::tree::extract_type(&self.cpl.type_provider, ty, &generics, &values))
            .collect::<anyhow::Result<_>>()
            .map_err(|e| compiler_error!(self, "{}", e))?;
        callable.return_type = crate::tree::extract_type(&self.cpl.type_provider, callable.return_type, &generics, &values)
            .map_err(|e| compiler_error!(self, "{}", e))?;

        Ok(callable)
    }

    fn resolve_type_with_context(&self, ty: &ComplexType, import_map: &[ImportedMember]) -> Result<ComplexType> {
        if let Some(generic) = self.cpl.type_provider.context_generics.get(ty) {
            return Ok(generic.clone());
        }

        match ty {
            ComplexType::Basic(BasicType::Object(ident)) => {
                for absolute_name in utils::lookup_import_map(import_map, &ident.name) {
                    let generic_args = ident.generic_args.iter().map(|arg| self.resolve_type(arg)).collect::<Result<Vec<ComplexType>>>()?;

                    let abs_obj_type = GenericIdentifier::from_name_with_args(&absolute_name, &generic_args);
                    match self.cpl.type_provider.get_class_by_name(&abs_obj_type) {
                        Some(class_type) => return Ok(class_type.as_complex_type(self.cpl.type_provider.get_source_class(&class_type))),
                        None => {
                            match self.cpl.type_provider.get_typedef_by_name(&abs_obj_type) {
                                Some(typedef) => return Ok(typedef.target_type),
                                None => {
                                    match self.cpl.type_provider.get_enum_by_name(&abs_obj_type) {
                                        Some(enum_type) => {
                                            return Ok(enum_type.as_complex_type(self.cpl.type_provider.get_source_enum(&enum_type)))
                                        }
                                        None => {
                                            // if a generic type with the same name exists, then substitute the generic type with the actual type
                                            let source = self.get_source_function();
                                            if let Some(pos) = source.generic_defs.iter().position(|def| def.name == absolute_name) {
                                                return Ok(self.func.generic_impls[pos].clone());
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                Err(compiler_error!(self, "[ER2] Could not resolve type: `{}`", ident.to_string()))
            }
            _ => Ok(ty.clone()),
        }
    }

    fn resolve_type(&self, ty: &ComplexType) -> Result<ComplexType> {
        self.resolve_type_with_context(ty, &self.import_map)
    }

    fn resolve_class_member_ptr(
        &mut self,
        class_instance: &TypedValue,
        type_root: &ResolvedClassNode,
        field_name: &Token<Identifier>,
    ) -> Result<Box<dyn ValueContainer>> {
        let field_offset = match type_root.class_type {
            ClassType::Class => 2,                         // ref count + info ptr
            ClassType::Struct | ClassType::Interface => 1, // info ptr
            ClassType::Enum => panic!("enums have no members"),
        };
        self.loc(&field_name.loc);
        let fields = &type_root.fields;
        for i in 0..fields.len() {
            let field = &fields[i];
            if field.name == field_name.token.0 {
                let element_ptr =
                    self.emit(Insn::GetElementPtr(class_instance.val, type_root.as_llvm_type(self.cpl), field_offset + i as u32));
                return Ok(Box::new(TypedValueContainer(TypedValue::new(field.ty.clone(), element_ptr))));
            }
        }
        let getter_impl = 'getter_block: {
            let class = &self.cpl.type_provider.get_source_class(type_root);

            for accessor in &class.accessors {
                if accessor.name == field_name.token.0 {
                    let getter = self.cpl.type_provider.get_function_node(type_root.module_id, accessor.function_id).unwrap();
                    break 'getter_block getter
                        .create_impl(&self.cpl.type_provider, &type_root.generic_impls)
                        .map_err(|e| compiler_error!(self, "{}", e))?;
                }
            }

            let source_type = self.cpl.type_provider.get_source_class(type_root);
            let resolved_interface_impls = self
                .cpl
                .type_provider
                .get_resolved_interface_impls(&GenericIdentifier::from_name_with_args(&source_type.base_name, &type_root.generic_impls));
            for resolved_interface_impl in resolved_interface_impls {
                let interface_impl = self.cpl.type_provider.get_source_interface_impl(&resolved_interface_impl);
                for accessor in &interface_impl.accessors {
                    if accessor.name == field_name.token.0 {
                        let getter = self.cpl.type_provider.get_function_node(interface_impl.module_id, accessor.function_id).unwrap();
                        break 'getter_block getter
                            .create_impl(&self.cpl.type_provider, &resolved_interface_impl.generic_impls)
                            .map_err(|e| compiler_error!(self, "{}", e))?;
                    }
                }
            }
            return Err(compiler_error!(
                self,
                "No such field or `get` accessor `{}` in type `{}`",
                field_name.token.0,
                type_root.full_name,
            ));
        };

        Ok(Box::new(AccessorValueContainer::new(getter_impl, class_instance.val)))
    }

    pub fn is_unsafe(&self) -> bool {
        self.get_source_function().modifiers.contains(&FunctionModifier::Unsafe)
            || self.state.block_stack.iter().any(|block| block.block_type == BlockType::Unsafe)
    }

    fn resolve_static_function(&mut self, sfc: &StaticFuncCall, args: &[Token<Expr>]) -> Result<(ResolvedFunctionNode, Vec<TypedValue>)> {
        let generic_args = self.parse_generic_args(&sfc.call.generic_args)?;

        for owner in utils::lookup_import_map(&self.import_map, &sfc.owner.to_string()) {
            for full_name in utils::lookup_import_map(&self.import_map, &format!("{}::{}", owner, sfc.call.name.token.0)) {
                let ident = GenericIdentifier::from_name_with_args(&full_name, &generic_args);
                match self.find_function(&ident, args, None)? {
                    Some(sig) => return Ok(sig),
                    None => match self.find_function(
                        &GenericIdentifier::from_name_with_args(
                            &format!("{}::{}", self.get_source_function().namespace_name, sfc.get_full_name()),
                            &generic_args,
                        ),
                        args,
                        None,
                    )? {
                        Some(sig) => return Ok(sig),
                        None => (),
                    },
                }
            }
        }

        let evaluated_arguments =
            args.iter().map(|arg| self.compile_expr(arg, None).map(|arg| arg.ty.to_string())).collect::<Result<Vec<String>>>()?;
        self.loc(&sfc.call.name.loc);
        Err(compiler_error!(
            self,
            "No such function overload `{}({})`",
            GenericIdentifier::from_name_with_args(&format!("{}::{}", sfc.owner.to_string(), sfc.call.name.token.0), &generic_args)
                .to_string(),
            utils::iter_join(&evaluated_arguments),
        ))
    }

    fn resolve_instance_function(
        &mut self,
        fc: &FuncCall,
        instance: &TypedValue,
        args: &[Token<Expr>],
    ) -> Result<(ResolvedFunctionNode, Vec<TypedValue>)> {
        let generic_args = self.parse_generic_args(&fc.generic_args)?;
        Ok(match &instance.ty {
            ComplexType::Basic(BasicType::Object(ident)) => {
                let func = GenericIdentifier::from_name_with_args(&format!("{}::{}", ident.name, fc.name.token.0), &generic_args);
                match self.find_function(&func, args, Some(instance.clone()))? {
                    Some(func) => func,
                    None => {
                        let resolved_interface_impls = self.cpl.type_provider.get_resolved_interface_impls(ident);
                        for resolved_interface_impl in resolved_interface_impls {
                            let (module_id, function_ids) = {
                                let source_impl = self.cpl.type_provider.get_source_interface_impl(&resolved_interface_impl);
                                (source_impl.module_id, source_impl.functions.clone())
                            };
                            for function_id in function_ids {
                                let function_base_name =
                                    self.cpl.type_provider.get_function_node(module_id, function_id).unwrap().base_name.clone();
                                if function_base_name.contains(&format!("::{}#__impl#", fc.name.token.0)) {
                                    match self.find_function(
                                        &GenericIdentifier::from_name_with_args(
                                            &function_base_name,
                                            &resolved_interface_impl.generic_impls,
                                        ),
                                        args,
                                        Some(instance.clone()),
                                    )? {
                                        Some(func) => return Ok(func),
                                        None => (),
                                    }
                                }
                            }
                        }

                        let mut similar_methods = Vec::new();
                        let candidates = self.cpl.type_provider.get_functions_by_name(&func);
                        for candidate in candidates {
                            if let Ok(candidate) = candidate {
                                similar_methods.push(format!(
                                    "{}({}): {}",
                                    candidate.callable_name,
                                    utils::iter_join(&candidate.params),
                                    candidate.return_type.to_string(),
                                ));
                            }
                        }

                        let evaluated_args = args
                            .iter()
                            .map(|arg| self.compile_expr(arg, None).map(|arg| arg.ty.to_string()))
                            .collect::<Result<Vec<String>>>()?;
                        self.loc(&fc.name.loc);
                        return Err(compiler_error!(
                            self,
                            "No such method overload `{}({})` in type `{}` or derived interface implementations\n        hint: the following overloads exist:\n        {}",
                            func.to_string(),
                            utils::iter_join(&evaluated_args),
                            ident.to_string(),
                            similar_methods.join("\n        "),
                        ));
                    }
                }
            }
            other => return Err(compiler_error!(self, "No such method in non-object type `{}::{}`", other.to_string(), fc.name.token.0,)),
        })
    }

    fn add_call_site_attributes(&self, call_site: OpaqueValue, func: &ResolvedFunctionNode) {
        for i in 0..func.params.len() {
            if func.params[i].is_struct(&self.cpl.type_provider)
                && (!func.params[i].to_string().starts_with("core::mem::Pointer")
                    || (func.module_id != usize::MAX && {
                        let source = self.cpl.type_provider.get_source_function(func);
                        !source.modifiers.contains(&FunctionModifier::Extern)
                    }))
            {
                self.builder.add_call_site_attribute(call_site, i + 1, func.params[i].as_llvm_type(self.cpl), "byval");
            }
        }
    }

    pub fn get_function_ref(&mut self, func: &ResolvedFunctionNode) -> Result<OpaqueFunctionValue> {
        if let Some(extern_func) = self.unit.externed_functions.iter().find(|externed| &externed.external_name == &func.external_name) {
            Ok(extern_func.value_ref)
        } else {
            // intrinsic
            if func.module_id == usize::MAX {
                let intrinsic_function = self.cpl.add_function(&self.unit.mdl, &func.external_name, func).as_val();
                self.unit.externed_functions.push(ExternedFunction {
                    external_name: func.external_name.clone(),
                    value_ref: intrinsic_function,
                });
                Ok(intrinsic_function)
            } else {
                let mut compiled_function = self.cpl.type_provider.get_compiled_function(&func.external_name);
                if compiled_function.is_none() {
                    compiled_function = Some(self.cpl.queue_function_compilation(func.clone()));
                }

                if func.module_id != self.unit.module_id {
                    let source_external_name = func.external_name.clone();
                    compiled_function = Some(self.cpl.add_function(&self.unit.mdl, &source_external_name, func).as_val());
                }

                let compiled_function = compiled_function.ok_or(anyhow!("Compiled function not found: {}", func.callable_name)).unwrap();
                self.unit.externed_functions.push(ExternedFunction {
                    external_name: func.external_name.clone(),
                    value_ref: compiled_function,
                });
                Ok(compiled_function)
            }
        }
    }

    fn try_scope(&mut self, object: &TypedValue) -> Result<bool> {
        self.try_change_scope(object, ScopeChange::Inside)
    }

    fn try_unscope(&mut self, object: &TypedValue) -> Result<bool> {
        self.try_change_scope(object, ScopeChange::Outside)
    }
}
