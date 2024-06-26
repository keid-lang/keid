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
pub struct VirtualMethodTableEntry {
    /// The name of the function in the node it was originally defined in (i.e. the abstract/virtual definition, not the override definition)
    pub definition_name: GenericIdentifier,
    /// The name of the "root-most" implementation of the function for the class, or None if the definition function is abstract and has no implementation.
    pub implementation_name: Option<GenericIdentifier>,
}

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
    closure_count: usize,
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
            closure_count: 0,
            state: FunctionCompilerState::new(),
        }
    }

    pub fn consume(self) -> (Vec<CompilerError>, CompilationUnit) {
        (self.state.errors, self.unit)
    }

    pub fn compile(&mut self) {
        match self.compile_function_body() {
            Err(e) => self.state.errors.push(e),
            _ => self.builder.finish(),
        }
    }

    fn loc(&mut self, loc: &TokenLocation) {
        self.state.current_token = loc.clone();
    }

    fn get_source_function(&self) -> &FunctionNode {
        self.cpl.type_provider.get_source_function(self.func)
    }

    pub fn emit(&self, insn: Insn) -> OpaqueValue {
        self.builder.emit(insn)
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
        let root_block = self.builder.create_block();
        self.builder.append_block(&root_block);
        self.state.push_block(ScopeBlock::new(BlockType::Generic));
        self.initialize_local_vars().unwrap();
    }

    // fn get_hidden_function_names() -> Vec<&'static str> {
    //     vec![
    //         "core::array::copyFromPtr<uint8>",
    //         "core::array::fill<char>",
    //         "core::array::fill<uint8>",
    //         "core::collections::Iterator::__get_next#__impl#core::collections::Range",
    //         "core::collections::Range::create",
    //         "core::mem::Pointer::offset<uint8>",
    //         "core::mem::Pointer::to<uint8>",
    //         "core::string::String::fromUtf8",
    //         "core::runtime::pushStackFrame",
    //         "core::runtime::popStackFrame",
    //         "core::runtime::printStackFrames",
    //     ]
    // }

    fn push_stack_frame(&mut self) -> Result<()> {
        // if !Self::get_hidden_function_names().contains(&self.func.callable_name.as_str()) {
        //     let push_stack_frame_impl = self
        //         .cpl
        //         .type_provider
        //         .get_function_by_name(
        //             &GenericIdentifier::from_name("core::runtime::pushStackFrame"),
        //             &[BasicType::Object(GenericIdentifier::from_name("core::string::String")).to_complex()],
        //         )
        //         .unwrap();
        //     let push_stack_frame_ref = self.get_function_ref(&push_stack_frame_impl)?;
        //     let function_name = self.compile_string_literal_expr(&self.func.external_name)?;
        //     self.call_function(push_stack_frame_ref, &push_stack_frame_impl, &[function_name])?;
        // }

        Ok(())
    }

    fn pop_stack_frame(&mut self) -> Result<()> {
        // if !Self::get_hidden_function_names().contains(&self.func.callable_name.as_str()) {
        //     let pop_stack_frame_impl =
        //         self.cpl.type_provider.get_function_by_name(&GenericIdentifier::from_name("core::runtime::popStackFrame"), &[]).unwrap();
        //     let pop_stack_frame_ref = self.get_function_ref(&pop_stack_frame_impl)?;
        //     self.call_function(pop_stack_frame_ref, &pop_stack_frame_impl, &[])?;
        // }

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
            self.cpl.type_provider.context_generics.insert(BasicType::Object(GenericIdentifier::from_name(&source_defs[i].name)).to_complex(), arg);
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
                        self.compile_return(Some(&expr))?;

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

    fn resolve_interface_impl_function(&self, mut callable: ResolvedFunctionNode, interface_impl: &InterfaceImplNode) -> Result<ResolvedFunctionNode> {
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
        callable.return_type =
            crate::tree::extract_type(&self.cpl.type_provider, callable.return_type, &generics, &values).map_err(|e| compiler_error!(self, "{}", e))?;

        Ok(callable)
    }

    fn resolve_type_with_context(&self, ty: &ComplexType, import_map: &[ImportedMember]) -> Result<ComplexType> {
        if let Some(generic) = self.cpl.type_provider.context_generics.get(ty) {
            return Ok(generic.clone());
        }

        match ty.get_root_type() {
            BasicType::Object(ident) => {
                for absolute_name in utils::lookup_import_map(import_map, &ident.name) {
                    let generic_args = ident.generic_args.iter().map(|arg| self.resolve_type(arg)).collect::<Result<Vec<ComplexType>>>()?;

                    let abs_obj_type = GenericIdentifier::from_name_with_args(&absolute_name, &generic_args);
                    match self.cpl.type_provider.get_class_by_name(&abs_obj_type) {
                        Some(class_type) => {
                            return Ok(ty.clone().replace_root(class_type.as_complex_type(self.cpl.type_provider.get_source_class(&class_type))))
                        }
                        None => {
                            match self.cpl.type_provider.get_typedef_by_name(&abs_obj_type) {
                                Some(typedef) => return Ok(ty.clone().replace_root(typedef.target_type)),
                                None => {
                                    match self.cpl.type_provider.get_enum_by_name(&abs_obj_type) {
                                        Some(enum_type) => {
                                            return Ok(ty.clone().replace_root(enum_type.as_complex_type(self.cpl.type_provider.get_source_enum(&enum_type))))
                                        }
                                        None => {
                                            // if a generic type with the same name exists, then substitute the generic type with the actual type
                                            let source = self.get_source_function();
                                            if let Some(pos) = source.generic_defs.iter().position(|def| def.name == absolute_name) {
                                                return Ok(ty.clone().replace_root(self.func.generic_impls[pos].clone()));
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

    fn compile_closure(&mut self, closure_data_type: OpaqueType, class_type: ComplexType, callee: ResolvedFunctionNode) -> Result<OpaqueFunctionValue> {
        self.closure_count += 1;
        let name = format!("{}#_closure{}", self.func.external_name, self.closure_count);

        let mut callee_without_instance = callee.clone();
        callee_without_instance.params.remove(0);

        let closure_impl = ResolvedFunctionNode {
            external_name: name.clone(),
            callable_name: name.clone(),
            generic_impls: Vec::new(),
            params: [vec![BasicType::Void.to_complex().to_reference()], callee_without_instance.params.clone()].concat(),
            return_type: callee.return_type.clone(),
            module_id: self.unit.module_id,
            source_id: usize::MAX,
            varargs: callee.varargs.clone(),
        };
        let closure_function = self.cpl.add_function(
            &self.unit.mdl,
            &name,
            &closure_impl,
        );

        let mut bdl = closure_function.create_builder();
        let main_block = bdl.create_block();
        bdl.append_block(&main_block);

        let current_builder = self.builder.clone();

        self.builder.is_clone = true;
        self.builder = bdl;

        let mut args = Vec::new();
        let closure_data = closure_function.get_param(0);
        let instance_ptr = self.emit(Insn::GetElementPtr(closure_data, closure_data_type, 2));
        let instance = TypedValueContainer(TypedValue::new(class_type, instance_ptr)).load(self)?;
        args.push(instance);

        for i in 0..callee_without_instance.params.len() {
            let param_type = callee_without_instance.params[i].clone();
            let register = closure_function.get_param(i as u32 + 1);
            args.push(TypedValue::new(param_type, register));
        }

        let callee_ref = self.get_function_ref(&callee)?;
        let result = self.call_function(callee_ref, &callee, &args)?;

        if matches!(callee.return_type, ComplexType::Basic(BasicType::Void)) {
            self.emit(Insn::RetVoid);
        } else {
            if callee.return_type.is_struct(&self.cpl.type_provider) {
                let preallocated_return = closure_function.get_param(closure_impl.params.len() as u32);
                self.copy(&TypedValue::new(callee.return_type.clone(), result), &TypedValue::new(callee.return_type.clone(), preallocated_return))?;

                self.emit(Insn::RetVoid);
            } else {
                self.emit(Insn::Ret(result));
            }
        }

        self.builder = current_builder;
        self.builder.is_clone = false;

        Ok(closure_function.as_val())
    }

    fn resolve_interface_accessor(
        &mut self,
        class_instance: &TypedValue,
        ident: &GenericIdentifier,
        accessor_name: &Token<Identifier>,
    ) -> Result<Box<dyn ValueContainer>> {
        self.loc(&accessor_name.loc);

        let resolved_interface_impls = self.cpl.type_provider.get_resolved_interface_impls(ident);
        for resolved_interface_impl in resolved_interface_impls {
            let interface_impl = self.cpl.type_provider.get_source_interface_impl(&resolved_interface_impl);
            for accessor in &interface_impl.accessors {
                if accessor.name == accessor_name.token.0 {
                    let getter = self.cpl.type_provider.get_function_node(interface_impl.module_id, accessor.function_id).unwrap();
                    let accessor_impl =
                        getter.create_impl(&self.cpl.type_provider, &resolved_interface_impl.generic_impls).map_err(|e| compiler_error!(self, "{}", e))?;
                    return Ok(Box::new(AccessorValueContainer::new(accessor_impl, class_instance.val)));
                }
            }
        }

        if let Some(Some(superclass)) = self.cpl.type_provider.get_class_by_name(ident).map(|class| class.superclass) {
            let type_root = self.cpl.type_provider.get_class_by_name(&superclass).unwrap();
            return Ok(self.resolve_class_member_ptr(class_instance, &type_root, accessor_name)?);
        }

        Err(compiler_error!(self, "No such class member `{}.{}`", class_instance.ty.to_string(), accessor_name.token.0,))
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
                let element_ptr = self.emit(Insn::GetElementPtr(class_instance.val, type_root.as_llvm_type(self.cpl), field_offset + i as u32));
                return Ok(Box::new(TypedValueContainer(TypedValue::new(field.ty.clone(), element_ptr))));
            }
        }
        let getter_impl = 'getter_block: {
            let class = &self.cpl.type_provider.get_source_class(type_root);
            for accessor in &class.accessors {
                if accessor.name == field_name.token.0 {
                    let getter = self.cpl.type_provider.get_function_node(type_root.module_id, accessor.function_id).unwrap();
                    break 'getter_block getter.create_impl(&self.cpl.type_provider, &type_root.generic_impls).map_err(|e| compiler_error!(self, "{}", e))?;
                }
            }

            for function_id in &class.functions {
                let function = self.cpl.type_provider.get_function_node(type_root.module_id, *function_id).unwrap();
                if function.modifiers.contains(&FunctionModifier::Static) {
                    continue;
                }
                if utils::get_type_leaf(&function.base_name) == field_name.token.0 {
                    let resolved_function = function.create_impl(&self.cpl.type_provider, &type_root.generic_impls).unwrap();
                    let closure_abi = self.cpl.context.get_abi_closure_type(&[class_instance.ty.as_llvm_type(&self.cpl)]);
                    let closure_data_ptr = self.heap_allocate(closure_abi, None)?;

                    // set the ref count to 0
                    let const_zero = self.cpl.context.const_int(self.cpl.context.get_i64_type(), 0);
                    let ref_count_ptr = self.emit(Insn::GetElementPtr(closure_data_ptr, closure_abi, 0));
                    self.emit(Insn::Store(const_zero, ref_count_ptr));

                    let wrapper_function = self.compile_closure(closure_abi, class_instance.ty.clone(), resolved_function.clone())?;

                    // set the function pointer
                    let closure_func_ptr = self.emit(Insn::GetElementPtr(closure_data_ptr, closure_abi, 1));
                    self.emit(Insn::Store(wrapper_function.to_value(), closure_func_ptr));

                    // set the class instance
                    let closure_instance_ptr = self.emit(Insn::GetElementPtr(closure_data_ptr, closure_abi, 2));
                    self.copy(class_instance, &TypedValue::new(class_instance.ty.clone(), closure_instance_ptr))?;

                    let mut params = resolved_function.params;
                    params.remove(0); // remove the instance parameter

                    // TODO: call keid.scope_closure() on the closure instance

                    let closure_type = BasicType::Function(FunctionType {
                        params,
                        return_type: Box::new(resolved_function.return_type),
                        varargs: resolved_function.varargs,
                    })
                    .to_complex();
                    return Ok(Box::new(PreloadedTypedValueContainer(TypedValue::new(closure_type, closure_data_ptr))));
                }
            }

            let source_type = self.cpl.type_provider.get_source_class(type_root);
            return Ok(self.resolve_interface_accessor(
                class_instance,
                &GenericIdentifier::from_name_with_args(&source_type.base_name, &type_root.generic_impls),
                field_name,
            )?);
        };

        Ok(Box::new(AccessorValueContainer::new(getter_impl, class_instance.val)))
    }

    pub fn is_unsafe(&self) -> bool {
        self.get_source_function().modifiers.contains(&FunctionModifier::Unsafe)
            || self.state.block_stack.iter().any(|block| block.block_type == BlockType::Unsafe)
    }

    fn resolve_static_function(&mut self, sfc: &StaticFuncCall, args: &[Token<Expr>]) -> Result<(ResolvedFunctionNode, Vec<TypedValue>)> {
        let generic_args = self.parse_generic_args(&sfc.call.generic_args)?;
        let name = match sfc.call.callee.token.clone() {
            Expr::Ident(ident) => ident,
            x => unreachable!("{:?}", x),
        };

        for owner in utils::lookup_import_map(&self.import_map, &sfc.owner.to_string()) {
            for full_name in utils::lookup_import_map(&self.import_map, &format!("{}::{}", owner, name.token.0)) {
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

        let evaluated_arguments = args.iter().map(|arg| self.compile_expr(arg, None).map(|arg| arg.ty.to_string())).collect::<Result<Vec<String>>>()?;
        self.loc(&name.loc);
        Err(compiler_error!(
            self,
            "No such function overload `{}({})`",
            GenericIdentifier::from_name_with_args(&format!("{}::{}", sfc.owner.to_string(), name.token.0), &generic_args).to_string(),
            utils::iter_join(&evaluated_arguments),
        ))
    }

    fn add_call_site_attributes(&self, call_site: OpaqueValue, func: &ResolvedFunctionNode) {
        let is_interface = func.external_name.contains("#__impl#");
        for i in 0..func.params.len() {
            if is_interface && i == 0 {
                // interface functions always take a direct pointer to the object instance
                // TODO: should instance struct functions (interface or otherwise) be pass-by-reference for their own instance?
                continue;
            }
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
