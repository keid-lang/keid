use crate::{
    common::{types::*, *},
    compiler::llvm::{self, *},
    compiler_error,
    func::{
        utils::{self, get_type_leaf, FunctionCompilerUtils},
        FunctionCompiler,
    },
    tree::ast::*,
    tree::*,
};

use super::{ArrayCompiler, ExprCompiler};

pub enum InterfaceInvocation {
    // Takes the object instance as the value.
    Instance(TypedValue),
    // Takes the type of the target as the value.
    Static(ComplexType),
}

pub trait CallCompiler {
    /// expected that `args` includes the instance
    fn find_function(
        &mut self,
        name: &GenericIdentifier,
        args: &[Token<Expr>],
        instance: Option<TypedValue>,
    ) -> Result<Option<(ResolvedFunctionNode, Vec<TypedValue>)>>;

    fn compile_new_call(&mut self, nc: &NewCall) -> Result<TypedValue>;

    fn instantiate_object(&mut self, name: ComplexType) -> Result<TypedValue>;

    fn compile_static_func_call(&mut self, sfc: &StaticFuncCall) -> Result<TypedValue>;

    fn compile_instance_func_call(
        &mut self,
        fc: &FuncCall,
        instance: &TypedValue,
    ) -> Result<TypedValue>;

    fn get_interface_method_ptr(
        &mut self,
        invocation: &InterfaceInvocation,
        interface_id: usize,
        method_id: usize,
    ) -> Result<OpaqueFunctionValue>;

    fn compile_func_call(
        &mut self,
        func_ref: OpaqueFunctionValue,
        callable: &ResolvedFunctionNode,
        args: &[TypedValue],
    ) -> Result<TypedValue>;

    fn call_function(
        &mut self,
        func_ref: OpaqueFunctionValue,
        callable: &ResolvedFunctionNode,
        args: &[TypedValue],
    ) -> Result<OpaqueValue>;
}

impl<'a> CallCompiler for FunctionCompiler<'a> {
    fn find_function(
        &mut self,
        name: &GenericIdentifier,
        args: &[Token<Expr>],
        instance: Option<TypedValue>,
    ) -> Result<Option<(ResolvedFunctionNode, Vec<TypedValue>)>> {
        let mut name = name.clone();
        let mut args = args.to_vec();
        if let Some(instance) = instance {
            args.insert(
                0,
                Token {
                    loc: TokenLocation { start: 0, end: 0 },
                    token: Expr::Psuedo(instance.clone()),
                },
            );

            // assume generic args from object instance
            match &instance.ty {
                ComplexType::Basic(BasicType::Object(instance_name)) => {
                    if name.generic_args.is_empty() && !instance_name.generic_args.is_empty() {
                        name = GenericIdentifier::from_name_with_args(
                            &name.name,
                            &instance_name.generic_args,
                        );
                    }
                }
                _ => (),
            }
        }
        let candidates = self.cpl.type_provider.get_functions_by_name(&name);
        'candidate_loop: for candidate in candidates {
            let candidate = match candidate {
                Ok(candidate) => candidate,
                Err(_) => continue,
            };
            match candidate.varargs {
                Varargs::Array | Varargs::Native => {
                    if args.len() < candidate.params.len() - 1 {
                        continue;
                    }
                }
                Varargs::None => {
                    if candidate.params.len() != args.len() {
                        continue;
                    }
                }
            }

            macro_rules! varargs_element_type {
                () => {{
                    match self.resolve_type(candidate.params.last().unwrap())? {
                        ComplexType::Array(element_type) => *element_type,
                        _ => unreachable!(),
                    }
                }};
            }

            let extended_type_hint = match candidate.varargs {
                Varargs::Array => Some(varargs_element_type!()),
                Varargs::Native | Varargs::None => None,
            };

            macro_rules! evaluate_arguments {
                () => {{
                    let mut evaluated_arguments = {
                        args.iter()
                            .enumerate()
                            .map(|(i, arg)| {
                                let type_hint = if i >= candidate.params.len() {
                                    extended_type_hint.as_ref().unwrap()
                                } else {
                                    &candidate.params[i]
                                };
                                let expr = self.compile_expr(arg, Some(type_hint));
                                match expr {
                                    Ok(expr) => match type_hint {
                                        ComplexType::Basic(BasicType::Object(_)) => {
                                            self.autobox_primitive(expr.clone())
                                        }
                                        _ => self.implicit_cast(expr, &type_hint),
                                    },
                                    _ => expr,
                                }
                            })
                            .collect::<Result<Vec<TypedValue>>>()?
                    };
                    let spread = candidate.varargs == Varargs::Array
                        && if let Some(ComplexType::Spread(box ComplexType::Array(element))) =
                            evaluated_arguments.last().map(|arg| arg.ty.clone())
                        {
                            let len = evaluated_arguments.len();
                            evaluated_arguments[len - 1].ty = ComplexType::Array(element);
                            true
                        } else {
                            false
                        };
                    (spread, evaluated_arguments)
                }};
            }

            llvm::set_eval_only(true);
            let (spread, evaluated_arguments) = evaluate_arguments!();
            llvm::set_eval_only(false);

            for (i, evaluated) in evaluated_arguments.iter().enumerate() {
                if candidate.varargs == Varargs::Array {
                    if spread && i == candidate.params.len() - 1 {
                        if !self
                            .cpl
                            .type_provider
                            .is_assignable_to(&evaluated.ty, &varargs_element_type!().to_array())
                        {
                            continue 'candidate_loop;
                        }
                    } else if i >= candidate.params.len() - 1
                        && !self
                            .cpl
                            .type_provider
                            .is_assignable_to(&evaluated.ty, &varargs_element_type!())
                    {
                        continue 'candidate_loop;
                    }
                } else if !self
                    .cpl
                    .type_provider
                    .is_assignable_to(&evaluated.ty, &self.resolve_type(&candidate.params[i])?)
                {
                    continue 'candidate_loop;
                }
            }

            // recompile arguments again here, this time actually emitting the instructions
            let (_, mut evaluated_arguments) = evaluate_arguments!();
            if candidate.varargs == Varargs::Array {
                let element_type = varargs_element_type!();
                let const_null = TypedValue::new(
                    BasicType::Null.to_complex(),
                    self.cpl
                        .context
                        .const_null(element_type.as_llvm_type(self.cpl)),
                );
                if !spread {
                    // if we passed only non-varargs parameters, create an empty array and pass it at the end
                    if evaluated_arguments.len() == candidate.params.len() - 1 {
                        let const_zero = TypedValue::new(
                            BasicType::USize.to_complex(),
                            self.cpl
                                .context
                                .const_int(self.cpl.context.get_isize_type(), 0),
                        );
                        let arr =
                            self.compile_new_array(&element_type, &const_null, &const_zero)?;
                        evaluated_arguments.push(arr);
                    } else {
                        let varargs_size = args.len() - candidate.params.len() + 1;
                        let const_size = TypedValue::new(
                            BasicType::USize.to_complex(),
                            self.cpl
                                .context
                                .const_int(self.cpl.context.get_isize_type(), varargs_size as _),
                        );
                        let arr =
                            self.compile_new_array(&element_type, &const_null, &const_size)?;

                        // otherwise, collect all the varargs parameters into a single slice parameter
                        for i in 0..varargs_size {
                            let const_index = TypedValue::new(
                                BasicType::USize.to_complex(),
                                self.cpl
                                    .context
                                    .const_int(self.cpl.context.get_isize_type(), i as _),
                            );
                            let arg_value = evaluated_arguments.remove(candidate.params.len() - 1);
                            self.store_array_element(&arr, &arg_value, &const_index, true)?;
                        }
                        evaluated_arguments.push(arr);
                    }
                }
            }

            return Ok(Some((candidate, evaluated_arguments)));
        }

        Ok(None)
    }

    fn compile_static_func_call(&mut self, sfc: &StaticFuncCall) -> Result<TypedValue> {
        let (callable, args) = self.resolve_static_function(sfc, &sfc.call.args)?;
        let func_ref = self.get_function_ref(&callable)?;
        self.compile_func_call(func_ref, &callable, &args)
    }

    fn compile_instance_func_call(
        &mut self,
        fc: &FuncCall,
        instance: &TypedValue,
    ) -> Result<TypedValue> {
        let generic_args = self.parse_generic_args(&fc.generic_args)?;
        let ident = GenericIdentifier::from_complex_type(&instance.ty);

        let (mut candidates, is_interface) = match self.cpl.type_provider.get_class_by_name(&ident)
        {
            Some(class) => match class.class_type {
                ClassType::Interface => (
                    {
                        let interface = self
                            .cpl
                            .type_provider
                            .get_class_node(class.module_id, class.source_id)
                            .unwrap();
                        match interface.functions.iter().position(|id| {
                            get_type_leaf(
                                &self
                                    .cpl
                                    .type_provider
                                    .get_function_node(interface.module_id, *id)
                                    .unwrap()
                                    .base_name,
                            ) == fc.name.token.0
                        }) {
                            Some(function_id) => vec![(
                                self.cpl.type_provider.get_resolved_interface_id(&ident),
                                function_id,
                                interface.base_name.clone(),
                                class.generic_impls,
                            )],
                            None => vec![],
                        }
                    },
                    true,
                ),
                _ => (vec![], false),
            },
            None => match self.cpl.type_provider.get_enum_by_name(&ident) {
                Some(_) => (vec![], false),
                None => {
                    return Err(compiler_error!(
                        self,
                        "[ER3] Could not resolve type `{}`",
                        ident.to_string()
                    ))
                }
            },
        };

        let name = format!("{}::{}", ident.name, fc.name.token.0);
        if !is_interface {
            let func_ident = GenericIdentifier::from_name_with_args(&name, &generic_args);
            match self.find_function(&func_ident, &fc.args, Some(instance.clone()))? {
                Some((func, args)) => {
                    let func_ref = self.get_function_ref(&func)?;
                    let res = self.call_function(func_ref, &func, &args)?;
                    return Ok(TypedValue::new(func.return_type.clone(), res));
                }
                None => {
                    let resolved_interface_impls =
                        self.cpl.type_provider.get_resolved_interface_impls(&ident);
                    for resolved_interface_impl in &resolved_interface_impls {
                        let interface_impl = self
                            .cpl
                            .type_provider
                            .get_source_interface_impl(resolved_interface_impl);
                        let interface = self
                            .cpl
                            .type_provider
                            .get_impl_source_interface(interface_impl);
                        for (i, function_id) in interface.functions.clone().into_iter().enumerate()
                        {
                            let function_name = self
                                .cpl
                                .type_provider
                                .get_function_node(interface.module_id, function_id)
                                .unwrap()
                                .base_name
                                .clone();
                            let function_name = get_type_leaf(&function_name);
                            if function_name == fc.name.token.0 {
                                candidates.push((
                                    self.cpl.type_provider.get_resolved_interface_id(
                                        &GenericIdentifier::from_name_with_args(
                                            &interface.base_name,
                                            &resolved_interface_impl.interface_generic_impls,
                                        ),
                                    ),
                                    i,
                                    interface.base_name.clone(),
                                    resolved_interface_impl.interface_generic_impls.clone(),
                                ));
                            }
                        }
                    }
                }
            }
        }

        let any_exists = self.cpl.type_provider.has_any_function_by_name(&name);

        if candidates.is_empty() {
            if any_exists {
                let evaluated_args = fc
                    .args
                    .iter()
                    .map(|arg| self.compile_expr(arg, None).map(|arg| arg.ty.to_string()))
                    .collect::<Result<Vec<String>>>()?;

                let err_text = format!(
                    "The instance method `{}::{}({})` does not exist\n        hint: the following overloads exist: ",
                    ident.to_string(),
                    fc.name.token.0,
                    utils::iter_join(&evaluated_args)
                );

                let similar_methods = self.cpl.type_provider.get_all_functions_by_name(&name);
                let similar_methods = similar_methods
                    .into_iter()
                    .map(|similar_method| {
                        format!(
                            "{}({})",
                            similar_method.base_name,
                            utils::iter_join(
                                &similar_method
                                    .params
                                    .iter()
                                    .skip(1)
                                    .map(|param| param.ty.to_string())
                                    .collect::<Vec<String>>()
                            )
                        )
                    })
                    .collect::<Vec<String>>();

                Err(compiler_error!(
                    self,
                    "{}{}",
                    err_text,
                    similar_methods.join(", ")
                ))
            } else {
                Err(compiler_error!(
                    self,
                    "No such instance method `{}::{}`",
                    ident.to_string(),
                    fc.name.token.0
                ))
            }
        } else if candidates.len() > 1 {
            Err(compiler_error!(
                self,
                "Ambiguous call to method `{}::{}`",
                ident.to_string(),
                fc.name.token.0
            ))
        } else {
            let (source_id, function_id, interface_name, interface_generic_impls) =
                candidates.remove(0);
            let interface_func_ident = GenericIdentifier::from_name_with_args(
                &format!("{}::{}", interface_name, fc.name.token.0),
                &interface_generic_impls,
            );
            let (interface_func_impl, evaluated_args) = match self.find_function(
                &interface_func_ident,
                &fc.args,
                Some(instance.clone()),
            )? {
                Some(interface_func_impl) => interface_func_impl,
                None => {
                    self.loc(&fc.name.loc);
                    return Err(compiler_error!(
                        self,
                        "Could not find interface method overload `{}`",
                        interface_func_ident.to_string()
                    ));
                }
            };

            let method_ptr = self.get_interface_method_ptr(
                &InterfaceInvocation::Instance(instance.clone()),
                source_id,
                function_id,
            )?;

            Ok(self.compile_func_call(method_ptr, &interface_func_impl, &evaluated_args)?)
        }
    }

    fn get_interface_method_ptr(
        &mut self,
        instance: &InterfaceInvocation,
        interface_id: usize,
        method_id: usize,
    ) -> Result<OpaqueFunctionValue> {
        let find_interface_method_impl = ResolvedFunctionNode::externed(
            match instance {
                InterfaceInvocation::Instance(_) => "keid.find_virtual_interface_method",
                InterfaceInvocation::Static(_) => "keid.find_static_interface_method",
            },
            &[
                BasicType::Void.to_complex().to_reference(), // pointer
                BasicType::Int32.to_complex(),
                BasicType::Int32.to_complex(),
            ],
            Varargs::None,
            BasicType::Void.to_complex().to_reference(), // pointer,
        );
        let find_interface_method_ref = self.get_function_ref(&find_interface_method_impl)?;

        let interface_id_const = self
            .cpl
            .context
            .const_int(self.cpl.context.get_i32_type(), interface_id as _);
        let method_id_const = self
            .cpl
            .context
            .const_int(self.cpl.context.get_i32_type(), method_id as _);

        let args = match instance {
            InterfaceInvocation::Instance(instance) => {
                vec![instance.val, interface_id_const, method_id_const]
            }
            InterfaceInvocation::Static(ty) => {
                let ty = self
                    .cpl
                    .type_provider
                    .get_class_by_name(&GenericIdentifier::from_complex_type(ty))
                    .unwrap();
                vec![
                    self.cpl.class_info.get_abi_class_info_ptr(
                        &self.cpl.context,
                        &self.unit.mdl,
                        &ty,
                    ),
                    interface_id_const,
                    method_id_const,
                ]
            }
        };

        let method_ptr = self.emit(Insn::Call(
            find_interface_method_ref,
            find_interface_method_impl.as_llvm_type(self.cpl),
            args,
        ));
        Ok(method_ptr.to_function())
    }

    fn compile_new_call(&mut self, nc: &NewCall) -> Result<TypedValue> {
        self.loc(&nc.ty.loc);

        let class_instance = self.instantiate_object(self.resolve_type(&nc.ty.complex).unwrap())?;
        match &class_instance.ty {
            ComplexType::Basic(BasicType::Object(ident)) => {
                let class_impl = self.cpl.type_provider.get_class_by_name(ident).unwrap();

                // TODO: ensure all fields are initialized
                // TODO: call constructor if present
                for arg in &nc.args {
                    let field = self.resolve_class_member_ptr(
                        &class_instance,
                        &class_impl,
                        &arg.field_name,
                    )?;
                    let arg_value = match &arg.value {
                        Some(expr) => self.compile_expr(expr, Some(&field.get_type()))?,
                        None => {
                            let ident = self.resolve_ident(&arg.field_name)?;
                            self.load_local_var(&ident)?
                        }
                    };
                    field.store(Operator::Equals, self, arg_value)?;
                }

                Ok(class_instance)
            }
            _ => panic!(),
        }
    }

    fn instantiate_object(&mut self, object_type: ComplexType) -> Result<TypedValue> {
        match &object_type {
            ComplexType::Basic(BasicType::Object(_)) => {
                let class_type = self.resolve_type(&object_type)?;
                let ident = GenericIdentifier::from_complex_type(&class_type);

                let class_impl = match self.cpl.type_provider.get_class_by_name(&ident) {
                    Some(class_impl) => match class_impl.class_type {
                        ClassType::Class => class_impl,
                        ClassType::Struct => {
                            let struct_object =
                                self.emit(Insn::Alloca(object_type.as_llvm_type(self.cpl)));

                            // store the classinfo pointer
                            let class_info_global = self.cpl.class_info.get_abi_class_info_ptr(
                                &self.cpl.context,
                                &self.unit.mdl,
                                &class_impl,
                            );
                            let class_abi = self
                                .cpl
                                .context
                                .get_abi_class_data_type(self.cpl, &class_impl);

                            let class_info_local =
                                self.emit(Insn::GetElementPtr(struct_object, class_abi, 0));
                            self.emit(Insn::Store(class_info_global, class_info_local));

                            return Ok(TypedValue::new(object_type.clone(), struct_object));
                        }
                        ClassType::Interface => {
                            return Err(compiler_error!(
                                self,
                                "Cannot instantiate object with interface type `{}`",
                                ident.to_string()
                            ))
                        }
                        ClassType::Enum => panic!("enums must be a BasicType::Enum"),
                    },
                    // sometimes structs are reported to be objects
                    // as an awful workaround, we just call this function again with the write typings
                    None => {
                        return Err(compiler_error!(
                            self,
                            "[ER7] Could not resolve type `{}`",
                            object_type.to_string()
                        ))
                    }
                };

                // allocate the object
                let class_abi = self
                    .cpl
                    .context
                    .get_abi_class_data_type(self.cpl, &class_impl);
                let class_pointer = self.heap_allocate(class_abi, None)?;
                let mut class_instance = TypedValue {
                    ty: class_type,
                    val: class_pointer,
                };

                // store the classinfo pointer
                let class_info_global = self.cpl.class_info.get_abi_class_info_ptr(
                    &self.cpl.context,
                    &self.unit.mdl,
                    &class_impl,
                );
                let class_info_local = self.emit(Insn::GetElementPtr(class_pointer, class_abi, 0));
                self.emit(Insn::Store(class_info_global, class_info_local));

                // set the ref count to 0
                let const_one = self
                    .cpl
                    .context
                    .const_int(self.cpl.context.get_i64_type(), 0);
                let ref_count_ptr = self.emit(Insn::GetElementPtr(class_pointer, class_abi, 1));
                self.emit(Insn::Store(const_one, ref_count_ptr));

                let class_abi_ptr_type = self.cpl.context.get_pointer_type(class_abi);
                let class_pointer = self.emit(Insn::PointerCast(class_pointer, class_abi_ptr_type));
                class_instance.val = class_pointer;

                let register_object = &ResolvedFunctionNode::externed(
                    "keid.register_object",
                    &[BasicType::Void.to_complex().to_reference()],
                    Varargs::None,
                    BasicType::Void.to_complex(),
                );
                let register_object_ref = self.get_function_ref(register_object)?;
                self.call_function(
                    register_object_ref,
                    register_object,
                    &[class_instance.clone()],
                )?;

                // scope the class type
                self.try_scope(&class_instance)?;

                Ok(class_instance)
            }
            _ => panic!("not an object or struct: {}", object_type.to_string()),
        }
    }

    fn compile_func_call(
        &mut self,
        func_ref: OpaqueFunctionValue,
        callable: &ResolvedFunctionNode,
        args: &[TypedValue],
    ) -> Result<TypedValue> {
        let result = self.call_function(func_ref, callable, args)?;

        Ok(TypedValue {
            ty: callable.return_type.clone(),
            val: result,
        })
    }

    fn call_function(
        &mut self,
        func_ref: OpaqueFunctionValue,
        callable: &ResolvedFunctionNode,
        args: &[TypedValue],
    ) -> Result<OpaqueValue> {
        let args: Vec<TypedValue> = args.to_vec();

        // if the callable is an externed function, then we can't get its source
        // so we just let instrinsics be called from safe contexts since we guarantee their safety regardless
        if callable.module_id != usize::MAX && !self.is_unsafe() {
            let mods = self
                .cpl
                .type_provider
                .get_source_function(callable)
                .modifiers
                .clone();
            if mods.contains(&FunctionModifier::Unsafe) {
                return Err(compiler_error!(
                    self,
                    "Cannot call `unsafe` function `{}` without an `unsafe` block or the caller being an `unsafe` function",
                    callable.callable_name
                ));
            }

            if mods.contains(&FunctionModifier::Extern) {
                return Err(compiler_error!(
                    self,
                    "Cannot call `extern` function `{}` without an `unsafe` block or the caller being an `unsafe` function",
                    callable.callable_name
                ));
            }
        }

        let mut args = match callable.varargs {
            Varargs::Native => args
                .iter()
                .enumerate()
                .map(|(i, arg)| {
                    if i >= callable.params.len() {
                        Ok(arg.clone())
                    } else {
                        self.implicit_cast(arg.clone(), &callable.params[i])
                    }
                })
                .collect::<Result<Vec<_>>>()?,
            Varargs::None | Varargs::Array => args
                .iter()
                .enumerate()
                .take(callable.params.len())
                .map(|(i, arg)| self.implicit_cast(arg.clone(), &callable.params[i]))
                .collect::<Result<Vec<_>>>()?,
        };

        let is_extern = callable.module_id == usize::MAX || {
            let mods = self
                .cpl
                .type_provider
                .get_source_function(callable)
                .modifiers
                .clone();
            mods.contains(&FunctionModifier::Extern)
        };

        let (coerce_pointer, return_type, is_struct) =
            if callable.return_type.is_struct(&self.cpl.type_provider) {
                match &callable.return_type {
                    ComplexType::Basic(BasicType::Object(ident)) => {
                        if ident.name == "core::mem::Pointer" && is_extern {
                            // we need to coerce the returned pointer (usize-length) into a `core::mem::Pointer<T>`
                            (true, BasicType::USize.to_complex(), false)
                        } else {
                            (false, BasicType::Void.to_complex(), true)
                        }
                    }
                    _ => (false, BasicType::Void.to_complex(), true),
                }
            } else {
                (false, callable.return_type.clone(), false)
            };

        // if the function returns a struct,
        // allocate the memory for that struct and pass it in as the last arg
        let return_memory = if is_struct {
            let return_memory =
                self.emit(Insn::Alloca(callable.return_type.as_llvm_type(self.cpl)));
            let retmem_arg = TypedValue::new(
                ComplexType::Reference(Box::new(callable.return_type.clone())),
                return_memory,
            );
            args.push(retmem_arg);
            Some(return_memory)
        } else {
            None
        };

        let (arg_vals, arg_types): (Vec<OpaqueValue>, Vec<OpaqueType>) = args
            .into_iter()
            .map(|arg| {
                let arg_llvm_ty = arg.ty.as_llvm_type(self.cpl);
                match &arg.ty {
                    ComplexType::Basic(BasicType::Object(ident)) => {
                        if arg.ty.is_struct(&self.cpl.type_provider) {
                            // the Pointer struct is automatically converted
                            // to a system-native pointer type when invoking `extern` functions
                            if ident.name == "core::mem::Pointer" && is_extern {
                                let address_ptr =
                                    self.emit(Insn::GetElementPtr(arg.val, arg_llvm_ty, 1));
                                let address = self.emit(Insn::Load(
                                    address_ptr,
                                    BasicType::USize.as_llvm_type(self.cpl),
                                ));
                                (address, BasicType::USize.as_llvm_type(self.cpl))
                            } else {
                                (arg.val, arg_llvm_ty)
                            }
                        } else {
                            (arg.val, arg_llvm_ty)
                        }
                    }
                    _ => (arg.val, arg_llvm_ty),
                }
            })
            .unzip();

        let func_type = self.cpl.context.get_function_type(
            &arg_types,
            callable.varargs,
            return_type.as_llvm_type(self.cpl),
        );
        let call = self.emit(Insn::Call(func_ref, func_type, arg_vals));
        self.add_call_site_attributes(call, callable);

        // prevent recursively checking errors, or error-checking external functions
        if callable.module_id != usize::MAX
            && callable.external_name != "keid.check_unhandled_error"
            && callable.external_name != "keid.get_unhandled_error"
        // && !Self::get_hidden_function_names().contains(&callable.callable_name.as_str())
        {
            self.handle_unhandled_error(true)?;
        }

        let result = if is_struct {
            return_memory.unwrap()
        } else if coerce_pointer {
            let ptr_struct_ty = callable.return_type.as_llvm_type(self.cpl);
            let ptr_struct = self.emit(Insn::Alloca(ptr_struct_ty));
            let address_ptr = self.emit(Insn::GetElementPtr(ptr_struct, ptr_struct_ty, 1));
            self.emit(Insn::Store(call, address_ptr));
            ptr_struct
        } else {
            call
        };

        Ok(result)
    }
}
