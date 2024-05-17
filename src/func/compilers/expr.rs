use super::*;
use crate::{
    common::types::IntoOpaqueType,
    common::{
        types::{BasicType, ComplexType},
        *,
    },
    compiler::llvm::Insn,
    compiler_error, compiler_error_loc,
    func::{utils::FunctionCompilerUtils, *},
};

#[derive(Debug)]
pub struct ResolvedEnumMember {
    pub enum_impl: ResolvedEnumNode,
    pub variant_id: usize,
    pub data_type: Option<Vec<AnonymousStructField>>,
}

pub trait ExprCompiler {
    fn compile_expr(&mut self, expr: &Token<Expr>, type_hint: Option<&ComplexType>) -> Result<TypedValue>;
    fn compile_default_expr(&mut self, ty: &ComplexType) -> Result<TypedValue>;
    fn compile_reference_expr(&mut self, val: &TypedValue) -> Result<TypedValue>;
    fn compile_dereference_expr(&mut self, val: &TypedValue) -> Result<TypedValue>;
    fn compile_integer_literal_expr(&mut self, val: i64, type_hint: Option<&ComplexType>) -> Result<TypedValue>;
    fn compile_string_literal_expr(&mut self, str: &str) -> Result<TypedValue>;
    fn compile_sizeof_expr(&mut self, ty: &ComplexType) -> Result<TypedValue>;
    fn resolve_static_field_reference(&mut self, sfr: &Qualifier) -> Result<TypedValue>;
    fn resolve_enum_variant(&mut self, declaring_type: &GenericIdentifier, member: &str) -> Result<ResolvedEnumMember>;
    fn compile_new_enum_member(
        &mut self,
        declaring_type: &GenericIdentifier,
        member: &str,
        data: Option<Vec<NewCallField>>,
    ) -> Result<TypedValue>;
    fn compile_match_expr(&mut self, mtch: &MatchExpr, type_hint: Option<&ComplexType>) -> Result<TypedValue>;
    fn load_local_var(&mut self, var: &LocalVar) -> Result<TypedValue>;
}

impl<'a> ExprCompiler for FunctionCompiler<'a> {
    /// Returns a single scalar value loaded, denoted by the given AST expression node.
    /// For identifiers, field references, etc. the deferenced value is returned, not the pointer to the value.
    fn compile_expr(&mut self, expr: &Token<Expr>, type_hint: Option<&ComplexType>) -> Result<TypedValue> {
        self.loc(&expr.loc);
        Ok(match &expr.token {
            Expr::SignedIntLit(val) => self.compile_integer_literal_expr(*val, type_hint)?,
            Expr::StringLit(str) => self.compile_string_literal_expr(str)?,
            Expr::CharLit(ch) => {
                TypedValue::new(BasicType::Char.to_complex(), self.cpl.context.const_int(self.cpl.context.get_i32_type(), *ch as u64))
            }
            Expr::Ident(ident) => {
                let field_ref = self.resolve_static_field_reference(&Qualifier(vec![ident.clone()]))?;
                let container = TypedValueContainer(field_ref);
                container.load(self)?
            }
            Expr::Reference(expr) => {
                let compiled_expr = self.compile_expr(expr, None)?;
                self.compile_reference_expr(&compiled_expr)?
            }
            Expr::Dereference(expr) => {
                let compiled_expr = self.compile_expr(expr, None)?;
                self.compile_dereference_expr(&compiled_expr)?
            }
            Expr::FuncCall(func_call) => self.compile_static_func_call(&StaticFuncCall {
                owner: Qualifier(Vec::new()),
                call: func_call.clone(),
            })?,
            Expr::Member(member_expr) => {
                let mut members = member_expr.members.clone();

                let (mut current, skip) = if let Some(prefix) = &member_expr.prefix {
                    let (current, skip) = if prefix.0.len() == 1
                        && let Ok(local_ident) = self.resolve_ident(&prefix.0[0])
                    {
                        (self.load_local_var(&local_ident)?, 0)
                    } else {
                        let result = 'result: {
                            let imported_names = utils::lookup_import_map(&self.import_map, &prefix.to_string());
                            match &members[0].value.token {
                                Expr::FuncCall(call) => {
                                    match &call.callee.token {
                                        Expr::Ident(ident) => {
                                            for imported_name in imported_names {
                                                let full_name = format!("{}::{}", imported_name, ident.token.0);
                                                if self.cpl.type_provider.has_any_function_by_name(&full_name) {
                                                    break 'result Some(self.compile_static_func_call(&StaticFuncCall {
                                                        owner: prefix.clone(),
                                                        call: call.clone(),
                                                    })?);
                                                }
                                            }
                                        }
                                        _ => (),
                                    };
                                    break 'result None;
                                }
                                Expr::Ident(ident) => {
                                    for imported_name in imported_names {
                                        let mut parts = Vec::new();
                                        for part in imported_name.split("::") {
                                            parts.push(Token {
                                                token: Identifier(part.to_owned()),
                                                loc: prefix.get_location(),
                                            })
                                        }
                                        parts.push(ident.clone());

                                        if let Ok(sfr) = self.resolve_static_field_reference(&Qualifier(parts)) {
                                            break 'result Some(sfr);
                                        }
                                    }
                                    break 'result None;
                                }
                                x => unreachable!("{:#?}", x),
                            }
                        };
                        match result {
                            Some(result) => (result, 1),
                            None => {
                                let identifier = self.resolve_static_field_reference(prefix)?;
                                (identifier, 0)
                            }
                        }
                    };
                    if skip == 0 {
                        members[0].ty = MemberType::Class;
                    }
                    (current, skip)
                } else {
                    (self.compile_expr(&members[0].value, None)?, 1)
                };

                for next_member in members.iter().skip(skip) {
                    self.loc(&next_member.value.loc);

                    current = self.try_unbox_object(current)?;

                    match (next_member.ty, current.ty.clone()) {
                        (MemberType::Class, ComplexType::Array(element_type)) => match &next_member.value.token {
                            Expr::Ident(field_name) => {
                                if field_name.token.0 == "length" {
                                    let length_ptr = self.emit(Insn::GetElementPtr(
                                        current.val,
                                        self.cpl.context.get_abi_slice_type(element_type.as_llvm_type(self.cpl), &element_type.to_string()),
                                        1,
                                    ));

                                    let length = self.emit(Insn::Load(length_ptr, BasicType::USize.as_llvm_type(self.cpl)));
                                    current = TypedValue::new(BasicType::USize.to_complex(), length);
                                } else {
                                    return Err(compiler_error!(
                                        self,
                                        "No such field `{}` in type `{}`",
                                        field_name.token.0,
                                        current.ty.to_string()
                                    ));
                                }
                            }
                            Expr::FuncCall(_) => {
                                return Err(compiler_error!(self, "Cannot invoke method on array type `{}`", current.ty.to_string()))
                            }
                            x => unimplemented!("{:#?}", x),
                        },
                        (MemberType::Array, ComplexType::Array(_)) => match &next_member.value.token {
                            Expr::Subslice(subslice) => {
                                current = self.compile_subslice(&current, subslice)?;
                            }
                            _ => {
                                let index = self.compile_expr(&next_member.value, Some(&BasicType::USize.to_complex()))?;
                                self.assert_assignable_to(&index.ty, &BasicType::USize.to_complex())?;
                                current = self.load_array_element(&current, &index)?;
                            }
                        },
                        (MemberType::Class, ComplexType::Basic(_)) => {
                            let instance = self.autobox_primitive(current.clone())?;
                            let ident = match &instance.ty {
                                ComplexType::Basic(BasicType::Object(ident)) => ident,
                                ty => return Err(compiler_error!(self, "The `.` operator is forbidden on type `{}`", ty.to_string())),
                            };

                            let class_impl = match self.cpl.type_provider.get_class_by_name(ident) {
                                Some(class_impl) => match class_impl.class_type {
                                    ClassType::Interface => {
                                        match &next_member.value.token {
                                            Expr::FuncCall(fc) => {
                                                current = self.compile_instance_func_call(fc, &instance)?;
                                            }
                                            Expr::Ident(field) => {
                                                return Err(compiler_error!(
                                                    self,
                                                    "Could not resolve accessor `{}::{}`",
                                                    ident.to_string(),
                                                    field.token.0
                                                ))
                                            }
                                            x => unreachable!("{:?}", x),
                                        }

                                        continue;
                                    }
                                    _ => class_impl,
                                },
                                None => match self.cpl.type_provider.get_enum_by_name(ident) {
                                    Some(resolved_enum) => match &next_member.value.token {
                                        Expr::FuncCall(fc) => {
                                            current = self.compile_instance_func_call(fc, &instance)?;
                                            continue;
                                        }
                                        Expr::Ident(field_name) => {
                                            self.loc(&field_name.loc);
                                            if field_name.token.0 == "variant" {
                                                let length_ptr = self.emit(Insn::GetElementPtr(
                                                    current.val,
                                                    self.cpl.context.get_abi_enum_type_any_element(&self.cpl, &resolved_enum),
                                                    1,
                                                ));

                                                let length = self.emit(Insn::Load(length_ptr, self.cpl.context.get_i32_type()));
                                                current = TypedValue::new(BasicType::UInt32.to_complex(), length);
                                                continue;
                                            } else {
                                                let accessor = self.resolve_interface_accessor(&instance, ident, field_name)?;
                                                current = accessor.load(self)?;
                                                continue;
                                            }
                                        }
                                        x => unreachable!("{:?}", x),
                                    },
                                    None => return Err(compiler_error!(self, "[ER4] Could not resolve type `{}`", ident.to_string())),
                                },
                            };
                            match &next_member.value.token {
                                Expr::Ident(field_name) => {
                                    let member = self.resolve_class_member_ptr(&instance, &class_impl, field_name)?;
                                    current = member.load(self)?;
                                }
                                Expr::Unary(unary) => match &unary.value.token {
                                    Expr::Ident(field_name) => {
                                        let member = self.resolve_class_member_ptr(&instance, &class_impl, field_name)?;
                                        let expr = member.load(self)?;
                                        current = self.compile_unary_expr(unary.op, &expr)?;
                                    }
                                    Expr::FuncCall(fc) => {
                                        let call = self.compile_instance_func_call(fc, &instance)?;
                                        current = self.compile_unary_expr(unary.op, &call)?;
                                    }
                                    x => unreachable!("{:?}", x),
                                },
                                Expr::FuncCall(fc) => {
                                    current = self.compile_instance_func_call(fc, &instance)?;
                                }
                                x => unreachable!("{:?}", x),
                            }
                        }
                        (MemberType::Class, _) => {
                            return Err(compiler_error!(self, "[ER8] The `.` operator is forbidden on type `{}`", current.ty.to_string()))
                        }
                        (MemberType::Array, _) => {
                            return Err(compiler_error!(self, "[ER9] The `[]` operator is forbidden on type `{}`", current.ty.to_string()))
                        }
                        (x, y) => unreachable!("x = {:?}, y = {:?}", x, y),
                    }
                }

                current
            }
            Expr::Range(range) => {
                let obj =
                    self.instantiate_object(BasicType::Object(GenericIdentifier::from_name("core::collections::Range")).to_complex())?;
                let class_impl = self.cpl.type_provider.get_class_by_name(&GenericIdentifier::from_complex_type(&obj.ty)).unwrap();

                let start = self.compile_expr(&range.start, Some(&BasicType::Int32.to_complex()))?;
                let end = self.compile_expr(&range.end, Some(&BasicType::Int32.to_complex()))?;

                let start_ptr = self.resolve_class_member_ptr(&obj, &class_impl, &Identifier::from_string("start"))?;
                let end_ptr = self.resolve_class_member_ptr(&obj, &class_impl, &Identifier::from_string("end"))?;
                let current_ptr = self.resolve_class_member_ptr(&obj, &class_impl, &Identifier::from_string("current"))?;

                start_ptr.store(Operator::Equals, self, start.clone())?;
                end_ptr.store(Operator::Equals, self, end)?;
                current_ptr.store(Operator::Equals, self, start)?;

                obj
            }
            Expr::Null => {
                TypedValue::new(ComplexType::Basic(BasicType::Null), self.cpl.context.const_null_ptr(self.cpl.context.get_void_type()))
            }
            Expr::Psuedo(typed_value) => typed_value.clone(),
            Expr::New(new) => self.compile_new_call(new)?,
            Expr::NewArray(arr) => {
                let element_type = self.resolve_type(&arr.element_type.complex)?;
                let length = self.compile_expr(&arr.length, Some(&BasicType::USize.to_complex()))?;
                let initial_value = self.compile_expr(&arr.initial_value, Some(&element_type))?;

                self.compile_new_array(&element_type, &initial_value, &length)?
            }
            Expr::SpecifiedArray(arr) => {
                let element_type = self.resolve_type(&arr.element_type.complex)?;

                let const_length = self.cpl.context.const_int(BasicType::USize.as_llvm_type(self.cpl), arr.initial_values.len() as _);
                let const_null =
                    TypedValue::new(BasicType::Null.to_complex(), self.cpl.context.const_null(element_type.as_llvm_type(self.cpl)));
                let length = TypedValue::new(BasicType::USize.to_complex(), const_length);

                let array_value = self.compile_new_array(&element_type, &const_null, &length)?;
                for i in 0..arr.initial_values.len() {
                    let element_value = self.compile_expr(&arr.initial_values[i], Some(&element_type))?;
                    let const_i = TypedValue::new(
                        BasicType::USize.to_complex(),
                        self.cpl.context.const_int(BasicType::USize.as_llvm_type(self.cpl), i as _),
                    );
                    self.store_array_element(&array_value, &element_value, &const_i, true)?;
                }
                array_value
            }
            Expr::Logic(logic) => {
                let lhs = self.compile_expr(&logic.lhs, None)?;

                if logic.op == Operator::As {
                    let cast_target = match &logic.rhs.token {
                        Expr::CastTarget(target) => target,
                        _ => unreachable!(),
                    };
                    let cast_target = self.resolve_type(&cast_target.complex)?;
                    self.compile_cast(lhs, cast_target)?
                } else {
                    let mut rhs = self.compile_expr(&logic.rhs, Some(&lhs.ty))?;
                    // We never want to implicitly cast "null" in a logical expression because it holds special significance in this scenario.
                    // If we implicitly cast it to a nullable type, this will attempt to compare their nullability AND their inner equality should they both be non-null.
                    // Performing an explicit null check should NOT trigger an inner equality check.
                    // We also never want to implicitly cast the right-hand side of a null coalescing expression,
                    // since the nullability of the right-hand side determines whether or not the return value is nullable.
                    if rhs.ty != BasicType::Null.to_complex() && logic.op != Operator::NullCoalesce {
                        rhs = self.implicit_cast(rhs, &lhs.ty)?;
                    }
                    self.compile_logic_expr(lhs, logic.op, rhs)?
                }
            }
            Expr::BoolLit(value) => {
                let bool_val = self.cpl.context.const_int(BasicType::Bool.as_llvm_type(self.cpl), u64::from(*value));
                TypedValue::new(BasicType::Bool.to_complex(), bool_val)
            }
            Expr::Unary(unary) => {
                let expr = self.compile_expr(&unary.value, None)?;
                self.compile_unary_expr(unary.op, &expr)?
            }
            Expr::Default(default) => {
                let ty = self.resolve_type(&default.complex)?;
                self.compile_default_expr(&ty)?
            }
            Expr::SizeOf(size_of) => {
                let ty = self.resolve_type(&size_of.complex)?;
                self.compile_sizeof_expr(&ty)?
            }
            Expr::EnumWithData(enum_with_data) => {
                let declaring_type = self.resolve_type(&QualifiedType::from_qualifier(&enum_with_data.declaring_type).complex)?;
                let declaring_type = match declaring_type {
                    ComplexType::Basic(BasicType::Object(ident)) => ident,
                    _ => return Err(compiler_error!(self, "Type `{}` is not an enum type", declaring_type.to_string())),
                };
                self.compile_new_enum_member(&declaring_type, &enum_with_data.member.token.0, Some(enum_with_data.data.clone()))?
            }
            Expr::Match(mtch) => self.compile_match_expr(mtch, type_hint)?,
            _ => return Err(compiler_error!(self, "Unimplemented expression type")),
        })
    }

    fn compile_match_expr(&mut self, mtch: &MatchExpr, type_hint: Option<&ComplexType>) -> Result<TypedValue> {
        let value = self.compile_expr(&mtch.value, None)?;

        let mut match_result_types = Vec::new();

        let state = self.state.clone();
        llvm::set_eval_only(true);
        for branch in &mtch.branches {
            let mut temp_block = ScopeBlock::new(BlockType::Generic);
            match &branch.arg {
                MatchExprBranchArg::EnumWithData {
                    member,
                    ..
                } => {
                    let resolved = self.resolve_enum_variant(&GenericIdentifier::from_complex_type(&value.ty), &member.token.0)?;
                    match resolved.data_type {
                        Some(_) => {
                            for field in resolved.data_type.unwrap() {
                                temp_block.locals.push(LocalVar {
                                    name: field.name,
                                    value: TypedValue::new(field.ty, self.cpl.context.const_null_ptr(self.cpl.context.get_void_type())),
                                });
                            }
                        }
                        None => return Err(compiler_error_loc!(&member.loc, "Enum variant `{}` takes no associated data", member.token.0)),
                    }
                }
                _ => (),
            }

            self.state.block_stack.push(temp_block);
            match &branch.statement.token {
                Statement::Expr(expr) => {
                    let result = self.compile_expr(expr, type_hint)?;
                    match_result_types.push((result.ty, expr.loc.clone()));
                    self.state.block_stack.pop();
                }
                _ => {
                    let returns = self.compile_block(&[branch.statement.clone()]);
                    if !returns {
                        self.state.block_stack.pop();
                    }
                }
            }
        }
        llvm::set_eval_only(false);
        if !llvm::get_eval_only() {
            self.state = state;
        }

        for (ty, loc) in &match_result_types {
            for (other_ty, _) in &match_result_types {
                if !self.cpl.type_provider.is_assignable_to(ty, other_ty) {
                    return Err(compiler_error_loc!(loc, "Mismatched types `{}` and `{}`", ty.to_string(), other_ty.to_string()));
                }
            }
        }

        let mut prealloc_result =
            TypedValue::new(BasicType::Void.to_complex(), self.cpl.context.const_null_ptr(self.cpl.context.get_void_type()));
        if !match_result_types.is_empty() && match_result_types[0].0 != BasicType::Void.to_complex() {
            let allocated = self.emit(Insn::Alloca(match_result_types[0].0.as_llvm_type(self.cpl)));
            prealloc_result = TypedValue::new(match_result_types[0].0.clone(), allocated);
        }

        let final_rotated_parent = self.builder.create_block();
        for (i, branch) in mtch.branches.iter().enumerate() {
            let statement_block = self.builder.create_block();
            let rotated_parent = if i == mtch.branches.len() - 1 {
                final_rotated_parent.clone()
            } else {
                self.builder.create_block()
            };

            match &branch.arg {
                MatchExprBranchArg::Catchall(loc) => {
                    if i != mtch.branches.len() - 1 {
                        return Err(compiler_error_loc!(loc, "Catchall branch must be the last branch in the match"));
                    }

                    self.emit(Insn::Br(statement_block.as_val()));
                }
                MatchExprBranchArg::EnumWithData {
                    member,
                    ..
                }
                | MatchExprBranchArg::Enum(member) => {
                    if !matches!(value.ty, ComplexType::Basic(BasicType::Object(_))) {
                        return Err(compiler_error!(self, "Expecting value of enum type but received `{}`", value.ty.to_string()));
                    }
                    let resolved = self.resolve_enum_variant(&GenericIdentifier::from_complex_type(&value.ty), &member.token.0)?;
                    let test_variant_id_ptr = self.emit(Insn::GetElementPtr(value.val, value.ty.as_llvm_type(self.cpl), 1));
                    let test_variant_id = self.emit(Insn::Load(test_variant_id_ptr, self.cpl.context.get_i32_type()));
                    let real_variant_id_const = self.cpl.context.const_int(self.cpl.context.get_i32_type(), resolved.variant_id as _);
                    let are_equal = self.emit(Insn::ICmp(IntPredicate::LLVMIntEQ, test_variant_id, real_variant_id_const));

                    self.emit(Insn::CondBr(are_equal, statement_block.as_val(), rotated_parent.as_val()));
                }
                MatchExprBranchArg::Expr(expr) => {
                    let to_match = self.compile_expr(expr, Some(&value.ty))?;
                    self.assert_assignable_to(&to_match.ty, &value.ty)?;

                    let are_equal = self.compile_logic_expr(value.clone(), Operator::Equals, to_match)?;
                    self.assert_assignable_to(&are_equal.ty, &BasicType::Bool.to_complex())?;

                    self.emit(Insn::CondBr(are_equal.val, statement_block.as_val(), rotated_parent.as_val()));
                }
            }

            self.builder.append_block(&statement_block);
            self.state.push_block(ScopeBlock::new(BlockType::Generic)); // statement scope block

            match &branch.arg {
                MatchExprBranchArg::EnumWithData {
                    member,
                    ..
                } => {
                    let resolved = self.resolve_enum_variant(&GenericIdentifier::from_complex_type(&value.ty), &member.token.0)?;

                    let specific_variant_type =
                        self.cpl.context.get_abi_enum_type_specific_element(self.cpl, &resolved.enum_impl, resolved.variant_id);
                    let enum_ptr = self.emit(Insn::BitCast(value.val, self.cpl.context.get_pointer_type(specific_variant_type)));
                    for (i, field) in resolved.data_type.unwrap().into_iter().enumerate() {
                        let field_ptr = self.emit(Insn::GetElementPtr(enum_ptr, specific_variant_type, (i + 2) as u32)); // ID 2 to offset the classinfo and variant ID
                        self.state.get_current_block_mut().locals.push(LocalVar {
                            name: field.name,
                            value: TypedValue::new(field.ty, field_ptr),
                        });
                    }
                }
                _ => (),
            }

            match &branch.statement.token {
                Statement::Expr(expr) => {
                    let result = self.compile_expr(expr, type_hint)?;
                    self.copy(&result, &prealloc_result)?;

                    self.pop_block()?;
                    self.emit(Insn::Br(final_rotated_parent.as_val()));
                }
                _ => {
                    let returns = self.compile_block(&[branch.statement.clone()]);
                    if !returns {
                        self.pop_block()?;
                        self.emit(Insn::Br(final_rotated_parent.as_val()));
                    }
                }
            }

            self.builder.append_block(&rotated_parent);
        }

        let loaded_result = match prealloc_result.ty {
            ComplexType::Basic(BasicType::Void) => prealloc_result.val,
            _ => TypedValueContainer(prealloc_result.clone()).load(self)?.val,
        };
        Ok(TypedValue::new(prealloc_result.ty, loaded_result))
    }

    /// Returns a pointer to the local identifier, static field, or enum resolved by this function.
    fn resolve_static_field_reference(&mut self, sfr: &Qualifier) -> Result<TypedValue> {
        self.loc(&sfr.get_location());

        if sfr.0.len() == 1 {
            match self.resolve_ident(&sfr.0[0]) {
                Ok(var) => return Ok(var.value),
                Err(_) => (),
            }
        }

        let field_name = sfr.to_string();
        let imports = utils::lookup_import_map(&self.import_map, &field_name);
        for import in &imports {
            match self.cpl.type_provider.get_static_field_by_name(import) {
                Some(field) => {
                    let global = self.unit.mdl.get_or_extern_global(&GlobalVariable {
                        name: field.external_name.clone(),
                        ty: field.ty.as_llvm_type(self.cpl),
                    });
                    return Ok(TypedValue::new(field.ty.clone(), global));
                }
                None => (),
            }
        }

        if sfr.0.len() > 1 {
            let declaring_type = Qualifier(sfr.0[0..sfr.0.len() - 1].to_vec());
            let enum_name = GenericIdentifier::from_name(&declaring_type.to_string());
            let member = sfr.0[sfr.0.len() - 1].token.0.clone();
            match self.cpl.type_provider.get_enum_by_name(&enum_name) {
                Some(_) => return self.compile_new_enum_member(&enum_name, &member, None),
                None => {
                    let namespaced_name = format!("{}::{}", self.get_source_function().namespace_name, declaring_type.to_string());
                    let enum_name = GenericIdentifier::from_name(&namespaced_name);
                    match self.cpl.type_provider.get_enum_by_name(&enum_name) {
                        Some(_) => return self.compile_new_enum_member(&enum_name, &member, None),
                        None => (),
                    }
                }
            };
        }

        return Err(compiler_error!(self, "No such enum member, static field, or local identifier `{}`", field_name));
    }

    fn resolve_enum_variant(&mut self, declaring_type: &GenericIdentifier, member: &str) -> Result<ResolvedEnumMember> {
        let enum_impl = match self.cpl.type_provider.get_enum_by_name(declaring_type) {
            Some(enum_impl) => enum_impl,
            None => {
                let namespaced_name = format!("{}::{}", self.get_source_function().namespace_name, declaring_type.name);
                let namespaced_name = GenericIdentifier::from_name_with_args(&namespaced_name, &declaring_type.generic_args);
                match self.cpl.type_provider.get_enum_by_name(&namespaced_name) {
                    Some(enum_impl) => enum_impl,
                    None => return Err(compiler_error!(self, "No such enum type `{}`", declaring_type.to_string())),
                }
            }
        };
        match enum_impl.elements.iter().enumerate().find(|element| element.1.name == member) {
            Some(member) => Ok(ResolvedEnumMember {
                enum_impl: enum_impl.clone(),
                variant_id: member.0,
                data_type: member.1.data.clone(),
            }),
            None => Err(compiler_error!(self, "No such enum member `{}` in type `{}`", member, declaring_type.to_string())),
        }
    }

    fn compile_new_enum_member(
        &mut self,
        declaring_type: &GenericIdentifier,
        member: &str,
        data: Option<Vec<NewCallField>>,
    ) -> Result<TypedValue> {
        let resolved = self.resolve_enum_variant(declaring_type, member)?;
        if data.is_none()
            && let Some(data_ty) = resolved.data_type
        {
            return Err(compiler_error!(
                self,
                "Enum member `{}::{}` required associated data type `{}` but no data was provided",
                declaring_type.name,
                member,
                BasicType::AnonymousStruct(data_ty).to_string()
            ));
        }
        if resolved.data_type.is_none() && data.is_some() {
            return Err(compiler_error!(self, "Enum member `{}::{}` has no associated data", declaring_type.name, member,));
        }

        let any_variant_type = self.cpl.context.get_abi_enum_type_any_element(self.cpl, &resolved.enum_impl);
        let enum_ptr = self.emit(Insn::Alloca(any_variant_type));

        // store the classinfo pointer
        let class_info_global = self.cpl.class_info.get_abi_class_info_ptr(
            &self.cpl.context,
            &self.unit.mdl,
            &ClassInfoData::from_resolved_enum(&self.cpl.type_provider, &resolved.enum_impl),
        );
        let class_info_local = self.emit(Insn::GetElementPtr(enum_ptr, any_variant_type, 0));
        self.emit(Insn::Store(class_info_global, class_info_local));

        let variant_id_const = self.cpl.context.const_int(self.cpl.context.get_i32_type(), resolved.variant_id as _);
        let variant_id_ptr = self.emit(Insn::GetElementPtr(enum_ptr, any_variant_type, 1));

        self.emit(Insn::Store(variant_id_const, variant_id_ptr));

        if let Some(data) = data {
            if let Some(data_ref) = &resolved.enum_impl.elements[resolved.variant_id].data {
                let specific_variant_type =
                    self.cpl.context.get_abi_enum_type_specific_element(self.cpl, &resolved.enum_impl, resolved.variant_id);
                let enum_ptr = self.emit(Insn::BitCast(enum_ptr, self.cpl.context.get_pointer_type(specific_variant_type)));

                for field in &data {
                    let (field_offset, corresponding_field) =
                        match data_ref.iter().enumerate().find(|e| e.1.name == field.field_name.token.0) {
                            Some(field) => field,
                            None => {
                                return Err(compiler_error!(
                                    self,
                                    "Enum variant `{}` associated data has no field `{}`",
                                    resolved.enum_impl.elements[resolved.variant_id].name,
                                    field.field_name.token.0
                                ))
                            }
                        };
                    let field_ptr = self.emit(Insn::GetElementPtr(
                        enum_ptr,
                        specific_variant_type,
                        (2 + field_offset) as u32, // offset 2 to accomodate for the classinfo pointer and variant ID
                    ));
                    let arg_value = match &field.value {
                        Some(expr) => self.compile_expr(expr, Some(&corresponding_field.ty))?,
                        None => {
                            let ident = self.resolve_ident(&field.field_name)?;
                            self.load_local_var(&ident)?
                        }
                    };
                    self.store(Operator::Equals, arg_value, &TypedValue::new(corresponding_field.ty.clone(), field_ptr))?;
                }
            } else {
                return Err(compiler_error!(
                    self,
                    "Enum variant `{}` takes no data",
                    resolved.enum_impl.elements[resolved.variant_id].name
                ));
            }
        } else if resolved.enum_impl.elements[resolved.variant_id].data.is_some() {
            return Err(compiler_error!(
                self,
                "Enum variant `{}` takes associated data",
                resolved.enum_impl.elements[resolved.variant_id].name,
            ));
        }

        Ok(TypedValue::new(BasicType::Object(declaring_type.clone()).to_complex(), enum_ptr))
    }

    fn compile_default_expr(&mut self, ty: &ComplexType) -> Result<TypedValue> {
        let result = match ty {
            ComplexType::Basic(basic) => match basic {
                BasicType::Object(ident) => {
                    let (is_classlike, fields) = match self.cpl.type_provider.get_class_by_name(ident) {
                        Some(class) => (matches!(class.class_type, ClassType::Class | ClassType::Interface), class.fields),
                        None => match self.cpl.type_provider.get_enum_by_name(ident) {
                            Some(_) => (true, Vec::new()),
                            None => panic!(),
                        },
                    };
                    if is_classlike {
                        let resolved_interface_impls = self.cpl.type_provider.get_resolved_interface_impls(ident);
                        for resolved_interface_impl in resolved_interface_impls {
                            let source_interface = self.cpl.type_provider.get_source_interface_impl(&resolved_interface_impl);
                            if source_interface.interface_name == "core::object::Default" {
                                let interface_id =
                                    self.cpl.type_provider.get_resolved_interface_id(&GenericIdentifier::from_name_with_args(
                                        &source_interface.interface_name,
                                        &resolved_interface_impl.interface_generic_impls,
                                    ));
                                let create_default_ptr =
                                    self.get_interface_method_ptr(&InterfaceInvocation::Static(ty.clone()), interface_id, 0)?;
                                let callable = self
                                    .cpl
                                    .type_provider
                                    .get_function_by_name(
                                        &GenericIdentifier::from_name_with_args("core::object::Default::default", &[]),
                                        &[],
                                    )
                                    .unwrap();
                                let result = self.call_function(create_default_ptr, &callable, &[])?;
                                return Ok(TypedValue::new(ty.clone(), result));
                            }
                        }

                        return Err(compiler_error!(self, "Type `{}` does not implement `core::object::Default`", ident.to_string()));
                    } else {
                        let struct_ty = basic.as_llvm_type(self.cpl);
                        let obj = self.emit(Insn::Alloca(struct_ty));
                        for i in 0..fields.len() {
                            let ptr = self.emit(Insn::GetElementPtr(obj, struct_ty, (i + 1) as _)); // +1 to offset the classinfo pointer

                            let default_value = self.compile_default_expr(&fields[i].ty)?;
                            self.copy(&default_value, &TypedValue::new(default_value.ty.clone(), ptr))?;
                        }
                    }

                    let ty = basic.as_llvm_type(self.cpl);
                    self.cpl.context.const_int(ty, 0)
                }
                _ => {
                    let ty = basic.as_llvm_type(self.cpl);
                    self.cpl.context.const_int(ty, 0)
                }
            },
            _ => panic!(),
        };
        Ok(TypedValue::new(ty.clone(), result))
    }

    fn compile_reference_expr(&mut self, val: &TypedValue) -> Result<TypedValue> {
        Ok(match &val.ty {
            ComplexType::Array(element_type) => {
                let slice_type = self.cpl.context.get_abi_slice_type(element_type.as_llvm_type(self.cpl), &element_type.to_string());
                let array_ptr_ptr = self.emit(Insn::GetElementPtr(val.val, slice_type, 2)); // element 2 is the pointer to the heap data
                let array_data_type =
                    self.cpl.context.get_abi_array_data_type(element_type.as_llvm_type(self.cpl), &element_type.to_string());
                let array_ptr = self.emit(Insn::Load(array_ptr_ptr, self.cpl.context.get_pointer_type(array_data_type)));
                let data_ptr_ptr = self.emit(Insn::GetElementPtr(array_ptr, array_data_type, 1)); // functionally this is like a `T*`, a pointer to an array
                let data_ptr = self.emit(Insn::Load(data_ptr_ptr, BasicType::USize.as_llvm_type(self.cpl)));

                let ty =
                    BasicType::Object(GenericIdentifier::from_name_with_args("core::mem::Pointer", &[*element_type.clone()])).to_complex();
                let pointer_struct = self.instantiate_object(ty)?;
                let address_ptr = self.emit(Insn::GetElementPtr(pointer_struct.val, pointer_struct.ty.as_llvm_type(self.cpl), 1));
                self.emit(Insn::Store(data_ptr, address_ptr));

                pointer_struct
            }
            ComplexType::Basic(BasicType::Object(ident)) => {
                let struct_type = match self.cpl.type_provider.get_class_by_name(ident) {
                    Some(class_impl) => match class_impl.class_type {
                        ClassType::Class | ClassType::Interface => self.cpl.context.get_abi_class_data_type(self.cpl, &class_impl),
                        ClassType::Struct => self.cpl.context.get_abi_class_data_type(self.cpl, &class_impl),
                        ClassType::Enum => unreachable!(),
                    },
                    None => match self.cpl.type_provider.get_enum_by_name(ident) {
                        Some(enum_impl) => self.cpl.context.get_abi_enum_type_any_element(self.cpl, &enum_impl),
                        None => unreachable!(),
                    },
                };

                // take a pointer to the first element of the struct
                let data_ptr = self.emit(Insn::GetElementPtr(val.val, struct_type, 0));

                let ty = BasicType::Object(GenericIdentifier::from_name_with_args("core::mem::Pointer", &[val.ty.clone()])).to_complex();
                let pointer_struct = self.instantiate_object(ty)?;
                let address_ptr = self.emit(Insn::GetElementPtr(pointer_struct.val, pointer_struct.ty.as_llvm_type(self.cpl), 1));
                self.emit(Insn::Store(data_ptr, address_ptr));

                pointer_struct
            }
            ComplexType::Nullable(_) => {
                // take a pointer to the first element of the nullable struct
                let data_ptr = self.emit(Insn::GetElementPtr(val.val, val.ty.as_llvm_type(&self.cpl), 0));

                let ty = BasicType::Object(GenericIdentifier::from_name_with_args("core::mem::Pointer", &[val.ty.clone()])).to_complex();
                let pointer_struct = self.instantiate_object(ty)?;
                let address_ptr = self.emit(Insn::GetElementPtr(pointer_struct.val, pointer_struct.ty.as_llvm_type(self.cpl), 1));
                self.emit(Insn::Store(data_ptr, address_ptr));

                pointer_struct
            }
            ComplexType::Basic(basic) => {
                let data_ptr = self.emit(Insn::Alloca(basic.as_llvm_type(&self.cpl)));
                self.copy(val, &TypedValue::new(basic.clone().to_complex(), data_ptr))?;

                let ty = BasicType::Object(GenericIdentifier::from_name_with_args("core::mem::Pointer", &[basic.clone().to_complex()]))
                    .to_complex();
                let pointer_struct = self.instantiate_object(ty)?;
                let address_ptr = self.emit(Insn::GetElementPtr(pointer_struct.val, pointer_struct.ty.as_llvm_type(self.cpl), 1));
                self.emit(Insn::Store(data_ptr, address_ptr));

                pointer_struct
            }
            ty => return Err(compiler_error!(self, "Cannot take reference to type `{}`", ty.to_string())),
        })
    }

    fn compile_dereference_expr(&mut self, val: &TypedValue) -> Result<TypedValue> {
        if !self.is_unsafe() {
            return Err(compiler_error!(self, "Cannot peform unsafe operation `deref` without an `unsafe` block"));
        }

        Ok(match &val.ty {
            ComplexType::Basic(BasicType::Object(ident)) => {
                if ident.name == "core::mem::Pointer" {
                    let ty = self.resolve_type(&ident.generic_args[0])?; // deref Pointer<T> -> T
                    let addr_ptr = self.emit(Insn::GetElementPtr(val.val, val.ty.as_llvm_type(self.cpl), 1)); // pointer to the `address` field in the Pointer<T> struct
                    let addr_int = self.emit(Insn::Load(addr_ptr, BasicType::USize.as_llvm_type(self.cpl))); // this Load loads the address stored in the Pointer<T> struct
                    let addr_ptr = self.emit(Insn::IntToPtr(addr_int, ty.clone().to_reference().as_llvm_type(self.cpl)));
                    let val = if ty.is_struct(&self.cpl.type_provider) {
                        let dest = self.emit(Insn::Alloca(ty.as_llvm_type(&self.cpl)));
                        let const_size = self.cpl.context.const_int(
                            self.cpl.context.get_isize_type(),
                            self.cpl.context.target.get_type_size(ty.as_llvm_type(&self.cpl)),
                        );
                        self.emit(Insn::Memmove(addr_ptr, dest, const_size)); // copy the bytes from the pointer to the local stack
                        dest
                    } else {
                        // this Load derefences the pointer
                        self.emit(Insn::Load(addr_ptr, ty.as_llvm_type(self.cpl)))
                    };

                    TypedValue::new(ty, val)
                } else {
                    return Err(compiler_error!(self, "Cannot dereference non-reference type `{}`", val.ty.to_string()));
                }
            }
            _ => return Err(compiler_error!(self, "Cannot dereference non-reference type `{}`", val.ty.to_string())),
        })
    }

    fn compile_integer_literal_expr(&mut self, val: i64, type_hint: Option<&ComplexType>) -> Result<TypedValue> {
        let ty = match type_hint {
            Some(ComplexType::Basic(basic) | ComplexType::Nullable(box ComplexType::Basic(basic))) => match basic {
                BasicType::Bool
                | BasicType::Void
                | BasicType::Object {
                    ..
                } => &BasicType::Int32,
                numeric => numeric,
            },
            _ => &BasicType::Int32,
        };
        let llvm_type = ty.as_llvm_type(self.cpl);
        Ok(TypedValue::new(ty.clone().to_complex(), self.cpl.context.const_int(llvm_type, val as u64)))
    }

    fn compile_string_literal_expr(&mut self, str: &str) -> Result<TypedValue> {
        let string_global_name = format!("__strconst.object('{}')", str);
        if let Some(global_ref) = self.cpl.context.get_global(&string_global_name) {
            let string_global = self.unit.mdl.get_or_extern_global(global_ref);
            return Ok(TypedValue::new(
                BasicType::Object(GenericIdentifier::from_name("core::string::String")).to_complex(),
                string_global,
            ));
        }

        let chty = BasicType::Char.as_llvm_type(&self.cpl);
        let mut char_values = Vec::with_capacity(str.len());
        for ch in str.chars() {
            char_values.push(self.cpl.context.const_int(chty, ch as u64));
        }
        let char_array = self.cpl.context.const_array(chty, &char_values);
        let char_array_type = self.cpl.context.get_type_of_value(char_array);
        let char_array_global =
            self.unit.mdl.create_global(&mut self.cpl.context, &format!("__strconst.array('{}')", str), char_array_type);
        self.unit.mdl.initialize_global(char_array_global, char_array);

        let char_array_meta_type = self.cpl.context.get_abi_array_data_type(chty, "char");
        let const_zero = self.cpl.context.const_int(BasicType::USize.as_llvm_type(&self.cpl), 0);
        let const_length = self.cpl.context.const_int(BasicType::USize.as_llvm_type(&self.cpl), str.len() as u64);
        let char_array_meta = self.cpl.context.const_struct(char_array_meta_type, &mut [const_zero, char_array_global]);
        let char_array_meta_global =
            self.unit.mdl.create_global(&mut self.cpl.context, &format!("__strconst.data('{}')", str), char_array_meta_type);
        self.unit.mdl.initialize_global(char_array_meta_global, char_array_meta);

        let char_slice_type = self.cpl.context.get_abi_slice_type(BasicType::Char.as_llvm_type(&self.cpl), "char");
        let char_slice = self.cpl.context.const_struct(char_slice_type, &mut [const_zero, const_length, char_array_meta_global]);

        let string_class = self.cpl.type_provider.get_class_by_name(&GenericIdentifier::from_name("core::string::String")).unwrap();
        let string_type = self.cpl.context.get_abi_class_data_type(&self.cpl, &string_class);
        let string_classinfo = self.cpl.class_info.get_abi_class_info_ptr(
            &self.cpl.context,
            &self.unit.mdl,
            &ClassInfoData::from_resolved_class(&self.cpl.type_provider, &string_class),
        );
        let string = self.cpl.context.const_struct(string_type, &mut [string_classinfo, const_zero, char_slice]);
        let string_global = self.unit.mdl.create_global(&mut self.cpl.context, &string_global_name, string_type);
        self.unit.mdl.initialize_global(string_global, string);

        Ok(TypedValue::new(BasicType::Object(GenericIdentifier::from_name("core::string::String")).to_complex(), string_global))
    }

    fn compile_sizeof_expr(&mut self, ty: &ComplexType) -> Result<TypedValue> {
        let type_size = self.cpl.context.target.get_type_size(ty.as_llvm_type(self.cpl));
        let size_const = self.cpl.context.const_int(BasicType::USize.as_llvm_type(self.cpl), type_size);
        Ok(TypedValue::new(BasicType::USize.to_complex(), size_const)) // typeof(T): usize
    }

    fn load_local_var(&mut self, var: &LocalVar) -> Result<TypedValue> {
        let val = if var.value.ty.is_struct(&self.cpl.type_provider) {
            var.value.val
        } else {
            self.emit(Insn::Load(var.value.val, var.value.ty.as_llvm_type(self.cpl)))
        };
        Ok(TypedValue {
            ty: var.value.ty.clone(),
            val,
        })
    }
}
