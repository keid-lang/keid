use std::collections::HashMap;

use super::*;
use crate::{common::*, compiler_error, primitive_types};

pub enum ScopeChange {
    Inside,
    Outside,
}

pub trait FunctionCompilerUtils {
    fn assert_assignable_to(&self, child: &ComplexType, parent: &ComplexType) -> Result<()>;
    fn try_change_scope(&mut self, object: &TypedValue, scope_change: ScopeChange) -> Result<bool>;
    fn parse_generic_args(&mut self, generics: &Option<GenericArgs>) -> Result<Vec<ComplexType>>;

    fn get_array_element_ptr(&mut self, slice: &TypedValue, index: &TypedValue) -> Result<TypedValue>;
    fn load_array_element(&mut self, slice: &TypedValue, index: &TypedValue) -> Result<TypedValue>;
    fn store_array_element(&mut self, array: &TypedValue, value: &TypedValue, index: &TypedValue, was_null: bool) -> Result<()>;

    /// Copies and stores value `src` into container `dest`.
    ///
    /// The difference between [`FunctionCompilerUtils::copy`] and [`FunctionCompilerUtils::store`] is that `store` performs reference counting --
    /// `src` gets an additional reference and `dest` loses a reference.
    fn store(&mut self, op: Operator, src: TypedValue, dest: &TypedValue) -> Result<()>;

    /// Copies value `src` into container `dest`.
    /// Generally, `src` is a value type and `dest` is an LLVM pointer to the same value type (i.e. the result of [`Insn::GetElementPtr`]).
    ///
    /// If `src` is a pointer to a complex structure (such as a nullable type),
    /// then it is expected that `dest` is also a pointer to the same type.
    /// Each individual field of the `src` structure is copied to its repective field in `dest`.
    ///
    /// Implicit casting is performed prior to copying; see [`FunctionCompilerUtils::implicit_cast`] for more information.
    /// The returned value is the result of the the call to `implicit_cast`.
    fn copy(&mut self, src: &TypedValue, dest: &TypedValue) -> Result<TypedValue>;

    /// Attempts to implicitly cast the type of `src` to `dest_type` via the rules of implicit casting.
    /// An implicit cast is guaranteed to be safe.
    ///   1. If `src` is a [`BasicType::Null`] value and `dest_type` is a complex nullable type (e.g. `?i32`),
    /// then the untyped `null` value is implicitly cast to the complex nullable type.
    ///   2. If `src` is an object type and `dest_type` is any superclass or implemented interface type of the object,
    /// then the type of `src` becomes `dest_type` without any change to the value held.
    ///   3. If `src` is a numerical type and `dest_type` is an equivalent numerical type (i.e. similarly integral or decimal)
    /// but with more bits than `src` (e.g. `src` is an `i16` and `dest_type` is an `i32`) then the value is length-extended
    /// and the type is implicitly cast to the larger numerical type.
    fn implicit_cast(&mut self, src: TypedValue, dest_type: &ComplexType) -> Result<TypedValue>;

    /// Attempts to cast an object value (`src`)'s type to a superclass or superinterface (`parent`) statically.
    ///
    /// This function does not generate any LLVM instructions.
    /// The object's type is checked and upcasted at compile time statically.
    /// If `parent` specifies a superclass or superinterface that `src` does not descend from,
    /// the returned [`Result<TypedValue>`] is an error.
    /// Otherwise, it's a [`TypedValue`] that has the exact same value as `src` but with the type of `parent`.
    ///
    /// If the parent type has generic arguments, then `generic_args` must be supplied and are checked as well.
    /// To upcast to a superclass or superinterface that has generic arguments but you do not want to check
    /// the compatability of generic arguments, passing [`None`] will only evaluate the type name and disregard generics.
    ///
    /// If `src` is a primitive type and `parent` is `core::object::Object`, then the upcast will result in an autoboxing operation.
    ///
    /// If no upcast could be made, `None` is returned, unless `src` is equivalent to `parent`, in which case the `src` is returned.
    fn upcast(&mut self, src: TypedValue, parent: &str, generic_args: Option<Vec<ComplexType>>) -> Result<Option<TypedValue>>;

    fn autobox_primitive(&mut self, primitive: TypedValue) -> Result<TypedValue>;
    fn handle_unhandled_error(&mut self, check: bool) -> Result<()>;
    fn return_null(&mut self) -> Result<()>;

    fn box_object(&mut self, value: TypedValue) -> Result<TypedValue>;
    fn try_unbox_object(&mut self, value: TypedValue) -> Result<TypedValue>;
    fn heap_allocate(&mut self, ty: OpaqueType, count: Option<OpaqueValue>) -> Result<OpaqueValue>;
}

impl<'a> FunctionCompilerUtils for FunctionCompiler<'a> {
    fn box_object(&mut self, value: TypedValue) -> Result<TypedValue> {
        let box_ident = GenericIdentifier::from_name_with_args("core::object::Box", &[value.ty.clone()]);
        let boxed = self.instantiate_object(BasicType::Object(box_ident.clone()).to_complex())?;
        let type_root = self.cpl.type_provider.get_class_by_name(&box_ident).unwrap();
        let element_ptr = self.resolve_class_member_ptr(
            &boxed,
            &type_root,
            &Token {
                loc: TokenLocation {
                    start: 0,
                    end: 0,
                },
                token: Identifier("element".to_owned()),
            },
        )?;
        element_ptr.store(Operator::Equals, self, value)?;
        Ok(boxed)
    }

    fn try_unbox_object(&mut self, value: TypedValue) -> Result<TypedValue> {
        match &value.ty {
            ComplexType::Basic(BasicType::Object(ident)) => {
                if ident.name == "core::object::Box" {
                    let type_root = self.cpl.type_provider.get_class_by_name(ident).unwrap();
                    let element_ptr = self.resolve_class_member_ptr(
                        &value,
                        &type_root,
                        &Token {
                            loc: TokenLocation {
                                start: 0,
                                end: 0,
                            },
                            token: Identifier("element".to_owned()),
                        },
                    )?;
                    return Ok(element_ptr.load(self)?);
                }
            }
            _ => (),
        }

        Ok(value)
    }

    fn assert_assignable_to(&self, child: &ComplexType, parent: &ComplexType) -> Result<()> {
        if !self.cpl.type_provider.is_assignable_to(child, parent) {
            Err(compiler_error!(self, "Expected `{}` but received `{}`", parent.to_string(), child.to_string()))
        } else {
            Ok(())
        }
    }

    /// Given a **pointer** to an object type or nullable object type, load the object pointer at the provided pointer, and change its scope accordingly.
    fn try_change_scope(&mut self, _object: &TypedValue, _scope_change: ScopeChange) -> Result<bool> {
        // let scope_block = self.builder.create_block();
        // let rotated_parent_block = self.builder.create_block();

        // let (object_val, rotate) = match &object.ty {
        //     ComplexType::Nullable(box ComplexType::Basic(BasicType::Object(ref ident))) => {
        //         let nullability_ptr = self.emit(Insn::GetElementPtr(object.val, object.ty.as_llvm_type(self.cpl), 1)); // nullable type nullability
        //         let nullability = self.emit(Insn::Load(nullability_ptr, self.cpl.context.get_i8_type()));
        //         let const_zero = self.cpl.context.const_int(self.cpl.context.get_i8_type(), 0);
        //         let is_null = self.emit(Insn::ICmp(IntPredicate::LLVMIntEQ, nullability, const_zero));

        //         // if the value is null, then skip the `scope_block`
        //         self.emit(Insn::CondBr(is_null, rotated_parent_block.as_val(), scope_block.as_val()));

        //         self.builder.append_block(&scope_block);

        //         let object_ptr = self.emit(Insn::GetElementPtr(object.val, object.ty.as_llvm_type(self.cpl), 0)); // nullable type value
        //         (self.emit(Insn::Load(object_ptr, BasicType::Object(ident.clone()).as_llvm_type(self.cpl))), true)
        //     }
        //     ComplexType::Basic(BasicType::Object(ident)) => {
        //         // keid.unscope only unscopes if the object type is a class type
        //         // if we know the type to be a concrete struct type at compile time,
        //         // we can omit the keid.unscope call since it will have no effect at runtime
        //         match self.cpl.type_provider.get_class_by_name(ident) {
        //             Some(class) => {
        //                 if class.class_type == ClassType::Struct || class.class_type == ClassType::Enum {
        //                     return Ok(false);
        //                 }
        //             }
        //             None => match self.cpl.type_provider.get_enum_by_name(ident) {
        //                 Some(_) => return Ok(false),
        //                 None => (),
        //             },
        //         }

        //         (self.emit(Insn::Load(object.val, BasicType::Object(ident.clone()).as_llvm_type(self.cpl))), false)
        //     }
        //     _ => return Ok(false),
        // };

        // let func_name = match scope_change {
        //     ScopeChange::Inside => "keid.scope",
        //     ScopeChange::Outside => "keid.unscope",
        // };
        // let scope_impl = ResolvedFunctionNode::externed(func_name, &[BasicType::Void.to_complex().to_reference()], Varargs::None, BasicType::Void.to_complex());
        // let scope_func = self.get_function_ref(&scope_impl)?;
        // // TODO: fix this
        // // self.emit(Insn::Call(scope_func, scope_impl.as_llvm_type(self.cpl), vec![object_val]));

        // if rotate {
        //     self.emit(Insn::Br(rotated_parent_block.as_val()));
        //     self.builder.append_block(&rotated_parent_block);
        // }

        // Ok(true)
        Ok(false)
    }

    fn parse_generic_args(&mut self, generics: &Option<GenericArgs>) -> Result<Vec<ComplexType>> {
        generics
            .as_ref()
            .map(|generics| generics.args.iter().map(|qual| self.resolve_type(&qual.complex)).collect::<Result<_>>())
            .unwrap_or_else(|| Ok(Vec::new()))
    }

    fn store(&mut self, op: Operator, mut src: TypedValue, dest: &TypedValue) -> Result<()> {
        match &dest.ty {
            ComplexType::Basic(BasicType::Object(ident)) => {
                // convert assignment of T -> Box<T> if needed
                if ident.name == "core::object::Box" {
                    match &src.ty {
                        ComplexType::Basic(BasicType::Object(ident)) => {
                            if ident.name != "core::object::Box" {
                                src = self.box_object(src)?;
                            }
                        }
                        _ => (),
                    }
                } else {
                    // convert assignment of Box<T> -> T if needed
                    src = self.try_unbox_object(src)?;
                }
            }
            _ => (),
        }

        if !self.cpl.type_provider.is_assignable_to(&src.ty, &dest.ty) {
            Err(compiler_error!(self, "Cannot assign value of type `{}` to `{}`", src.ty.to_string(), dest.ty.to_string()))
        } else {
            match op {
                Operator::Equals => {
                    // unscope needs to happen prior to the compilation of the right-hand side
                    // thus it is the responsibility of the caller to unscope the prior value
                    let casted = self.copy(&src, dest)?;
                    self.try_scope(&casted)?;
                }
                Operator::Add => {
                    let current = self.emit(Insn::Load(dest.val, dest.ty.as_llvm_type(self.cpl)));
                    let total = self.emit(Insn::IAdd(current, src.val));
                    self.copy(&TypedValue::new(src.ty.clone(), total), dest)?;
                }
                Operator::Subtract => {
                    let current = self.emit(Insn::Load(dest.val, dest.ty.as_llvm_type(self.cpl)));
                    let total = self.emit(Insn::ISub(current, src.val));
                    self.copy(&TypedValue::new(src.ty.clone(), total), dest)?;
                }
                Operator::Multiply => {
                    let current = self.emit(Insn::Load(dest.val, dest.ty.as_llvm_type(self.cpl)));
                    let total = self.emit(Insn::IMul(current, src.val));
                    self.copy(&TypedValue::new(src.ty.clone(), total), dest)?;
                }
                Operator::Divide => {
                    let current = self.emit(Insn::Load(dest.val, dest.ty.as_llvm_type(self.cpl)));
                    let total = self.emit(Insn::UDiv(current, src.val));
                    self.copy(&TypedValue::new(src.ty.clone(), total), dest)?;
                }
                x => unimplemented!("{:?}", x),
            }

            Ok(())
        }
    }

    fn get_array_element_ptr(&mut self, slice: &TypedValue, index: &TypedValue) -> Result<TypedValue> {
        // TODO: add back bounds checking -- it's currently broken

        // let check_block = self.builder.create_block();
        // let load_block = self.builder.create_block();
        // let error_block = self.builder.create_block();
        // let rotated_parent_block = self.builder.create_block();

        let element_type = match &slice.ty {
            ComplexType::Array(element_type) => *element_type.clone(),
            _ => unreachable!(),
        };

        let offset_ptr = self.emit(Insn::GetElementPtr(slice.val, slice.ty.as_llvm_type(self.cpl), 0));
        let offset = self.emit(Insn::Load(offset_ptr, BasicType::USize.as_llvm_type(self.cpl)));
        let offset_index = self.emit(Insn::IAdd(offset, index.val));
        // let length_ptr = self.emit(Insn::GetElementPtr(slice.val, slice.ty.as_llvm_type(self.cpl), 1));
        // let length = self.emit(Insn::Load(length_ptr, BasicType::USize.as_llvm_type(self.cpl)));

        // let const_zero = self.cpl.context.const_int(self.cpl.context.get_isize_type(), 0);

        // let is_below_bounds = self.emit(Insn::ICmp(IntPredicate::LLVMIntULT, offset_index, const_zero));
        // self.emit(Insn::CondBr(is_below_bounds, error_block.as_val(), check_block.as_val()));

        // self.builder.append_block(&check_block);

        // let is_above_bounds = self.emit(Insn::ICmp(IntPredicate::LLVMIntUGE, offset_index, length));
        // self.emit(Insn::CondBr(is_above_bounds, error_block.as_val(), load_block.as_val()));

        // {
        //     self.builder.append_block(&error_block);

        //     let throw_out_of_bounds = ResolvedFunctionNode::externed(
        //         "keid.throw_out_of_bounds",
        //         &[BasicType::Int64.to_complex(), BasicType::Int64.to_complex()],
        //         Varargs::None,
        //         BasicType::Void.to_complex(),
        //     );
        //     let throw_out_of_bounds_ref = self.get_function_ref(&throw_out_of_bounds)?;
        //     self.emit(Insn::Call(throw_out_of_bounds_ref, throw_out_of_bounds.as_llvm_type(self.cpl), vec![index.val, length]));

        //     self.emit(Insn::Unreachable);
        // }

        let element_ptr = {
            // self.builder.append_block(&load_block);

            let array_data_type = self.cpl.context.get_abi_array_data_type(element_type.as_llvm_type(self.cpl), &element_type.to_string());

            let metadata_ptr = self.emit(Insn::GetElementPtr(slice.val, slice.ty.as_llvm_type(self.cpl), 2));
            let metadata = self.emit(Insn::Load(metadata_ptr, self.cpl.context.get_pointer_type(array_data_type)));

            let data_type = element_type.as_llvm_type(self.cpl);
            let data_ptr = self.emit(Insn::GetElementPtr(metadata, array_data_type, 1));
            let data = self.emit(Insn::Load(data_ptr, self.cpl.context.get_pointer_type(data_type)));

            let element_ptr = self.emit(Insn::GetElementPtrDynamic(data, data_type, offset_index));

            // self.emit(Insn::Br(rotated_parent_block.as_val()));

            element_ptr
        };

        // self.builder.append_block(&rotated_parent_block);

        Ok(TypedValue::new(element_type, element_ptr))
    }

    fn load_array_element(&mut self, slice: &TypedValue, index: &TypedValue) -> Result<TypedValue> {
        let element_ptr = self.get_array_element_ptr(slice, index)?;
        let element = TypedValueContainer(element_ptr).load(self)?;

        Ok(element)
    }

    fn store_array_element(&mut self, slice: &TypedValue, value: &TypedValue, index: &TypedValue, was_null: bool) -> Result<()> {
        let element_type = match &slice.ty {
            ComplexType::Array(element_type) => *element_type.clone(),
            _ => unreachable!(),
        };

        self.assert_assignable_to(&value.ty, &element_type)?;

        let element_ptr = self.get_array_element_ptr(slice, index)?;
        if !was_null {
            self.try_unscope(&element_ptr)?;
        }
        self.store(Operator::Equals, value.clone(), &element_ptr)?;

        Ok(())
    }

    fn copy(&mut self, src: &TypedValue, dest: &TypedValue) -> Result<TypedValue> {
        let casted = self.implicit_cast(src.clone(), &dest.ty)?;
        match &casted.ty {
            ComplexType::Basic(BasicType::Void) => panic!("attempt to copy void"),
            ComplexType::Nullable(ref inner) => {
                let llvm_type = casted.ty.as_llvm_type(self.cpl);

                let dest_val_ptr = self.emit(Insn::GetElementPtr(dest.val, llvm_type, 0));
                let casted_val_ptr = self.emit(Insn::GetElementPtr(casted.val, llvm_type, 0));
                let dest_nullable_ptr = self.emit(Insn::GetElementPtr(dest.val, llvm_type, 1));
                let casted_nullable_ptr = self.emit(Insn::GetElementPtr(casted.val, llvm_type, 1));

                let local_val = self.emit(Insn::Load(casted_val_ptr, inner.as_llvm_type(self.cpl)));
                self.emit(Insn::Store(local_val, dest_val_ptr));

                let local_val = TypedValueContainer(TypedValue::new(*inner.clone(), casted_val_ptr)).load(self)?;
                self.copy(&local_val, &TypedValue::new(*inner.clone(), dest_val_ptr))?;

                let nullable_val = self.emit(Insn::Load(casted_nullable_ptr, self.cpl.context.get_i8_type()));
                self.emit(Insn::Store(nullable_val, dest_nullable_ptr));
            }
            ComplexType::Array(ref element_type) => {
                let llvm_type = casted.ty.as_llvm_type(self.cpl);

                let dest_offset_ptr = self.emit(Insn::GetElementPtr(dest.val, llvm_type, 0));
                let casted_offset_ptr = self.emit(Insn::GetElementPtr(casted.val, llvm_type, 0));
                let dest_length_ptr = self.emit(Insn::GetElementPtr(dest.val, llvm_type, 1));
                let casted_length_ptr = self.emit(Insn::GetElementPtr(casted.val, llvm_type, 1));
                let dest_metadata_ptr = self.emit(Insn::GetElementPtr(dest.val, llvm_type, 2));
                let casted_metadata_ptr = self.emit(Insn::GetElementPtr(casted.val, llvm_type, 2));

                let offset_val = self.emit(Insn::Load(casted_offset_ptr, self.cpl.context.get_isize_type()));
                self.emit(Insn::Store(offset_val, dest_offset_ptr));

                let length_val = self.emit(Insn::Load(casted_length_ptr, self.cpl.context.get_isize_type()));
                self.emit(Insn::Store(length_val, dest_length_ptr));

                let metadata_val = self.emit(Insn::Load(
                    casted_metadata_ptr,
                    self.cpl.context.get_pointer_type(self.cpl.context.get_abi_array_data_type(element_type.as_llvm_type(self.cpl), &element_type.to_string())),
                ));
                self.emit(Insn::Store(metadata_val, dest_metadata_ptr));
            }
            ComplexType::Basic(BasicType::Object(ident)) => {
                let class = match self.cpl.type_provider.get_class_by_name(ident) {
                    Some(class) => class,
                    None => match self.cpl.type_provider.get_enum_by_name(ident) {
                        Some(_) => {
                            let size = self.cpl.context.target.get_type_size(casted.ty.as_llvm_type(self.cpl));
                            let const_size = self.cpl.context.const_int(self.cpl.context.get_isize_type(), size);
                            self.emit(Insn::Memmove(casted.val, dest.val, const_size));
                            return Ok(casted);
                        }
                        None => panic!("unresolved type"),
                    },
                };
                match class.class_type {
                    ClassType::Struct => {
                        self.assert_assignable_to(&src.ty, &dest.ty)?;

                        let src_classinfo_ptr = self.emit(Insn::GetElementPtr(casted.val, casted.ty.as_llvm_type(self.cpl), 0));
                        let src_classinfo =
                            self.emit(Insn::Load(src_classinfo_ptr, self.cpl.context.get_pointer_type(self.cpl.context.get_abi_class_info_type())));

                        let dest_classinfo_ptr = self.emit(Insn::GetElementPtr(dest.val, dest.ty.as_llvm_type(self.cpl), 0));
                        self.emit(Insn::Store(src_classinfo, dest_classinfo_ptr));

                        for i in 0..class.fields.len() as u32 {
                            let field_ty = class.fields[i as usize].ty.clone();

                            let src_element_ptr = self.emit(Insn::GetElementPtr(casted.val, casted.ty.as_llvm_type(self.cpl), i + 1));
                            let src_element = if field_ty.is_struct(&self.cpl.type_provider) {
                                src_element_ptr
                            } else {
                                self.emit(Insn::Load(src_element_ptr, field_ty.as_llvm_type(self.cpl)))
                            };
                            let dest_element_ptr = self.emit(Insn::GetElementPtr(dest.val, dest.ty.as_llvm_type(self.cpl), i + 1));

                            self.copy(&TypedValue::new(field_ty.clone(), src_element), &TypedValue::new(field_ty, dest_element_ptr))?;
                        }
                    }
                    ClassType::Interface | ClassType::Class => {
                        self.emit(Insn::Store(casted.val, dest.val));
                    }
                    _ => unreachable!(),
                }
            }
            _ => {
                self.emit(Insn::Store(casted.val, dest.val));
            }
        }

        Ok(casted)
    }

    fn implicit_cast(&mut self, src: TypedValue, dest_type: &ComplexType) -> Result<TypedValue> {
        match (src.ty.clone(), dest_type) {
            (ComplexType::Basic(BasicType::Null), ComplexType::Nullable(_)) => {
                // we are setting a nullable value to null
                // create a new value to assign to the value
                // then ensure the nullability byte is 0

                let new_nullable = self.emit(Insn::Alloca(dest_type.as_llvm_type(self.cpl)));
                let nullability_ptr = self.emit(Insn::GetElementPtr(new_nullable, dest_type.as_llvm_type(self.cpl), 1));
                let null_value = self.cpl.context.const_int(self.cpl.context.get_i8_type(), 0);
                self.emit(Insn::Store(null_value, nullability_ptr));

                return Ok(TypedValue {
                    ty: dest_type.clone(),
                    val: new_nullable,
                });
            }
            (ComplexType::Basic(from), ComplexType::Nullable(to)) => {
                self.assert_assignable_to(&from.to_complex(), &to)?;
                // coerce from a normal type to a nullable

                let new_nullable = self.emit(Insn::Alloca(dest_type.as_llvm_type(self.cpl)));

                let value_ptr = self.emit(Insn::GetElementPtr(new_nullable, dest_type.as_llvm_type(self.cpl), 0));
                self.copy(&src, &TypedValue::new(src.ty.clone(), value_ptr))?;

                let nullability_ptr = self.emit(Insn::GetElementPtr(new_nullable, dest_type.as_llvm_type(self.cpl), 1));
                let null_value = self.cpl.context.const_int(self.cpl.context.get_i8_type(), 1); // 1 indicates the value is non-null
                self.emit(Insn::Store(null_value, nullability_ptr));

                return Ok(TypedValue::new(dest_type.clone(), new_nullable));
            }
            (ComplexType::Basic(primitive_types!()), ComplexType::Basic(BasicType::Object(ident))) => {
                if ident.generic_args.len() != 0 {
                    return Ok(src);
                }

                let boxed = self.autobox_primitive(src.clone())?;
                if boxed.ty != BasicType::Object(ident.clone()).to_complex() {
                    return Ok(src);
                }

                return Ok(boxed);
            }
            (ComplexType::Basic(BasicType::Object(ident)), ComplexType::Basic(primitive_types!())) => {
                if ident.generic_args.len() != 0 {
                    return Ok(src);
                }
                let unboxed_type = match ident.name.as_str() {
                    "core::object::Bool" => BasicType::Bool,
                    "core::object::Char" => BasicType::Char,
                    "core::object::Int8" => BasicType::Int8,
                    "core::object::Int16" => BasicType::Int16,
                    "core::object::Int32" => BasicType::Int32,
                    "core::object::Int64" => BasicType::Int64,
                    "core::object::UInt8" => BasicType::UInt8,
                    "core::object::UInt16" => BasicType::UInt16,
                    "core::object::UInt32" => BasicType::UInt32,
                    "core::object::UInt64" => BasicType::UInt64,
                    "core::object::Float32" => BasicType::Float32,
                    "core::object::Float64" => BasicType::Float64,
                    "core::object::ISize" => BasicType::ISize,
                    "core::object::USize" => BasicType::USize,
                    _ => return Ok(src),
                }
                .to_complex();

                if &unboxed_type != dest_type {
                    return Ok(src);
                }

                let value_ptr = self.emit(Insn::GetElementPtr(src.val, src.ty.as_llvm_type(self.cpl), 1)); // element 1 is the only field in the struct type
                let unboxed_value = self.emit(Insn::Load(value_ptr, unboxed_type.as_llvm_type(&self.cpl)));
                return Ok(TypedValue::new(unboxed_type, unboxed_value));
            }
            (ComplexType::Basic(BasicType::Object(src_ident)), dest) => {
                if src_ident.name == "core::object::Box" && &src_ident.generic_args[0] == dest {
                    // element 2 is the Box element; 0 = classinfo ptr, 1 = ref count
                    let element_ptr = self.emit(Insn::GetElementPtr(src.val, src.ty.as_llvm_type(self.cpl), 2));
                    let element = TypedValueContainer(TypedValue::new(dest.clone(), element_ptr)).load(self).unwrap();
                    return Ok(element);
                }
            }
            // (ComplexType::Array(src_element), ComplexType::Array(dst_element)) => {

            // }
            _ => (),
        }
        Ok(src)
    }

    fn upcast(&mut self, src: TypedValue, parent: &str, generic_args: Option<Vec<ComplexType>>) -> Result<Option<TypedValue>> {
        let base_type = match &src.ty {
            ComplexType::Basic(BasicType::Object(ident)) => ident,
            ComplexType::Basic(primitive_types!()) => {
                if parent == "core::object::Object" {
                    return Ok(Some(self.autobox_primitive(src)?));
                }
                return Ok(None);
            }
            _ => return Ok(None),
        };

        if base_type.name == parent {
            if let Some(ref generic_args) = generic_args {
                if &base_type.generic_args == generic_args {
                    return Ok(Some(src));
                }
            } else {
                return Ok(Some(src));
            }
        }

        let resolved_interface_impls = self.cpl.type_provider.get_resolved_interface_impls(base_type);
        for resolved_interface_impl in resolved_interface_impls {
            let interface_impl = self.cpl.type_provider.get_source_interface_impl(&resolved_interface_impl);
            if interface_impl.interface_name == parent {
                // if generic args were specified, then ensure that the interface impl has the same args
                if let Some(generic_args) = generic_args.clone() {
                    if resolved_interface_impl.interface_generic_impls == generic_args {
                        return Ok(Some(TypedValue {
                            ty: BasicType::Object(GenericIdentifier::from_name_with_args(parent, &generic_args)).to_complex(),
                            val: src.val,
                        }));
                    }
                } else {
                    // if no generic args were specified, return the first generic implementation
                    return Ok(Some(TypedValue {
                        ty: BasicType::Object(GenericIdentifier::from_name_with_args(parent, &resolved_interface_impl.interface_generic_impls)).to_complex(),
                        val: src.val,
                    }));
                }
            }
        }

        Ok(None)
    }

    fn autobox_primitive(&mut self, primitive: TypedValue) -> Result<TypedValue> {
        let boxed_name = match &primitive.ty {
            ComplexType::Basic(basic) => match basic {
                BasicType::Bool => "core::object::Bool",
                BasicType::Char => "core::object::Char",
                BasicType::Int8 => "core::object::Int8",
                BasicType::Int16 => "core::object::Int16",
                BasicType::Int32 => "core::object::Int32",
                BasicType::Int64 => "core::object::Int64",
                BasicType::UInt8 => "core::object::UInt8",
                BasicType::UInt16 => "core::object::UInt16",
                BasicType::UInt32 => "core::object::UInt32",
                BasicType::UInt64 => "core::object::UInt64",
                BasicType::Float32 => "core::object::Float32",
                BasicType::Float64 => "core::object::Float64",
                BasicType::ISize => "core::object::ISize",
                BasicType::USize => "core::object::USize",
                _ => return Ok(primitive),
            },
            _ => return Ok(primitive),
        };

        let ident = GenericIdentifier::from_name(boxed_name);
        let boxed = self.instantiate_object(ComplexType::Basic(BasicType::Object(ident)))?;

        let value_ptr = self.emit(Insn::GetElementPtr(boxed.val, boxed.ty.as_llvm_type(self.cpl), 1)); // element 1 is the only field in the struct type
        self.emit(Insn::Store(primitive.val, value_ptr));

        Ok(boxed)
    }

    fn handle_unhandled_error(&mut self, check: bool) -> Result<()> {
        // some compiler-generated function calls are outside the block stack
        // we don't check those compiler-generated calls for errors
        if self.state.block_stack.is_empty() {
            return Ok(());
        }

        let catch_block = self.state.block_stack.iter().rev().find_map(|block| match &block.block_type {
            BlockType::Try(catch) => Some(catch.catch_block.as_val()),
            _ => None,
        });

        let rotated_parent = self.builder.create_block();
        if check {
            let check_unhandled_error_callable = ResolvedFunctionNode::externed("keid.check_unhandled_error", &[], Varargs::None, BasicType::Bool.to_complex());
            let check_unhandled_error_ref = self.get_function_ref(&check_unhandled_error_callable)?;
            let check_unhandled_error_val = self.call_function(check_unhandled_error_ref, &check_unhandled_error_callable, &[])?;

            let return_block = self.builder.create_block();

            self.emit(Insn::CondBr(check_unhandled_error_val, return_block.as_val(), rotated_parent.as_val()));

            self.builder.append_block(&return_block);
        }

        if let Some(catch_block) = catch_block {
            // jump to the catch block to handle the error
            self.emit(Insn::Br(catch_block));
        } else {
            // the error is unhandled, terminate execution of this function immediately
            self.return_null()?;
        }

        if check {
            self.builder.append_block(&rotated_parent);
        }

        Ok(())
    }

    fn return_null(&mut self) -> Result<()> {
        // self.pop_stack_frame()?;
        if self.func.return_type == BasicType::Void.to_complex() || self.func.return_type.is_struct(&self.cpl.type_provider) {
            self.emit(Insn::RetVoid);
        } else {
            self.emit(Insn::Ret(self.cpl.context.const_null(self.func.return_type.as_llvm_type(self.cpl))));
        }
        Ok(())
    }

    fn heap_allocate(&mut self, ty: OpaqueType, count: Option<OpaqueValue>) -> Result<OpaqueValue> {
        let malloc_func =
            ResolvedFunctionNode::externed("keid_malloc", &[BasicType::USize.to_complex()], Varargs::None, BasicType::Void.to_complex().to_reference());

        let size = self.cpl.context.target.get_type_size(ty);
        let mut size_const = self.cpl.context.const_int(self.cpl.context.get_i64_type(), size);
        if let Some(count) = count {
            size_const = self.emit(Insn::IMul(size_const, count));
        }

        let size_const = TypedValue::new(BasicType::USize.to_complex(), size_const);

        let malloc_func_ref = self.get_function_ref(&malloc_func)?;
        let memory = self.call_function(malloc_func_ref, &malloc_func, &[size_const])?;
        Ok(memory)
    }
}

pub fn get_import_map_with(imports: &[ImportNode], all_type_names: Vec<&String>) -> HashMap<String, String> {
    let mut map = HashMap::new();
    map.insert("string".to_owned(), "core::string::String".to_owned());
    map.insert("range".to_owned(), "core::collections::Range".to_owned());
    map.insert("object".to_owned(), "core::object::Object".to_owned());
    for node in imports {
        let types: Vec<String> = all_type_names.iter().filter(|type_name| get_type_namespace(type_name) == node.module).map(|str| str.to_string()).collect();
        for item in &types {
            map.try_insert(get_type_leaf(item).to_owned(), item.to_string()).unwrap();
        }
    }

    map
}

#[derive(Debug, PartialEq)]
pub struct ImportedMember {
    pub local_name: String,
    pub absolute_name: String,
}

pub fn lookup_import_map(map: &[ImportedMember], key: &str) -> Vec<String> {
    let mut imports: Vec<String> = map.iter().filter(|member| member.local_name == key).map(|member| member.absolute_name.clone()).collect();
    imports.push(key.to_owned());
    imports
}

pub fn get_import_map(imports: &[ImportNode], type_provider: &TypeProvider, from_namespace: Option<&str>) -> Vec<ImportedMember> {
    let mut map = vec![
        ImportedMember {
            local_name: "string".to_owned(),
            absolute_name: "core::string::String".to_owned(),
        },
        ImportedMember {
            local_name: "range".to_owned(),
            absolute_name: "core::collections::Range".to_owned(),
        },
        ImportedMember {
            local_name: "object".to_owned(),
            absolute_name: "core::object::Object".to_owned(),
        },
    ];

    let mut modules = imports.iter().map(|import| import.module.clone()).collect::<Vec<String>>();
    if let Some(from_namespace) = from_namespace {
        modules.push(from_namespace.to_string());
    }
    for module in modules {
        let types = type_provider.get_namespace_members(&module);
        let top_namespace_name = &module[module.rfind("::").map(|i| i + 2).unwrap_or(0)..];

        {
            let member = ImportedMember {
                local_name: top_namespace_name.to_owned(),
                absolute_name: module.clone(),
            };
            if !map.contains(&member) {
                map.push(member);
            }
        }

        for item in &types {
            let absolute_name = format!("{}::{}", module, item.name);
            match item.member_type {
                NamespaceMemberType::Type => {
                    map.push(ImportedMember {
                        local_name: item.name.clone(),
                        absolute_name,
                    });
                }
                NamespaceMemberType::Member => {
                    let member = ImportedMember {
                        local_name: format!("{}.{}", top_namespace_name, item.name),
                        absolute_name: absolute_name.clone(),
                    };
                    if !map.contains(&member) {
                        map.push(member);
                    }
                    if let Some(from_namespace) = from_namespace {
                        if from_namespace == module {
                            map.push(ImportedMember {
                                local_name: item.name.clone(),
                                absolute_name,
                            });
                        }
                    }
                }
            }
        }
    }

    let default_modules = vec![
        "core::array",
        "core::collections",
        "core::error",
        "core::math",
        "core::mem",
        "core::object",
        "core::ops",
        "core::runtime",
        "core::string",
        "core::test",
    ];
    for default_module in default_modules {
        let top_namespace_name = &default_module[default_module.rfind("::").map(|i| i + 2).unwrap_or(0)..];
        let member = ImportedMember {
            local_name: top_namespace_name.to_owned(),
            absolute_name: default_module.to_owned(),
        };
        if !map.contains(&member) {
            map.push(member);
        }
    }

    map
}

pub fn iter_join<T>(iter: &[T]) -> String
where
    T: ToString,
{
    iter.iter().map(ToString::to_string).collect::<Vec<_>>().join(", ")
}

pub fn path_to_module_name(root: &str, path: &str) -> String {
    let path = if path.starts_with(root) {
        &path[root.len() + 1..]
    } else {
        path
    };

    path[0..path.rfind('.').unwrap_or(path.len())].to_string().replace('/', "_")
}

pub fn get_type_namespace(ty: &str) -> &str {
    &ty[0..ty.rfind("::").unwrap_or(ty.len())]
}

pub fn get_type_leaf(ty: &str) -> &str {
    &ty[ty.rfind("::").map(|i| i + 2).unwrap_or(0)..ty.len()]
}

/// 1. List all abstract and virtual functions in the hierarchy, starting with the root node.
/// 2. Find the "deepest" implementation of each function in the hierarchy and include the pointer to that function.
pub fn generate_vtable(type_provider: &TypeProvider, ident: &GenericIdentifier) -> Vec<VirtualMethodTableEntry> {
    let mut vtable = Vec::new();

    // Step 1 -- locate each virtual method definition
    {
        let mut current_ident = Some(ident.clone());
        while let Some(ref ci) = current_ident {
            match type_provider.get_class_by_name(ci) {
                Some(class_impl) => {
                    let mut layer_vtable = Vec::new();
                    let source_class = type_provider.get_source_class(&class_impl);
                    for function_id in &source_class.functions {
                        let function = type_provider.get_function_node(class_impl.module_id, *function_id).unwrap();
                        if function.modifiers.contains(&FunctionModifier::Abstract) || function.modifiers.contains(&FunctionModifier::Virtual) {
                            layer_vtable.push(VirtualMethodTableEntry {
                                definition_name: GenericIdentifier::from_name_with_args(&function.base_name, &class_impl.generic_impls),
                                implementation_name: None,
                            })
                        }
                    }

                    current_ident = class_impl.superclass.clone();
                    vtable.insert(0, layer_vtable);
                }
                None => break,
            }
        }
    }

    // Step 2 -- locate each virtual method implementation
    let mut vtable: Vec<VirtualMethodTableEntry> = vtable.into_iter().flatten().collect();
    {
        let mut current_ident = Some(ident.clone());
        while let Some(ref ci) = current_ident {
            match type_provider.get_class_by_name(ci) {
                Some(class_impl) => {
                    let functions = type_provider.get_source_class(&class_impl).functions.clone();
                    for function_id in functions {
                        let function_name = {
                            let function = type_provider.get_function_node(class_impl.module_id, function_id).unwrap();
                            if function.modifiers.contains(&FunctionModifier::Abstract) {
                                continue;
                            }
                            utils::get_type_leaf(&function.base_name).to_owned()
                        };

                        for vtable_entry in &mut vtable {
                            // don't modify vtable entries which have already been assigned an implementation
                            if vtable_entry.implementation_name.is_some() {
                                continue;
                            }

                            let vf_name = utils::get_type_leaf(&vtable_entry.definition_name.name);

                            // TODO: check args in the future to support overloading
                            if function_name == vf_name {
                                let implementation_name = {
                                    let function = type_provider.get_function_node(class_impl.module_id, function_id).unwrap();
                                    GenericIdentifier::from_name_with_args(&function.base_name, &class_impl.generic_impls)
                                };
                                vtable_entry.implementation_name = Some(implementation_name);
                            }
                        }
                    }

                    current_ident = class_impl.superclass.clone();
                }
                None => break,
            }
        }
    }

    vtable
}
