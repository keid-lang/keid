use super::CallCompiler;
use crate::{
    common::{types::*, *},
    compiler::llvm::*,
    compiler_error,
    func::*,
};

macro_rules! is_int {
    () => {
        BasicType::UInt8
            | BasicType::UInt16
            | BasicType::UInt32
            | BasicType::UInt64
            | BasicType::USize
            | BasicType::Int8
            | BasicType::Int16
            | BasicType::Int32
            | BasicType::Int64
            | BasicType::ISize
            | BasicType::Char
    };
}

macro_rules! is_float {
    () => {
        BasicType::Float32 | BasicType::Float64
    };
}

pub trait LogicCompiler {
    fn compile_logic_expr(&mut self, lhs: TypedValue, op: Operator, rhs: TypedValue) -> Result<TypedValue>;
    fn compile_cast(&mut self, from: TypedValue, to: ComplexType) -> Result<TypedValue>;
}

impl<'a> LogicCompiler for FunctionCompiler<'a> {
    fn compile_cast(&mut self, from: TypedValue, to: ComplexType) -> Result<TypedValue> {
        let casted = match (&from.ty, &to) {
            (ComplexType::Basic(original), ComplexType::Basic(target)) => match (original, target) {
                (is_int!(), is_int!()) => self.emit(Insn::IntCast(from.val, target.as_llvm_type(self.cpl), original.is_signed())),
                (is_int!(), is_float!()) => self.emit(Insn::IntToFloat(from.val, target.as_llvm_type(self.cpl), original.is_signed())),
                (is_float!(), is_int!()) => self.emit(Insn::FloatToInt(from.val, target.as_llvm_type(self.cpl), target.is_signed())),
                (is_float!(), is_float!()) => self.emit(Insn::FloatCast(from.val, target.as_llvm_type(self.cpl))),
                _ => return Err(compiler_error!(self, "Cannot cast from type `{}` to `{}`", from.ty.to_string(), to.to_string())),
            },
            _ => return Err(compiler_error!(self, "Cannot cast from type `{}` to `{}`", from.ty.to_string(), to.to_string())),
        };

        Ok(TypedValue {
            ty: to,
            val: casted,
        })
    }

    fn compile_logic_expr(&mut self, lhs: TypedValue, op: Operator, rhs: TypedValue) -> Result<TypedValue> {
        if !self.cpl.type_provider.is_assignable_to(&lhs.ty, &rhs.ty) && !self.cpl.type_provider.is_assignable_to(&rhs.ty, &lhs.ty) {
            return Err(compiler_error!(self, "Cannot compare value of type `{}` with `{}`", lhs.ty.to_string(), rhs.ty.to_string()));
        }

        match (&lhs.ty, &rhs.ty) {
            (ComplexType::Array(lhs_element_type), ComplexType::Array(rhs_element_type)) => {
                let inverse = match op {
                    Operator::Equals => false,
                    Operator::NotEquals => true,
                    _ => return Err(compiler_error!(self, "Only the `==` and `!=` operators are implemented for array types")),
                };
                self.assert_assignable_to(rhs_element_type, lhs_element_type)?;

                let array_equals_impl = self
                    .cpl
                    .type_provider
                    .get_function_by_name(
                        &GenericIdentifier::from_name_with_args("core::array::equals", &[*lhs_element_type.clone()]),
                        &[(*lhs_element_type.clone()).to_array(), (*lhs_element_type.clone()).to_array()],
                    )
                    .unwrap();
                let array_equals_ref = self.get_function_ref(&array_equals_impl)?;
                let result = self.call_function(array_equals_ref, &array_equals_impl, &[lhs.clone(), rhs.clone()])?;
                if !inverse {
                    return Ok(TypedValue::new(BasicType::Bool.to_complex(), result));
                } else {
                    let true_const = self.cpl.context.const_int(BasicType::Bool.as_llvm_type(self.cpl), 1);
                    let inverted_bool = self.emit(Insn::Xor(result, true_const));

                    return Ok(TypedValue::new(BasicType::Bool.to_complex(), inverted_bool));
                }
            }
            (ComplexType::Basic(BasicType::Object(lhs_ident)), ComplexType::Basic(BasicType::Object(rhs_ident))) => {
                let (operator_interface_name, func_name) = match op {
                    Operator::Add => ("core::ops::Add", "add"),
                    Operator::Subtract => ("core::ops::Subtract", "subtract"),
                    Operator::Multiply => ("core::ops::Multiply", "multiply"),
                    Operator::Divide => ("core::ops::Divide", "divide"),
                    Operator::Equals | Operator::NotEquals => ("core::ops::Equals", "equals"),
                    x => unimplemented!("{:?}", x),
                };

                let resolved_interface_impls = self.cpl.type_provider.get_resolved_interface_impls(lhs_ident);
                for resolved_interface_impl in resolved_interface_impls {
                    let source_interface_impl = self.cpl.type_provider.get_source_interface_impl(&resolved_interface_impl);
                    let rhs_type = BasicType::Object(rhs_ident.clone()).to_complex();
                    if source_interface_impl.interface_name == operator_interface_name && resolved_interface_impl.interface_generic_impls[0] == rhs_type {
                        let interface_id = self
                            .cpl
                            .type_provider
                            .get_resolved_interface_id(&GenericIdentifier::from_name_with_args(&source_interface_impl.interface_name, &[rhs_type.clone()]));

                        let callable = self
                            .cpl
                            .type_provider
                            .get_function_by_name(
                                &GenericIdentifier::from_name_with_args(&format!("{}::{}", operator_interface_name, func_name), &[rhs_type.clone()]),
                                &[lhs.ty.clone(), rhs_type.clone()],
                            )
                            .unwrap();
                        let callable = self.resolve_interface_impl_function(callable, source_interface_impl)?;

                        // method ID 0, since all of these interfaces have only one single method
                        let method_ptr = self.get_interface_method_ptr(&InterfaceInvocation::Instance(lhs.clone()), interface_id, 0)?;

                        let result = self.call_function(method_ptr, &callable, &[lhs.clone(), rhs.clone()])?;
                        if op == Operator::Equals {
                            return Ok(TypedValue::new(BasicType::Bool.to_complex(), result));
                        } else if op == Operator::NotEquals {
                            let true_const = self.cpl.context.const_int(BasicType::Bool.as_llvm_type(self.cpl), 1);
                            let inverted_bool = self.emit(Insn::Xor(result, true_const));

                            return Ok(TypedValue::new(BasicType::Bool.to_complex(), inverted_bool));
                        } else {
                            return Ok(TypedValue::new(rhs_type, result));
                        }
                    }
                }

                return Err(compiler_error!(self, "Type `{}` missing interface implementation for `{}`", lhs_ident.to_string(), operator_interface_name));
            }
            _ => (),
        }

        let is_signed = if matches!(&lhs.ty, ComplexType::Basic(is_int!())) && matches!(&rhs.ty, ComplexType::Basic(is_int!())) {
            let lhs_signed = matches!(&lhs.ty, ComplexType::Basic(BasicType::Int8 | BasicType::Int16 | BasicType::Int32 | BasicType::Int64));
            let rhs_signed = matches!(&rhs.ty, ComplexType::Basic(BasicType::Int8 | BasicType::Int16 | BasicType::Int32 | BasicType::Int64));
            if rhs_signed != lhs_signed {
                return Err(compiler_error!(self, "Cannot compare integers of type `{}` and `{}`", lhs.ty.to_string(), rhs.ty.to_string()));
            }
            lhs_signed
        } else {
            false
        };

        let (val, ty) = match &op {
            Operator::Equals | Operator::NotEquals => {
                let predicate = match &op {
                    Operator::Equals => IntPredicate::LLVMIntEQ,
                    Operator::NotEquals => IntPredicate::LLVMIntNE,
                    x => unreachable!("{:?}", x),
                };

                let (lhs_val, rhs_val) = match ((lhs.val, &lhs.ty), (rhs.val, &rhs.ty)) {
                    ((lhs_val, ComplexType::Nullable(lhs_type)), (rhs_val, ComplexType::Nullable(rhs_type))) => {
                        let nullable_llvm_type = ComplexType::Nullable(lhs_type.clone()).as_llvm_type(self.cpl);
                        let lhs_nullability_ptr = self.emit(Insn::GetElementPtr(lhs_val, nullable_llvm_type, 1));
                        let lhs_nullability = self.emit(Insn::Load(lhs_nullability_ptr, self.cpl.context.get_i8_type()));
                        let rhs_nullability_ptr = self.emit(Insn::GetElementPtr(rhs_val, nullable_llvm_type, 1));
                        let rhs_nullability = self.emit(Insn::Load(rhs_nullability_ptr, self.cpl.context.get_i8_type()));
                        let nullability_equal = self.emit(Insn::ICmp(IntPredicate::LLVMIntEQ, lhs_nullability, rhs_nullability));
                        let local_result_ptr = self.emit(Insn::Alloca(self.cpl.context.get_i1_type()));
                        self.emit(Insn::Store(nullability_equal, local_result_ptr));

                        let inner_equality_block = self.builder.create_block();
                        let rotated_parent = self.builder.create_block();

                        let const_one = self.cpl.context.const_int(self.cpl.context.get_i8_type(), 1);
                        let is_not_null = self.emit(Insn::ICmp(IntPredicate::LLVMIntEQ, lhs_nullability, const_one));

                        self.emit(Insn::CondBr(is_not_null, inner_equality_block.as_val(), rotated_parent.as_val()));

                        {
                            self.builder.append_block(&inner_equality_block);
                            self.builder.use_block(&inner_equality_block);

                            let lhs_value_ptr = self.emit(Insn::GetElementPtr(lhs_val, nullable_llvm_type, 0));
                            let lhs_value = TypedValueContainer(TypedValue::new(*lhs_type.clone(), lhs_value_ptr)).load(self)?;
                            let rhs_value_ptr = self.emit(Insn::GetElementPtr(rhs_val, nullable_llvm_type, 0));
                            let rhs_value = TypedValueContainer(TypedValue::new(*rhs_type.clone(), rhs_value_ptr)).load(self)?;

                            let result = self.compile_logic_expr(lhs_value, op, rhs_value)?;
                            self.assert_assignable_to(&result.ty, &BasicType::Bool.to_complex())?;

                            self.emit(Insn::Store(result.val, local_result_ptr));
                            self.emit(Insn::Br(rotated_parent.as_val()));
                        }

                        {
                            self.builder.append_block(&rotated_parent);
                            self.builder.use_block(&rotated_parent);

                            let local_result = self.emit(Insn::Load(local_result_ptr, self.cpl.context.get_i1_type()));

                            let const_true = self.cpl.context.const_int(self.cpl.context.get_i1_type(), 1);
                            let local_op = self.emit(Insn::ICmp(predicate, local_result, const_true));

                            return Ok(TypedValue::new(BasicType::Bool.to_complex(), local_op));
                        }
                    }
                    ((nullable, ComplexType::Nullable(inner)), (_, ComplexType::Basic(BasicType::Null)))
                    | ((_, ComplexType::Basic(BasicType::Null)), (nullable, ComplexType::Nullable(inner))) => {
                        let nullability_ptr = self.emit(Insn::GetElementPtr(nullable, ComplexType::Nullable(inner.clone()).as_llvm_type(self.cpl), 1));
                        let nullability = self.emit(Insn::Load(nullability_ptr, self.cpl.context.get_i8_type()));
                        let const_zero = self.cpl.context.const_int(self.cpl.context.get_i8_type(), 0);
                        (nullability, const_zero)
                    }
                    ((lhs_val, _), (rhs_val, _)) => (lhs_val, rhs_val),
                };

                (self.emit(Insn::ICmp(predicate, lhs_val, rhs_val)), BasicType::Bool.to_complex())
            }
            Operator::Add => (self.emit(Insn::IAdd(lhs.val, rhs.val)), lhs.ty),
            Operator::Subtract => (self.emit(Insn::ISub(lhs.val, rhs.val)), lhs.ty),
            Operator::Multiply => (self.emit(Insn::IMul(lhs.val, rhs.val)), lhs.ty),
            Operator::Divide => (self.emit(Insn::UDiv(lhs.val, rhs.val)), lhs.ty),
            Operator::Modulus => (self.emit(Insn::URem(lhs.val, rhs.val)), lhs.ty),
            Operator::LessThan => (
                self.emit(Insn::ICmp(
                    if is_signed {
                        IntPredicate::LLVMIntSLT
                    } else {
                        IntPredicate::LLVMIntULT
                    },
                    lhs.val,
                    rhs.val,
                )),
                BasicType::Bool.to_complex(),
            ),
            Operator::GreaterThan => (
                self.emit(Insn::ICmp(
                    if is_signed {
                        IntPredicate::LLVMIntSGT
                    } else {
                        IntPredicate::LLVMIntUGT
                    },
                    lhs.val,
                    rhs.val,
                )),
                BasicType::Bool.to_complex(),
            ),
            Operator::LessThanOrEquals => (
                self.emit(Insn::ICmp(
                    if is_signed {
                        IntPredicate::LLVMIntSLE
                    } else {
                        IntPredicate::LLVMIntULE
                    },
                    lhs.val,
                    rhs.val,
                )),
                BasicType::Bool.to_complex(),
            ),
            Operator::GreaterThanOrEquals => (
                self.emit(Insn::ICmp(
                    if is_signed {
                        IntPredicate::LLVMIntSGE
                    } else {
                        IntPredicate::LLVMIntUGE
                    },
                    lhs.val,
                    rhs.val,
                )),
                BasicType::Bool.to_complex(),
            ),
            Operator::BooleanAnd => (self.emit(Insn::IAnd(lhs.val, rhs.val)), BasicType::Bool.to_complex()),
            Operator::BooleanOr => (self.emit(Insn::IOr(lhs.val, rhs.val)), BasicType::Bool.to_complex()),
            op => unimplemented!("not yet implemented: `{:?}`", op),
        };

        Ok(TypedValue {
            ty,
            val,
        })
    }
}
