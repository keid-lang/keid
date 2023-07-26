use super::CallCompiler;
use crate::{
    common::{types::*, *},
    compiler::llvm::*,
    compiler_error,
    func::*,
};

pub trait LogicCompiler {
    fn compile_logic_expr(&mut self, lhs: TypedValue, op: Operator, rhs: TypedValue) -> Result<TypedValue>;
    fn compile_cast(&mut self, from: TypedValue, to: ComplexType) -> Result<TypedValue>;
}

impl<'a> LogicCompiler for FunctionCompiler<'a> {
    fn compile_cast(&mut self, from: TypedValue, to: ComplexType) -> Result<TypedValue> {
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
                    if source_interface_impl.interface_name == operator_interface_name
                        && resolved_interface_impl.interface_generic_impls[0] == rhs_type
                    {
                        let interface_id = self.cpl.type_provider.get_resolved_interface_id(&GenericIdentifier::from_name_with_args(
                            &source_interface_impl.interface_name,
                            &[rhs_type.clone()],
                        ));

                        let callable = self
                            .cpl
                            .type_provider
                            .get_function_by_name(
                                &GenericIdentifier::from_name_with_args(
                                    &format!("{}::{}", operator_interface_name, func_name),
                                    &[rhs_type.clone()],
                                ),
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

                return Err(compiler_error!(
                    self,
                    "Type `{}` missing interface implementation for `{}`",
                    lhs_ident.to_string(),
                    operator_interface_name
                ));
            }
            _ => (),
        }

        let (val, ty) = match &op {
            Operator::Equals | Operator::NotEquals => {
                let (lhs_val, rhs_val) = match ((lhs.val, &lhs.ty), (rhs.val, &rhs.ty)) {
                    ((_, ComplexType::Nullable(_)), (_, ComplexType::Nullable(_))) => {
                        todo!("compare nullables")
                    }
                    ((nullable, ComplexType::Nullable(inner)), (_, ComplexType::Basic(BasicType::Null)))
                    | ((_, ComplexType::Basic(BasicType::Null)), (nullable, ComplexType::Nullable(inner))) => {
                        let nullability_ptr =
                            self.emit(Insn::GetElementPtr(nullable, ComplexType::Nullable(inner.clone()).as_llvm_type(self.cpl), 1));
                        let nullability = self.emit(Insn::Load(nullability_ptr, self.cpl.context.get_i8_type()));
                        let const_zero = self.cpl.context.const_int(self.cpl.context.get_i8_type(), 0);
                        (nullability, const_zero)
                    }
                    ((lhs_val, _), (rhs_val, _)) => (lhs_val, rhs_val),
                };

                let predicate = match &op {
                    Operator::Equals => IntPredicate::LLVMIntEQ,
                    Operator::NotEquals => IntPredicate::LLVMIntNE,
                    x => unreachable!("{:?}", x),
                };

                (self.emit(Insn::ICmp(predicate, lhs_val, rhs_val)), BasicType::Bool.to_complex())
            }
            Operator::Add => (self.emit(Insn::IAdd(lhs.val, rhs.val)), lhs.ty),
            Operator::Subtract => (self.emit(Insn::ISub(lhs.val, rhs.val)), lhs.ty),
            Operator::Multiply => (self.emit(Insn::IMul(lhs.val, rhs.val)), lhs.ty),
            Operator::Divide => (self.emit(Insn::UDiv(lhs.val, rhs.val)), lhs.ty),
            Operator::Modulus => (self.emit(Insn::URem(lhs.val, rhs.val)), lhs.ty),
            Operator::LessThan => (self.emit(Insn::ICmp(IntPredicate::LLVMIntULT, lhs.val, rhs.val)), BasicType::Bool.to_complex()),
            Operator::GreaterThan => (self.emit(Insn::ICmp(IntPredicate::LLVMIntUGT, lhs.val, rhs.val)), BasicType::Bool.to_complex()),
            Operator::LessThanOrEquals => (self.emit(Insn::ICmp(IntPredicate::LLVMIntULE, lhs.val, rhs.val)), BasicType::Bool.to_complex()),
            Operator::GreaterThanOrEquals => {
                (self.emit(Insn::ICmp(IntPredicate::LLVMIntUGE, lhs.val, rhs.val)), BasicType::Bool.to_complex())
            }
            op => unimplemented!("not yet implemented: `{:?}`", op),
        };

        Ok(TypedValue {
            ty,
            val,
        })
    }
}
