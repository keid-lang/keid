use crate::{
    common::{
        types::{BasicType, ComplexType, IntoOpaqueType},
        Result, TypedValue, TypedValueContainer, ValueContainer,
    },
    compiler::llvm::Insn,
    compiler_error,
    func::{utils::FunctionCompilerUtils, FunctionCompiler},
    tree::{
        ast::{Operator, Varargs},
        ResolvedFunctionNode,
    },
};

pub trait UnaryCompiler {
    fn compile_unary_expr(&mut self, operator: Operator, expr: &TypedValue) -> Result<TypedValue>;
}

impl<'a> UnaryCompiler for FunctionCompiler<'a> {
    fn compile_unary_expr(&mut self, operator: Operator, expr: &TypedValue) -> Result<TypedValue> {
        Ok(match operator {
            Operator::Spread => TypedValue {
                ty: ComplexType::Spread(Box::new(expr.ty.clone())),
                val: expr.val,
            },
            Operator::Not => {
                self.assert_assignable_to(&expr.ty, &BasicType::Bool.to_complex())?;

                let true_const = self.cpl.context.const_int(BasicType::Bool.as_llvm_type(self.cpl), 1);
                let inverted_bool = self.emit(Insn::Xor(expr.val, true_const));

                TypedValue::new(BasicType::Bool.to_complex(), inverted_bool)
            }
            Operator::NonNullAssertion => match &expr.ty {
                ComplexType::Nullable(inner) => {
                    let assert_impl = ResolvedFunctionNode::externed(
                        "keid.assert_non_null",
                        &[BasicType::Int8.to_complex()],
                        Varargs::None,
                        BasicType::Void.to_complex(),
                    );
                    let assert_func = self.get_function_ref(&assert_impl)?;

                    let nullability_ptr = self.emit(Insn::GetElementPtr(expr.val, expr.ty.as_llvm_type(self.cpl), 1));
                    let nullablility = self.emit(Insn::Load(nullability_ptr, self.cpl.context.get_i8_type()));
                    self.emit(Insn::Call(assert_func, assert_impl.as_llvm_type(self.cpl), vec![nullablility]));

                    let value_ptr = self.emit(Insn::GetElementPtr(expr.val, expr.ty.as_llvm_type(self.cpl), 0));
                    let value = TypedValueContainer(TypedValue::new(*inner.clone(), value_ptr)).load(self)?;

                    value
                }
                _ => {
                    return Err(compiler_error!(
                        self,
                        "the null assertion operator cannot be used on non-nullable type `{}`",
                        expr.ty.to_string()
                    ))
                }
            },
            Operator::Negate => {
                let negated = match &expr.ty {
                    ComplexType::Basic(BasicType::Int8 | BasicType::Int16 | BasicType::Int32 | BasicType::Int64 | BasicType::ISize) => {
                        self.emit(Insn::INeg(expr.val))
                    }
                    ComplexType::Basic(BasicType::Float32 | BasicType::Float64) => {
                        self.emit(Insn::FNeg(expr.val))
                    }
                    ComplexType::Basic(BasicType::UInt8 | BasicType::UInt16 | BasicType::UInt32 | BasicType::UInt64 | BasicType::USize) => {
                        return Err(compiler_error!(self, "cannot negate value of unsigned integer type `{}`", expr.ty.to_string()));
                    }
                    _ => {
                        return Err(compiler_error!(self, "cannot negate value of type `{}`", expr.ty.to_string()));
                    }
                };

                TypedValue::new(expr.ty.clone(), negated)
            }
            x => unimplemented!("unimplemented unary operator: {:#?}", x),
        })
    }
}
