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

                let true_const = self
                    .cpl
                    .context
                    .const_int(BasicType::Bool.as_llvm_type(self.cpl), 1);
                let inverted_bool = self.emit(Insn::Xor(expr.val, true_const));

                TypedValue {
                    ty: BasicType::Bool.to_complex(),
                    val: inverted_bool,
                }
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

                    let nullability_ptr = self.emit(Insn::GetElementPtr(
                        expr.val,
                        expr.ty.as_llvm_type(&self.cpl),
                        1,
                    ));
                    let nullablility =
                        self.emit(Insn::Load(nullability_ptr, self.cpl.context.get_i8_type()));
                    self.emit(Insn::Call(
                        assert_func,
                        assert_impl.as_llvm_type(&self.cpl),
                        vec![nullablility],
                    ));

                    let value_ptr = self.emit(Insn::GetElementPtr(
                        expr.val,
                        expr.ty.as_llvm_type(&self.cpl),
                        0,
                    ));
                    let value = TypedValueContainer(TypedValue::new(*inner.clone(), value_ptr))
                        .load(self)?;

                    TypedValue {
                        ty: *inner.clone(),
                        val: value,
                    }
                }
                _ => {
                    return Err(compiler_error!(
                        &self,
                        "the null assertion operator cannot be used on non-nullable type `{}`",
                        expr.ty.to_string()
                    ))
                }
            },
            x => unimplemented!("unimplemented unary operator: {:#?}", x),
        })
    }
}
