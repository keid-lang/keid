use crate::{
    common::{types::BasicType, Result, TypedValue},
    compiler::llvm::Insn,
    func::{utils::FunctionCompilerUtils, FunctionCompiler},
    tree::ast::{Expr, Token},
};

use super::ExprCompiler;

pub trait ReturnCompiler {
    fn compile_return(&mut self, return_val: Option<&Token<Expr>>) -> Result<()>;
}

impl<'a> ReturnCompiler for FunctionCompiler<'a> {
    fn compile_return(&mut self, return_val: Option<&Token<Expr>>) -> Result<()> {
        let return_type = self.resolve_type(&self.func.return_type)?;
        match return_val {
            Some(expr) => {
                let compiled_expr = self.compile_expr(expr, Some(&return_type))?;
                let casted = self.implicit_cast(compiled_expr, &return_type)?;

                self.assert_assignable_to(&casted.ty, &return_type)?;
                if return_type.is_struct(&self.cpl.type_provider) {
                    let preallocated_return = self.llvm_func.get_param(self.func.params.len() as u32);
                    self.copy(&casted, &TypedValue::new(return_type, preallocated_return))?;

                    self.pop_stack_frame()?;
                    self.emit(Insn::RetVoid);
                } else {
                    self.pop_stack_frame()?;
                    self.emit(Insn::Ret(casted.val));
                }
            }
            None => {
                self.assert_assignable_to(&BasicType::Void.to_complex(), &return_type)?;
                self.pop_stack_frame()?;
                self.emit(Insn::RetVoid);
            }
        };
        Ok(())
    }
}
