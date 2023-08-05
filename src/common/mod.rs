pub mod kpkg;
pub mod types;

mod type_provider;
pub use type_provider::*;

use self::types::{ComplexType, IntoOpaqueType};

use crate::compiler::llvm::{Insn, OpaqueValue};
use crate::func::compilers::CallCompiler;
use crate::func::utils::FunctionCompilerUtils;
use crate::func::FunctionCompiler;
use crate::parser;
use crate::parser::*;
use crate::tree::ast::{Operator, TokenLocation};
use crate::tree::ResolvedFunctionNode;
use std::backtrace::Backtrace;
use std::fmt::Display;
use thiserror::Error;
pub trait TypeResolver = Fn(&str) -> Result<ComplexType>;

#[derive(Error, Debug)]
pub struct CompilerError {
    pub message: String,
    pub loc: TokenLocation,
    pub backtrace: Backtrace,
}

impl Clone for CompilerError {
    fn clone(&self) -> Self {
        Self {
            message: self.message.clone(),
            loc: self.loc.clone(),
            backtrace: Backtrace::disabled(),
        }
    }
}

impl Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)?;
        Ok(())
    }
}

impl CompilerError {
    pub fn as_pest_error(&self, source: &str) -> pest::error::Error<Rule> {
        pest::error::Error::new_from_span(
            pest::error::ErrorVariant::<parser::Rule>::CustomError {
                message: self.message.clone(),
            },
            pest::Span::new(source, self.loc.start, self.loc.end).unwrap_or_else(|| panic!("message: {}", self.message)),
        )
    }
}

pub type Result<T> = std::result::Result<T, CompilerError>;

#[derive(Debug, Clone, PartialEq)]
pub struct TypedValue {
    pub ty: ComplexType,
    pub val: OpaqueValue,
}

impl TypedValue {
    pub fn new(ty: ComplexType, val: OpaqueValue) -> TypedValue {
        TypedValue {
            ty,
            val,
        }
    }
}

pub trait ValueContainer {
    /// gets the type of the value
    fn get_type(&self) -> ComplexType;
    /// loads `self` and returns the value
    fn load(&self, fc: &mut FunctionCompiler) -> Result<TypedValue>;
    /// stores `val` into `self`
    fn store(&self, op: Operator, fc: &mut FunctionCompiler, val: TypedValue) -> Result<()>;
}

#[derive(Debug)]
pub struct TypedValueContainer(pub TypedValue);

impl ValueContainer for TypedValueContainer {
    fn load(&self, fc: &mut FunctionCompiler) -> Result<TypedValue> {
        if self.0.ty.is_struct(&fc.cpl.type_provider) {
            Ok(TypedValue::new(self.0.ty.clone(), self.0.val))
        } else {
            Ok(TypedValue::new(self.0.ty.clone(), fc.emit(Insn::Load(self.0.val, self.0.ty.as_llvm_type(fc.cpl)))))
        }
    }

    fn store(&self, op: Operator, fc: &mut FunctionCompiler, val: TypedValue) -> Result<()> {
        fc.store(op, val, &self.0)?;
        Ok(())
    }

    fn get_type(&self) -> ComplexType {
        self.0.ty.clone()
    }
}

#[derive(Debug)]
pub struct AccessorValueContainer(ResolvedFunctionNode, OpaqueValue);

impl AccessorValueContainer {
    pub fn new(func: ResolvedFunctionNode, val: OpaqueValue) -> AccessorValueContainer {
        AccessorValueContainer(func, val)
    }
}

impl ValueContainer for AccessorValueContainer {
    fn load(&self, fc: &mut FunctionCompiler) -> Result<TypedValue> {
        let func_value = fc.get_function_ref(&self.0)?;
        Ok(TypedValue::new(self.0.return_type.clone(), fc.call_function(func_value, &self.0, &[TypedValue::new(self.0.params[0].clone(), self.1)])?))
    }

    fn store(&self, _: Operator, _: &mut FunctionCompiler, _: TypedValue) -> Result<()> {
        todo!()
    }

    fn get_type(&self) -> ComplexType {
        self.0.return_type.clone()
    }
}

#[macro_export]
macro_rules! compiler_error {
    ( $cpl:expr, $($arg:tt)+ ) => {{
        let message = format!($($arg)+);
        let loc = $cpl.state.current_token.clone();
        $crate::common::CompilerError {
            message,
            loc,
            backtrace: std::backtrace::Backtrace::force_capture(),
        }
    }};
}

#[macro_export]
macro_rules! compiler_error_loc {
    ( $loc:expr, $($arg:tt)+ ) => {{
        let message = format!($($arg)+);
        let loc = $loc.clone();
        $crate::common::CompilerError {
            message,
            loc,
            backtrace: std::backtrace::Backtrace::force_capture(),
        }
    }};
}
