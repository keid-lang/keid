use crate::{
    common::{
        types::{BasicType, ComplexType, IntoOpaqueType},
        GenericIdentifier, Result, TypedValue,
    },
    compiler::llvm::Insn,
    func::*,
    tree::ast::SubsliceExpr,
};

use super::{CallCompiler, ExprCompiler};

pub trait ArrayCompiler {
    fn compile_new_array(&mut self, element_type: &ComplexType, initial_value: &TypedValue, length: &TypedValue) -> Result<TypedValue>;

    fn compile_subslice(&mut self, original: &TypedValue, sublice: &SubsliceExpr) -> Result<TypedValue>;
}

impl<'a> ArrayCompiler for FunctionCompiler<'a> {
    fn compile_new_array(&mut self, element_type: &ComplexType, initial_value: &TypedValue, length: &TypedValue) -> Result<TypedValue> {
        self.assert_assignable_to(&length.ty, &BasicType::USize.to_complex())?;

        let array_llvm_type = self.cpl.context.get_abi_array_data_type(element_type.as_llvm_type(self.cpl), &element_type.to_string());
        let data_ptr = self.heap_allocate(element_type.as_llvm_type(self.cpl), Some(length.val))?;

        let element_size_const =
            self.cpl.context.const_int(BasicType::USize.as_llvm_type(&self.cpl), self.cpl.context.target.get_type_size(element_type.as_llvm_type(&self.cpl)));
        let total_length = self.emit(Insn::IMul(length.val, element_size_const));
        self.emit(Insn::Memset(data_ptr, self.cpl.context.const_int(self.cpl.context.get_i8_type(), 0), total_length));

        let metadata_ptr = self.heap_allocate(array_llvm_type, None)?;

        // set the ref count to 0
        let const_zero = self.cpl.context.const_int(self.cpl.context.get_isize_type(), 0);
        let ref_count_ptr = self.emit(Insn::GetElementPtr(metadata_ptr, array_llvm_type, 0));
        self.emit(Insn::Store(const_zero, ref_count_ptr));

        // pointer to the actual data within the array
        let array_data_ptr = self.emit(Insn::GetElementPtr(metadata_ptr, array_llvm_type, 1));
        self.emit(Insn::Store(data_ptr, array_data_ptr));

        let slice_llvm_type = self.cpl.context.get_abi_slice_type(element_type.as_llvm_type(self.cpl), &element_type.to_string());
        let slice_ptr = self.emit(Insn::Alloca(slice_llvm_type));

        // set the slice offset to the start of the array
        let offset_ptr = self.emit(Insn::GetElementPtr(slice_ptr, slice_llvm_type, 0));
        self.emit(Insn::Store(const_zero, offset_ptr));

        // set the slice length equal to the entire array length
        let length_ptr = self.emit(Insn::GetElementPtr(slice_ptr, slice_llvm_type, 1));
        self.emit(Insn::Store(length.val, length_ptr));

        // pointer to the array metadata
        let array_ptr = self.emit(Insn::GetElementPtr(slice_ptr, slice_llvm_type, 2));
        self.emit(Insn::Store(metadata_ptr, array_ptr));

        if initial_value.ty != BasicType::Null.to_complex() {
            self.assert_assignable_to(&initial_value.ty, element_type)?;

            let fill_impl = self
                .cpl
                .type_provider
                .get_function_by_name(
                    &GenericIdentifier::from_name_with_args("core::array::fill", &[element_type.clone()]),
                    &[element_type.clone().to_array(), element_type.clone()],
                )
                .expect("missing implementation of core::array::fill<T>");
            let fill_ref = self.get_function_ref(&fill_impl)?;
            self.call_function(fill_ref, &fill_impl, &[TypedValue::new(element_type.clone().to_array(), slice_ptr), initial_value.clone()])?;
        }

        Ok(TypedValue {
            ty: ComplexType::Array(Box::new(element_type.clone())),
            val: slice_ptr,
        })
    }

    fn compile_subslice(&mut self, original: &TypedValue, subslice: &SubsliceExpr) -> Result<TypedValue> {
        let element_type = match &original.ty {
            ComplexType::Array(element) => *element.clone(),
            _ => return Err(compiler_error!(self, "Cannot use subslice operator on value of non-slice type `{}`", original.ty.to_string())),
        };

        let offset = subslice.start.as_ref().map(|start| self.compile_expr(&start, Some(&BasicType::USize.to_complex()))).unwrap_or_else(|| {
            let const_zero = self.cpl.context.const_int(self.cpl.context.get_i64_type(), 0);
            Ok(TypedValue::new(BasicType::USize.to_complex(), const_zero))
        })?;
        self.assert_assignable_to(&offset.ty, &BasicType::USize.to_complex())?;

        let length = subslice.end.as_ref().map(|end| self.compile_expr(&end, Some(&BasicType::USize.to_complex()))).unwrap_or_else(|| {
            let current_length_ptr = self.emit(Insn::GetElementPtr(original.val, original.ty.as_llvm_type(&self.cpl), 1));
            let current_length = self.emit(Insn::Load(current_length_ptr, self.cpl.context.get_i64_type()));
            Ok(TypedValue::new(BasicType::USize.to_complex(), current_length))
        })?;
        self.assert_assignable_to(&length.ty, &BasicType::USize.to_complex())?;
        let adjusted_length = self.emit(Insn::ISub(length.val, offset.val));

        let slice_llvm_type = self.cpl.context.get_abi_slice_type(element_type.as_llvm_type(self.cpl), &element_type.to_string());
        let slice_ptr = self.emit(Insn::Alloca(slice_llvm_type));

        let supplied_offset_ptr = self.emit(Insn::GetElementPtr(original.val, slice_llvm_type, 0));
        let supplied_offset = self.emit(Insn::Load(supplied_offset_ptr, self.cpl.context.get_i64_type()));
        let total_offset = self.emit(Insn::IAdd(supplied_offset, offset.val));

        let offset_ptr = self.emit(Insn::GetElementPtr(slice_ptr, slice_llvm_type, 0));
        self.emit(Insn::Store(total_offset, offset_ptr));

        // set the slice length equal to (current length - offset)
        let length_ptr = self.emit(Insn::GetElementPtr(slice_ptr, slice_llvm_type, 1));
        self.emit(Insn::Store(adjusted_length, length_ptr));

        let original_array_ptr = self.emit(Insn::GetElementPtr(original.val, slice_llvm_type, 2));
        let original_array = self.emit(Insn::Load(
            original_array_ptr,
            self.cpl.context.get_pointer_type(self.cpl.context.get_abi_array_data_type(element_type.as_llvm_type(self.cpl), &element_type.to_string())),
        ));

        // pointer to the array metadata
        let array_ptr = self.emit(Insn::GetElementPtr(slice_ptr, slice_llvm_type, 2));
        self.emit(Insn::Store(original_array, array_ptr));

        Ok(TypedValue::new(original.ty.clone(), slice_ptr))
    }
}
