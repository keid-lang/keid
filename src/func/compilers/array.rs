use crate::{
    common::{
        types::{BasicType, ComplexType, IntoOpaqueType},
        GenericIdentifier, Result, TypedValue,
    },
    compiler::llvm::Insn,
    func::{utils::FunctionCompilerUtils, FunctionCompiler},
};

use super::CallCompiler;

pub trait ArrayCompiler {
    fn compile_new_array(&mut self, element_type: &ComplexType, initial_value: &TypedValue, length: &TypedValue) -> Result<TypedValue>;
}

impl<'a> ArrayCompiler for FunctionCompiler<'a> {
    fn compile_new_array(&mut self, element_type: &ComplexType, initial_value: &TypedValue, length: &TypedValue) -> Result<TypedValue> {
        self.assert_assignable_to(&length.ty, &BasicType::USize.to_complex())?;

        let array_llvm_type = self.cpl.context.get_abi_array_data_type(element_type.as_llvm_type(self.cpl), &element_type.to_string());
        let data_ptr = self.heap_allocate(element_type.as_llvm_type(self.cpl), Some(length.val))?;
        self.emit(Insn::Memset(data_ptr, self.cpl.context.const_null(element_type.as_llvm_type(self.cpl)), length.val));

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
            self.call_function(
                fill_ref,
                &fill_impl,
                &[TypedValue::new(element_type.clone().to_array(), slice_ptr), initial_value.clone()],
            )?;
        }

        Ok(TypedValue {
            ty: ComplexType::Array(Box::new(element_type.clone())),
            val: slice_ptr,
        })
    }
}
