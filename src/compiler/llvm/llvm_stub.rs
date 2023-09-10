use super::Insn;
use crate::common::types::BasicType;
use crate::common::TypedValue;
use crate::compiler::{Compiler, Linkage};
use crate::tree::ast::Varargs;
use crate::tree::*;
use anyhow::Result;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Target {
    pub name: String,
}

impl Target {
    pub fn get_host_target_triple() -> &'static str {
        env!("TARGET")
    }

    pub fn get_available_targets() -> Vec<Target> {
        vec![Target {
            name: Self::get_host_target_triple().to_owned(),
        }]
    }

    pub fn get_from_name(name: &str) -> Option<Target> {
        if name == Self::get_host_target_triple() {
            Some(Target {
                name: name.to_owned(),
            })
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct OpaqueType;
#[derive(Clone, Copy, Debug)]
pub struct OpaqueFunctionType;
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct OpaqueValue;
#[derive(Clone, Copy, Debug)]
pub struct OpaqueFunctionValue;
#[derive(Clone, Copy, Debug)]
pub struct OpaqueBasicBlock;

impl OpaqueFunctionValue {
    pub fn to_value(self) -> OpaqueValue {
        OpaqueValue {}
    }
}

impl OpaqueValue {
    pub fn to_function(self) -> OpaqueFunctionValue {
        OpaqueFunctionValue {}
    }
}

pub struct GlobalVariable {
    pub name: String,
    pub ty: OpaqueType,
}

pub struct Context {
    pub target: LLVMTargetData,
    global_variables: Vec<GlobalVariable>,
}

pub fn initialize() {}

impl Context {
    pub fn new(target: LLVMTargetData) -> Context {
        Context {
            target,
            global_variables: Vec::new(),
        }
    }

    pub fn create_module(&self, _: &str, _: &str) -> Module {
        Module {
            global_variables: HashMap::new(),
        }
    }

    pub fn const_array(&self, _: OpaqueType, _: &[OpaqueValue]) -> OpaqueValue {
        OpaqueValue {}
    }

    pub fn const_struct(&self, _: OpaqueType, _: &mut [OpaqueValue]) -> OpaqueValue {
        OpaqueValue {}
    }

    pub fn const_get_element_ptr_dynamic(&self, _: OpaqueType, _: OpaqueValue, _: usize) -> OpaqueValue {
        OpaqueValue {}
    }

    pub fn const_get_array_element_ptr(&self, _: OpaqueType, _: OpaqueValue, _: usize) -> OpaqueValue {
        OpaqueValue {}
    }

    pub fn const_null(&self, _: OpaqueType) -> OpaqueValue {
        OpaqueValue {}
    }

    pub fn const_int(&self, _: OpaqueType, _: u64) -> OpaqueValue {
        OpaqueValue {}
    }

    pub fn const_unknown(&self) -> TypedValue {
        TypedValue::new(BasicType::Unknown.to_complex(), OpaqueValue {})
    }

    pub fn const_null_ptr(&self, _: OpaqueType) -> OpaqueValue {
        OpaqueValue {}
    }

    pub fn const_func_ptr(&self, _: OpaqueFunctionValue) -> OpaqueValue {
        OpaqueValue {}
    }

    pub fn const_bitcast(&self, _: OpaqueValue, _: OpaqueType) -> OpaqueValue {
        OpaqueValue {}
    }

    pub fn const_string(&self, _: &str) -> OpaqueValue {
        OpaqueValue {}
    }

    pub fn set_linkage(&self, _: OpaqueValue, _: Linkage) {}

    pub fn get_size_of(&self, _: OpaqueType) -> OpaqueValue {
        OpaqueValue {}
    }

    pub fn replace_all_uses(&self, _: OpaqueValue, _: OpaqueValue) {}

    pub fn get_void_type(&self) -> OpaqueType {
        OpaqueType {}
    }

    pub fn get_i1_type(&self) -> OpaqueType {
        OpaqueType {}
    }

    pub fn get_i8_type(&self) -> OpaqueType {
        OpaqueType {}
    }

    pub fn get_i16_type(&self) -> OpaqueType {
        OpaqueType {}
    }

    pub fn get_i32_type(&self) -> OpaqueType {
        OpaqueType {}
    }

    pub fn get_i64_type(&self) -> OpaqueType {
        OpaqueType {}
    }

    pub fn get_f32_type(&self) -> OpaqueType {
        OpaqueType {}
    }

    pub fn get_f64_type(&self) -> OpaqueType {
        OpaqueType {}
    }

    pub fn get_isize_type(&self) -> OpaqueType {
        OpaqueType {}
    }

    pub fn get_array_type(&self, _: OpaqueType, _: usize) -> OpaqueType {
        OpaqueType {}
    }

    pub fn get_abi_class_metadata_type(&self) -> OpaqueType {
        OpaqueType {}
    }

    pub fn get_abi_class_data_type(&self, _: &Compiler, _: &ResolvedClassNode) -> OpaqueType {
        OpaqueType {}
    }

    pub fn get_abi_class_info_type(&self) -> OpaqueType {
        OpaqueType {}
    }

    pub fn get_abi_interface_impl_type(&self) -> OpaqueType {
        OpaqueType {}
    }

    pub fn get_abi_nullable_type(&self, _: OpaqueType, _: &str) -> OpaqueType {
        OpaqueType {}
    }

    pub fn get_pointer_type(&self, _: OpaqueType) -> OpaqueType {
        OpaqueType {}
    }

    pub fn get_abi_enum_type_any_element(&self, _: &Compiler, _: &ResolvedEnumNode) -> OpaqueType {
        OpaqueType {}
    }

    pub fn get_abi_enum_type_specific_element(&self, _: &Compiler, _: &ResolvedEnumNode, _: usize) -> OpaqueType {
        OpaqueType {}
    }

    pub fn get_abi_array_data_type(&self, _: OpaqueType, _: &str) -> OpaqueType {
        OpaqueType {}
    }

    pub fn get_abi_slice_type(&self, _: OpaqueType, _: &str) -> OpaqueType {
        OpaqueType {}
    }

    pub fn get_abi_closure_type(&self, _: &[OpaqueType]) -> OpaqueType {
        OpaqueType {}
    }

    pub fn get_abi_any_closure_type(&self) -> OpaqueType {
        OpaqueType {}
    }

    pub fn get_struct_type(&self, _: &str, _: &[OpaqueType]) -> OpaqueType {
        OpaqueType {}
    }

    pub fn set_struct_type_body(&self, _: OpaqueType, _: &[OpaqueType]) {}

    pub fn get_function_type(&self, _: &[OpaqueType], _: Varargs, _: OpaqueType) -> OpaqueFunctionType {
        OpaqueFunctionType {}
    }

    pub fn get_global(&self, name: &str) -> Option<&GlobalVariable> {
        self.global_variables.iter().find(|var| var.name == name)
    }

    pub fn parse_llvm_ir(&self, _: &str, _: &str) -> Module {
        Module {
            global_variables: HashMap::new(),
        }
    }

    pub fn register_global(&mut self, global_name: &str, global_type: OpaqueType) {
        if self.get_global(global_name).is_some() {
            panic!("global already exists: {}", global_name);
        }

        self.global_variables.push(GlobalVariable {
            name: global_name.to_owned(),
            ty: global_type,
        });
    }
}

#[derive(Debug)]
pub struct Module {
    global_variables: HashMap<String, OpaqueValue>,
}

impl Clone for Module {
    fn clone(&self) -> Self {
        Module {
            global_variables: self.global_variables.clone(),
        }
    }
}

impl Module {
    pub fn add_function(&self, _: &str, _: OpaqueFunctionType, _: u32) -> Function {
        Function {}
    }

    pub fn create_global(&mut self, ctx: &mut Context, name: &str, ty: OpaqueType) -> OpaqueValue {
        let global = OpaqueValue {};

        self.global_variables.insert(name.to_owned(), global);
        ctx.register_global(name, ty);

        global
    }

    pub fn initialize_global(&self, _: OpaqueValue, _: OpaqueValue) {}

    pub fn get_or_extern_global(&mut self, global_var: &GlobalVariable) -> OpaqueValue {
        if let Some(val) = self.global_variables.get(&global_var.name) {
            *val
        } else {
            self.extern_global(global_var)
        }
    }

    pub fn extern_global(&mut self, global_var: &GlobalVariable) -> OpaqueValue {
        let global = OpaqueValue {};
        self.global_variables.insert(global_var.name.clone(), global);

        global
    }

    pub fn get_global(&self, name: &str) -> Option<OpaqueValue> {
        self.global_variables.get(name).cloned()
    }

    pub fn to_llvm_ir(&self) -> String {
        String::new()
    }

    pub fn verify(&self, module_name: &str) {}

    pub fn to_object_code(&self, _: &LLVMTargetData) -> Result<LLVMArray> {
        panic!("to_object_code is unsupported on this platform")
    }
}

#[derive(Clone)]
pub struct Function;

impl Function {
    pub fn create_builder(&self) -> InsnBuilder {
        InsnBuilder {
            is_clone: false,
        }
    }

    pub fn get_param(&self, _: u32) -> OpaqueValue {
        OpaqueValue {}
    }

    pub fn add_param_attribute(&self, _: usize, _: OpaqueType, _: &str) {}

    pub fn as_val(&self) -> OpaqueFunctionValue {
        OpaqueFunctionValue {}
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BuilderBlock;

impl BuilderBlock {
    pub fn null() -> BuilderBlock {
        BuilderBlock {}
    }

    pub fn append(&self) {}

    pub fn has_predecessor(&self) -> bool {
        false
    }

    pub fn as_val(&self) -> OpaqueBasicBlock {
        OpaqueBasicBlock {}
    }
}

#[derive(Clone)]
pub struct InsnBuilder {
    pub is_clone: bool,
}

impl InsnBuilder {
    pub fn create_block(&mut self) -> BuilderBlock {
        BuilderBlock {}
    }

    pub fn use_block_at_start(&self, _: &BuilderBlock) {}

    pub fn append_block(&self, _: &BuilderBlock) {}

    pub fn finish(&self) {}

    pub fn get_allocated_type(&self, _: OpaqueValue) -> OpaqueType {
        OpaqueType {}
    }

    pub fn add_call_site_attribute(&self, _: OpaqueValue, _: usize, _: OpaqueType, _: &str) {}

    pub fn emit(&self, _: Insn) -> OpaqueValue {
        OpaqueValue {}
    }
}

pub struct LLVMArray;

impl LLVMArray {
    pub fn as_slice(&self) -> &[u8] {
        &[]
    }
}

#[derive(Clone)]
pub struct LLVMTargetData {
    pub is_opaque_pointers: bool,
}

impl LLVMTargetData {
    pub fn new(_: &str, _: bool) -> Result<LLVMTargetData> {
        Ok(LLVMTargetData {
            is_opaque_pointers: true,
        })
    }

    pub fn is_llvm_ir(&self) -> bool {
        true
    }

    pub fn get_pointer_size(&self) -> u32 {
        8
    }

    pub fn get_type_size(&self, _: OpaqueType) -> u64 {
        8
    }
}
