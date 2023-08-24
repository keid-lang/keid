#![allow(dead_code)]

use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::c_void;
use std::ffi::CStr;
use std::ffi::CString;
use std::path::PathBuf;
use std::slice;
use std::str::FromStr;
use std::usize;

use anyhow::anyhow;
use anyhow::Result;
use llvm_sys::analysis::*;
use llvm_sys::core::*;
use llvm_sys::debuginfo::*;
use llvm_sys::execution_engine::*;
use llvm_sys::ir_reader::*;
use llvm_sys::linker::LLVMLinkModules2;
use llvm_sys::prelude::*;
use llvm_sys::target::*;
use llvm_sys::target_machine::*;

use super::get_eval_only;
use super::Insn;
use super::Linkage;
use crate::common::types::BasicType;
use crate::common::types::IntoOpaqueType;
use crate::common::TypedValue;
use crate::compiler::Compiler;
use crate::tree::ast::Varargs;
use crate::tree::ClassType;
use crate::tree::ResolvedClassNode;
use crate::tree::ResolvedEnumNode;

#[derive(Debug)]
pub struct Target {
    pub name: String,
    tgt: LLVMTargetRef,
}

impl Target {
    pub fn get_host_target_triple() -> &'static str {
        unsafe { CStr::from_ptr(LLVMGetDefaultTargetTriple()).to_str().expect("invalid target triple") }
    }

    pub fn get_available_targets() -> Vec<Target> {
        unsafe {
            let mut targets = Vec::new();
            let mut current = LLVMGetFirstTarget();
            loop {
                if current.is_null() {
                    break;
                }
                let str = CStr::from_ptr(LLVMGetTargetName(current)).to_str().expect("invalid str");
                if LLVMTargetHasTargetMachine(current) != 0 {
                    targets.push(Target {
                        name: str.to_owned(),
                        tgt: current,
                    });
                }
                current = LLVMGetNextTarget(current);
            }
            targets
        }
    }

    pub fn get_from_name(name: &str) -> Option<Target> {
        unsafe {
            let c_name = CString::new(name).unwrap();
            let tgt = LLVMGetTargetFromName(c_name.as_ptr());
            if LLVMTargetHasTargetMachine(tgt) != 0 {
                Some(Target {
                    name: name.to_owned(),
                    tgt,
                })
            } else {
                None
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct OpaqueType(LLVMTypeRef);
#[derive(Clone, Copy, Debug)]
pub struct OpaqueFunctionType(LLVMTypeRef);
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct OpaqueValue(pub LLVMValueRef);
#[derive(Clone, Copy, Debug)]
pub struct OpaqueFunctionValue(LLVMValueRef);
#[derive(Clone, Copy, Debug)]
pub struct OpaqueBasicBlock(LLVMBasicBlockRef);

impl OpaqueFunctionType {
    pub fn to_any_type(self) -> OpaqueType {
        OpaqueType(self.0)
    }
}

impl OpaqueFunctionValue {
    pub fn to_value(self) -> OpaqueValue {
        OpaqueValue(self.0)
    }
}

impl OpaqueValue {
    pub fn to_function(self) -> OpaqueFunctionValue {
        OpaqueFunctionValue(self.0)
    }
}

pub struct GlobalVariable {
    pub name: String,
    pub ty: OpaqueType,
}

pub struct Context {
    ctx: LLVMContextRef,
    pub target: LLVMTargetData,
    cached_struct_types: RefCell<HashMap<String, OpaqueType>>,
    global_variables: Vec<GlobalVariable>,
}

pub fn initialize() {
    unsafe {
        LLVM_InitializeAllTargetInfos();
        LLVM_InitializeAllTargets();
        LLVM_InitializeAllTargetMCs();
        LLVM_InitializeAllAsmPrinters();
    }
}

fn get_attribute_by_name(attr: &str) -> u32 {
    unsafe {
        let attr_ptr = CString::new(attr).unwrap();
        let attr_id = LLVMGetEnumAttributeKindForName(attr_ptr.as_ptr(), attr.len());
        if attr_id == 0 {
            panic!("unknown attr: {}", attr);
        }
        attr_id
    }
}

impl Context {
    pub fn new(target: LLVMTargetData) -> Context {
        unsafe {
            let ctx = LLVMContextCreate();
            // LLVMContextSetOpaquePointers(ctx, i32::from(target.is_opaque_pointers));

            Context {
                ctx,
                target,
                cached_struct_types: RefCell::new(HashMap::new()),
                global_variables: Vec::new(),
            }
        }
    }

    fn get_debug_file(&self, di: LLVMDIBuilderRef, path: &str) -> LLVMMetadataRef {
        let source_path = std::fs::canonicalize(path).unwrap_or_else(|_| PathBuf::from_str(path).unwrap());
        let name = source_path.file_name().unwrap().to_str().unwrap();
        let name_cstr = CString::new(name).expect("invalid source path");
        let dir = source_path.parent().unwrap().to_str().unwrap();
        let dir_cstr = CString::new(dir).expect("invalid source path");

        unsafe { llvm_sys::debuginfo::LLVMDIBuilderCreateFile(di, name_cstr.as_ptr(), name.len(), dir_cstr.as_ptr(), dir.len()) }
    }

    pub fn create_module(&self, source_path: &str, name: &str) -> Module {
        unsafe {
            let name_cstr = CString::new(name).expect("invalid name");
            let mdl = LLVMModuleCreateWithNameInContext(name_cstr.as_ptr() as *const _, self.ctx);
            LLVMSetModuleIdentifier(mdl, name_cstr.as_ptr(), name.len());

            let debug = LLVMCreateDIBuilder(mdl);
            let debug_file = self.get_debug_file(debug, source_path);
            let debug_compile_unit = llvm_sys::debuginfo::LLVMDIBuilderCreateCompileUnit(
                debug,
                LLVMDWARFSourceLanguage::LLVMDWARFSourceLanguageC,
                debug_file,
                std::ptr::null(),
                0,
                i32::from(!self.target.is_debug),
                std::ptr::null(),
                0,
                0,
                std::ptr::null(),
                0,
                LLVMDWARFEmissionKind::LLVMDWARFEmissionKindFull,
                0,
                i32::from(true),
                i32::from(false),
                std::ptr::null(),
                0,
                std::ptr::null(),
                0,
            );

            Module {
                is_clone: false,
                ctx: self.ctx,
                debug,
                debug_file,
                debug_compile_unit,
                mdl,
                global_variables: HashMap::new(),
                target: self.target.clone(),
            }
        }
    }

    pub fn create_const_array(&self, element_type: OpaqueType, values: &[OpaqueValue]) -> OpaqueValue {
        unsafe {
            let mut inner_values: Vec<LLVMValueRef> = values.iter().map(|par| par.0).collect();
            let inner_values = inner_values.as_mut_ptr();
            OpaqueValue(LLVMConstArray(element_type.0, inner_values, values.len() as _))
        }
    }

    pub fn create_const_struct(&self, struct_type: OpaqueType, elements: &mut [OpaqueValue]) -> OpaqueValue {
        unsafe {
            let elements_len = elements.len() as u32;
            let elements_ptr = elements.as_mut_ptr();

            OpaqueValue(LLVMConstNamedStruct(struct_type.0, elements_ptr as *mut LLVMValueRef, elements_len))
        }
    }

    pub fn const_get_element_ptr_dynamic(&self, element_type: OpaqueType, val: OpaqueValue, index: usize) -> OpaqueValue {
        unsafe {
            let mut index_values: Vec<LLVMValueRef> = Vec::new();
            index_values.push(LLVMConstInt(self.get_i64_type().0, index as _, 0));

            OpaqueValue(LLVMConstGEP2(element_type.0, val.0, index_values.as_mut_ptr(), index_values.len() as _))
        }
    }

    pub fn const_null(&self, ty: OpaqueType) -> OpaqueValue {
        unsafe { OpaqueValue(LLVMConstNull(ty.0)) }
    }

    pub fn const_int(&self, ty: OpaqueType, val: u64) -> OpaqueValue {
        unsafe { OpaqueValue(LLVMConstInt(ty.0, val, 0)) }
    }

    pub fn const_unknown(&self) -> TypedValue {
        TypedValue::new(BasicType::Unknown.to_complex(), self.const_null_ptr(self.get_void_type()))
    }

    pub fn const_null_ptr(&self, ty: OpaqueType) -> OpaqueValue {
        unsafe { OpaqueValue(LLVMConstNull(self.get_pointer_type(ty).0)) }
    }

    pub fn const_func_ptr(&self, func: OpaqueFunctionValue) -> OpaqueValue {
        unsafe { OpaqueValue(LLVMConstBitCast(func.0, self.get_pointer_type(self.get_i8_type()).0)) }
    }

    pub fn const_bitcast(&self, val: OpaqueValue, ty: OpaqueType) -> OpaqueValue {
        unsafe { OpaqueValue(LLVMConstBitCast(val.0, ty.0)) }
    }

    pub fn const_string(&self, val: &str) -> OpaqueValue {
        unsafe {
            let null_str = format!("{}\0", val);
            let bytes = null_str.as_bytes();
            OpaqueValue(LLVMConstStringInContext(self.ctx, bytes.as_ptr() as _, val.len() as u32, 0))
        }
    }

    pub fn set_linkage(&self, val: OpaqueValue, linkage: Linkage) {
        unsafe { LLVMSetLinkage(val.0, linkage) }
    }

    pub fn get_size_of(&self, ty: OpaqueType) -> OpaqueValue {
        unsafe { OpaqueValue(LLVMSizeOf(ty.0)) }
    }

    pub fn replace_all_uses(&self, from: OpaqueValue, to: OpaqueValue) {
        if from.0 == to.0 {
            panic!("cannot replace value with itself");
        }
        unsafe { LLVMReplaceAllUsesWith(from.0, to.0) }
    }

    pub fn get_void_type(&self) -> OpaqueType {
        unsafe { OpaqueType(LLVMVoidTypeInContext(self.ctx)) }
    }

    pub fn get_i1_type(&self) -> OpaqueType {
        unsafe { OpaqueType(LLVMInt1TypeInContext(self.ctx)) }
    }

    pub fn get_i8_type(&self) -> OpaqueType {
        unsafe { OpaqueType(LLVMInt8TypeInContext(self.ctx)) }
    }

    pub fn get_i16_type(&self) -> OpaqueType {
        unsafe { OpaqueType(LLVMInt16TypeInContext(self.ctx)) }
    }

    pub fn get_i32_type(&self) -> OpaqueType {
        unsafe { OpaqueType(LLVMInt32TypeInContext(self.ctx)) }
    }

    pub fn get_i64_type(&self) -> OpaqueType {
        unsafe { OpaqueType(LLVMInt64TypeInContext(self.ctx)) }
    }

    pub fn get_f32_type(&self) -> OpaqueType {
        unsafe { OpaqueType(LLVMFloatTypeInContext(self.ctx)) }
    }

    pub fn get_f64_type(&self) -> OpaqueType {
        unsafe { OpaqueType(LLVMDoubleTypeInContext(self.ctx)) }
    }

    pub fn get_isize_type(&self) -> OpaqueType {
        match self.target.get_pointer_size() {
            1 => self.get_i8_type(),
            2 => self.get_i16_type(),
            4 => self.get_i32_type(),
            8 => self.get_i64_type(),
            other => panic!("invalid pointer size: {}", other),
        }
    }

    pub fn get_array_type(&self, element_type: OpaqueType, length: usize) -> OpaqueType {
        unsafe { OpaqueType(LLVMArrayType(element_type.0, length as _)) }
    }

    /// Represents just the metadata of a class without any fields.
    /// Only the referecence count and class info pointer are defined by this type.
    pub fn get_abi_class_metadata_type(&self) -> OpaqueType {
        self.get_struct_type("KeidAbiClass", &[self.get_isize_type(), self.get_pointer_type(self.get_abi_class_info_type())])
    }

    /// Returns a type that contains the 2-byte enum variant ID, followed by `n` bytes of padding,
    /// where `n` equals the size of the largest possible variant data.
    /// This ensures that any variant of the given type will fit into the returned type.
    pub fn get_abi_enum_type_any_element(&self, cpl: &Compiler, enum_impl: &ResolvedEnumNode) -> OpaqueType {
        let abi_name = &enum_impl.full_name;
        if let Some(existing_type) = self.get_cached_struct_type(&abi_name) {
            return existing_type;
        }

        let struct_type = self.get_struct_type(&abi_name, &[]);
        let mut field_types = Vec::with_capacity(10);
        field_types.push(self.get_pointer_type(self.get_abi_class_info_type())); // class info pointer
        field_types.push(self.get_i32_type()); // variant ID
        self.set_struct_type_body(struct_type, &field_types);

        let mut biggest_struct_size = 0;
        for element in &enum_impl.elements {
            if let Some(data) = &element.data {
                let current_struct_size = data.iter().map(|field| self.target.get_type_size(field.ty.as_llvm_type(cpl))).sum();
                if current_struct_size > biggest_struct_size {
                    biggest_struct_size = current_struct_size;
                }
            }
        }

        while biggest_struct_size >= 8 {
            field_types.push(self.get_i64_type());
            biggest_struct_size -= 8;
        }

        if biggest_struct_size > 0 {
            let padding_type = match biggest_struct_size {
                1 => self.get_i8_type(),
                2 => self.get_i16_type(),
                4 => self.get_i32_type(),
                3 | 5..=7 => self.get_array_type(self.get_i8_type(), biggest_struct_size as _),
                _ => unreachable!(),
            };
            field_types.push(padding_type);
        }

        self.set_struct_type_body(struct_type, &field_types);

        struct_type
    }

    /// Returns a type that contains the 2-byte enum variant ID,
    /// followed by the associated fields of that element (if any exist),
    /// then followed by padding until the length of the largest element is reached.
    pub fn get_abi_enum_type_specific_element(&self, cpl: &Compiler, enum_impl: &ResolvedEnumNode, id: usize) -> OpaqueType {
        let abi_name = format!("{}.{}", enum_impl.full_name, enum_impl.elements[id].name);
        if let Some(existing_type) = self.get_cached_struct_type(&abi_name) {
            return existing_type;
        }

        let struct_type = self.get_struct_type(&abi_name, &[]);
        let mut field_types = Vec::with_capacity(10);
        field_types.push(self.get_pointer_type(self.get_abi_class_info_type())); // class info pointer
        field_types.push(self.get_i32_type()); // variant ID

        let this_element = &enum_impl.elements[id];
        if let Some(fields) = &this_element.data {
            for field in fields {
                let ty = field.ty.as_llvm_type(cpl);
                field_types.push(ty);
            }
        }
        self.set_struct_type_body(struct_type, &field_types);
        let unpadded_size = self.target.get_type_size(struct_type);

        let pad_length = self.target.get_type_size(self.get_abi_enum_type_any_element(cpl, enum_impl));
        if pad_length > unpadded_size {
            let mut padding_size = pad_length - unpadded_size;
            while padding_size >= 8 {
                field_types.push(self.get_i64_type());
                padding_size -= 8;
            }
            if padding_size > 0 {
                let padding_type = match padding_size {
                    1 => self.get_i8_type(),
                    2 => self.get_i16_type(),
                    4 => self.get_i32_type(),
                    3 | 5..=7 => self.get_array_type(self.get_i8_type(), padding_size as _),
                    _ => unreachable!(),
                };
                field_types.push(padding_type);
            }
        }
        self.set_struct_type_body(struct_type, &field_types);

        struct_type
    }

    pub fn get_abi_class_data_type(&self, cpl: &Compiler, class_impl: &ResolvedClassNode) -> OpaqueType {
        match class_impl.class_type {
            ClassType::Class | ClassType::Interface => {
                let abi_name = &class_impl.full_name;
                if let Some(existing_type) = self.get_cached_struct_type(&abi_name) {
                    return existing_type;
                }
                let struct_type = self.get_struct_type(&abi_name, &[]);

                let ref_count_type = self.get_isize_type();
                let info_type = self.get_pointer_type(self.get_abi_class_info_type());
                let mut field_types = Vec::with_capacity(class_impl.fields.len());
                field_types.push(info_type);
                field_types.push(ref_count_type);
                for field in &class_impl.fields {
                    field_types.push(field.ty.as_llvm_type(cpl));
                }

                self.set_struct_type_body(struct_type, &field_types);

                struct_type
            }
            ClassType::Struct => {
                let abi_name = &class_impl.full_name;
                if let Some(existing_type) = self.get_cached_struct_type(&abi_name) {
                    return existing_type;
                }
                let struct_type = self.get_struct_type(&abi_name, &[]);

                let info_type = self.get_pointer_type(self.get_abi_class_info_type());
                let mut field_types = Vec::with_capacity(class_impl.fields.len() + 1);
                field_types.push(info_type);
                for field in &class_impl.fields {
                    field_types.push(field.ty.as_llvm_type(cpl));
                }

                self.set_struct_type_body(struct_type, &field_types);

                struct_type
            }
            ClassType::Enum => {
                panic!("no get_abi_class_data_type for enums (use get_abi_enum_type_any_element)")
            }
        }
    }

    pub fn get_abi_class_info_type(&self) -> OpaqueType {
        self.get_struct_type(
            "KeidAbiClassInfo",
            &[
                self.get_pointer_type(self.get_i8_type()),                 // destructor
                self.get_pointer_type(self.get_i8_type()),                 // vtable
                self.get_i32_type(),                                       // interfaces length
                self.get_pointer_type(self.get_abi_interface_impl_type()), // interfaces
                self.get_pointer_type(self.get_i8_type()),                 // class name
                self.get_i32_type(),                                       // class bitflags
            ],
        )
    }

    pub fn get_abi_interface_impl_type(&self) -> OpaqueType {
        self.get_struct_type(
            "KeidAbiInterfaceImpl",
            &[
                self.get_i32_type(),                                                                        // interface ID
                self.get_pointer_type(self.get_i8_type()),                                                  // interface name
                self.get_pointer_type(self.get_array_type(self.get_pointer_type(self.get_void_type()), 0)), // &[&void]; pointer to array of void pointers
            ],
        )
    }

    pub fn get_abi_nullable_type(&self, item_type: OpaqueType, item_type_name: &str) -> OpaqueType {
        self.get_struct_type(&format!("?{}", item_type_name), &[item_type, self.get_i8_type()])
    }

    pub fn get_pointer_type(&self, pointee_type: OpaqueType) -> OpaqueType {
        unsafe { OpaqueType(LLVMPointerType(pointee_type.0, 0)) }
    }

    pub fn get_byval_pointer_type(&self, pointee_type: OpaqueType) -> OpaqueType {
        unsafe { OpaqueType(LLVMPointerType(pointee_type.0, 0)) }
    }

    pub fn get_abi_array_data_type(&self, element_type: OpaqueType, element_type_name: &str) -> OpaqueType {
        // TODO: include class info pointer here
        // arrays of primitive types should be able to identify that as well
        self.get_struct_type(
            &format!("[{}]#Heap", element_type_name),
            &[
                self.get_isize_type(),               // ref count
                self.get_pointer_type(element_type), // array data
            ],
        )
    }

    pub fn get_abi_slice_type(&self, element_type: OpaqueType, element_type_name: &str) -> OpaqueType {
        self.get_struct_type(
            &format!("[{}]#Slice", element_type_name),
            &[
                self.get_isize_type(),                                                                // data offset
                self.get_isize_type(),                                                                // data length
                self.get_pointer_type(self.get_abi_array_data_type(element_type, element_type_name)), // array data
            ],
        )
    }

    pub fn get_abi_closure_type(&self, captured_value_types: &[OpaqueType]) -> OpaqueType {
        let mut hash = 7;
        for captured_value in captured_value_types {
            hash = 31 * hash + captured_value.0 as usize;
        }

        self.get_struct_type(
            &format!("__closure${}", hex::encode(&hash.to_be_bytes())),
            &[
                &[
                    self.get_isize_type(),                       // ref count
                    self.get_pointer_type(self.get_void_type()), // function pointer
                ],
                captured_value_types,
            ]
            .concat(),
        )
    }

    pub fn get_abi_any_closure_type(&self) -> OpaqueType {
        self.get_struct_type(
            &format!("__closure"),
            &[
                self.get_isize_type(),                       // ref count
                self.get_pointer_type(self.get_void_type()), // function pointer
            ],
        )
    }

    fn get_cached_struct_type(&self, name: &str) -> Option<OpaqueType> {
        let cached_struct_types = self.cached_struct_types.borrow();
        cached_struct_types.get(name).cloned()
    }

    pub fn get_struct_type(&self, name: &str, element_types: &[OpaqueType]) -> OpaqueType {
        let mut cached_struct_types = self.cached_struct_types.borrow_mut();
        if let Some(cached) = cached_struct_types.get(name) {
            return *cached;
        }

        unsafe {
            let c_name = CString::new(name).expect("invalid name");
            let struct_type = LLVMStructCreateNamed(self.ctx, c_name.as_ptr());

            let element_count = element_types.len() as u32;
            let element_types = element_types.as_ptr();

            LLVMStructSetBody(struct_type, element_types as *mut LLVMTypeRef, element_count, 0);

            let ty = OpaqueType(struct_type);
            cached_struct_types.insert(name.to_owned(), ty);
            ty
        }
    }

    pub fn set_struct_type_body(&self, struct_type: OpaqueType, element_types: &[OpaqueType]) {
        unsafe {
            let element_count = element_types.len() as u32;
            let element_types = element_types.as_ptr();

            LLVMStructSetBody(struct_type.0, element_types as *mut LLVMTypeRef, element_count, 0);
        }
    }

    pub fn get_function_type(&self, params: &[OpaqueType], varargs: Varargs, return_type: OpaqueType) -> OpaqueFunctionType {
        unsafe {
            let mut inner_params: Vec<LLVMTypeRef> = params.iter().map(|par| par.0).collect();
            let inner_params = inner_params.as_mut_slice();
            let params_ptr = if params.is_empty() {
                std::ptr::null_mut()
            } else {
                inner_params.as_mut_ptr()
            };
            OpaqueFunctionType(LLVMFunctionType(return_type.0, params_ptr, params.len() as u32, i32::from(varargs == Varargs::Native)))
        }
    }

    pub fn get_global(&self, name: &str) -> Option<&GlobalVariable> {
        self.global_variables.iter().find(|var| var.name == name)
    }

    pub fn parse_llvm_ir(&self, ir: &str, name: &str) -> Module {
        unsafe {
            // let input = ir.as_ptr() as *mut i8;

            let input = CString::new(ir).unwrap();
            let input_length = ir.len();

            let buf_name = CString::new(name).unwrap();
            let mem_buf = LLVMCreateMemoryBufferWithMemoryRange(input.as_ptr() as *const _, input_length, buf_name.as_ptr() as *const _, 0);

            let mut out_mod: LLVMModuleRef = std::ptr::null_mut();
            let mut out_msg = std::ptr::null_mut();
            if LLVMParseIRInContext(self.ctx, mem_buf, &mut out_mod, &mut out_msg) != 0 {
                let msg = CStr::from_ptr(out_msg);
                let msg = msg.to_str().unwrap();
                panic!("failed to parse IR: {}", msg)
            }

            Module {
                is_clone: false,
                debug: std::ptr::null_mut(),
                ctx: self.ctx,
                mdl: out_mod,
                global_variables: HashMap::new(),
                debug_file: std::ptr::null_mut(),
                debug_compile_unit: std::ptr::null_mut(),
                target: self.target.clone(),
            }
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

impl Drop for Context {
    fn drop(&mut self) {
        // unsafe {
        //     LLVMContextDispose(self.ctx);
        // }
    }
}

#[derive(Debug)]
pub struct Module {
    ctx: LLVMContextRef,
    mdl: LLVMModuleRef,
    debug_compile_unit: LLVMMetadataRef,
    debug_file: LLVMMetadataRef,
    debug: LLVMDIBuilderRef,
    global_variables: HashMap<String, OpaqueValue>,
    is_clone: bool,
    target: LLVMTargetData,
}

impl Drop for Module {
    fn drop(&mut self) {
        // if !self.is_clone {
        //     unsafe { LLVMDisposeDIBuilder(self.debug) }
        // }
    }
}

impl Clone for Module {
    fn clone(&self) -> Self {
        Module {
            ctx: self.ctx,
            mdl: self.mdl,
            debug: self.debug,
            global_variables: self.global_variables.clone(),
            debug_compile_unit: self.debug_compile_unit,
            debug_file: self.debug_file,
            is_clone: true,
            target: self.target.clone(),
        }
    }
}

impl Module {
    pub fn link_into(self, dest: &Module) {
        unsafe {
            if LLVMLinkModules2(dest.mdl, self.mdl) != 0 {
                eprintln!("LLVMLinkModules2 failed");
                std::process::exit(1);
            }
        }
    }

    pub fn as_val(&self) -> LLVMModuleRef {
        self.mdl
    }

    pub fn lookup_intrinsic(&self, name: &str, params: &[OpaqueType]) -> Option<OpaqueFunctionValue> {
        unsafe {
            let name_len = name.len();
            let name = CString::new(name).unwrap();
            let id = LLVMLookupIntrinsicID(name.as_ptr() as _, name_len as _);

            let mut params: Vec<LLVMTypeRef> = params.iter().map(|par| par.0).collect();
            let params = params.as_mut_slice();
            Some(OpaqueFunctionValue(LLVMGetIntrinsicDeclaration(self.mdl, id, params.as_mut_ptr(), params.len())))
        }
    }

    pub fn add_function(&self, name: &str, func_type: OpaqueFunctionType, _line_no: u32) -> Function {
        unsafe {
            let name_cstr = CString::new(name).expect("invalid name");

            // let debug_function_type = llvm_sys::debuginfo::LLVMDIBuilderCreateSubroutineType(
            //     self.debug,
            //     self.debug_file,
            //     parameter_types.as_mut_ptr(),
            //     parameter_types.len() as _,
            //     0,
            // );
            // let debug_function = llvm_sys::debuginfo::LLVMDIBuilderCreateFunction(
            //     self.debug,
            //     self.debug_compile_unit,
            //     name_cstr.as_ptr(),
            //     name.len(),
            //     std::ptr::null(),
            //     0,
            //     self.debug_file,
            //     line_no,
            //     debug_function_type,
            //     IsLocalToUnit,
            //     IsDefinition,
            //     ScopeLine,
            //     Flags,
            //     IsOptimized,
            // );

            Function {
                ctx: self.ctx,
                mdl: self.mdl,
                func: LLVMAddFunction(self.mdl, name_cstr.as_ptr() as *const _, func_type.0),
                debug: self.debug,
                debug_file: self.debug_file,
                target: self.target.clone(),
            }
        }
    }

    pub fn create_global(&mut self, ctx: &mut Context, name: &str, ty: OpaqueType) -> OpaqueValue {
        unsafe {
            let cstr_name = CString::new(name).expect("invalid name");
            let global = OpaqueValue(LLVMAddGlobal(self.mdl, ty.0, cstr_name.as_ptr() as *const _));

            // LLVMSetGlobalConstant(global.0, i32::from(true));

            self.global_variables.insert(name.to_owned(), global);
            ctx.register_global(name, ty);

            global
        }
    }

    pub fn initialize_global(&self, global: OpaqueValue, value: OpaqueValue) {
        unsafe {
            LLVMSetInitializer(global.0, value.0);
        }
    }

    pub fn get_or_extern_global(&mut self, global_var: &GlobalVariable) -> OpaqueValue {
        if let Some(val) = self.global_variables.get(&global_var.name) {
            *val
        } else {
            self.extern_global(global_var)
        }
    }

    pub fn extern_global(&mut self, global_var: &GlobalVariable) -> OpaqueValue {
        unsafe {
            let cstr_name = CString::new(global_var.name.clone()).expect("invalid name");
            let global = OpaqueValue(LLVMAddGlobal(self.mdl, global_var.ty.0, cstr_name.as_ptr() as *const _));

            self.global_variables.insert(global_var.name.clone(), global);

            global
        }
    }

    pub fn get_global(&self, name: &str) -> Option<OpaqueValue> {
        self.global_variables.get(name).cloned()
    }

    pub fn to_llvm_ir(&self) -> String {
        unsafe {
            let as_str = LLVMPrintModuleToString(self.mdl);
            let cstr = CStr::from_ptr(as_str);
            let owned = cstr.to_str().expect("invalid cstr").to_owned();
            LLVMDisposeMessage(as_str);
            owned
        }
    }

    pub fn verify(&self, module_name: &str) {
        unsafe {
            let mut error_message: *mut _ = std::ptr::null_mut();
            if LLVMVerifyModule(self.mdl, LLVMVerifierFailureAction::LLVMReturnStatusAction, &mut error_message) != 0 {
                let cstr = CStr::from_ptr(error_message);
                let error = String::from_utf8_lossy(cstr.to_bytes()).to_string();
                eprintln!("[Module `{}`] Error during module verification: {}", module_name, error);
                LLVMDisposeMessage(error_message);
            }
        }
    }

    pub fn to_object_code(&self, target: &LLVMTargetData) -> Result<LLVMArray> {
        unsafe {
            let pm = PassManager::new(0);
            pm.run(self.mdl);

            let mut error_message: *mut _ = std::ptr::null_mut();
            let mut buf: LLVMMemoryBufferRef = std::ptr::null_mut();
            if LLVMTargetMachineEmitToMemoryBuffer(
                target.machine,
                self.mdl,
                LLVMCodeGenFileType::LLVMObjectFile,
                &mut error_message,
                &mut buf,
            ) != 0
            {
                return Err(anyhow!("error while emitting to memory buffer"));
            }

            let buf_start = LLVMGetBufferStart(buf);
            let buf_len = LLVMGetBufferSize(buf);

            Ok(LLVMArray {
                buf,
                data_ptr: buf_start as *mut _,
                size: buf_len,
            })
        }
    }
}

#[derive(Clone)]
pub struct Function {
    ctx: LLVMContextRef,
    mdl: LLVMModuleRef,
    func: LLVMValueRef,
    debug: LLVMDIBuilderRef,
    debug_file: LLVMMetadataRef,
    target: LLVMTargetData,
}

impl Function {
    pub fn create_builder(&self) -> InsnBuilder {
        unsafe {
            InsnBuilder {
                ctx: self.ctx,
                bdl: LLVMCreateBuilderInContext(self.ctx),
                func: self.func,
                block_offset: 0,
                debug: self.debug,
                debug_file: self.debug_file,
                target: self.target.clone(),
                is_clone: false,
            }
        }
    }

    pub fn get_param(&self, idx: u32) -> OpaqueValue {
        unsafe { OpaqueValue(LLVMGetParam(self.func, idx)) }
    }

    /// Adds a type attribute to the given parameter index.
    /// Parameter Index 0 is for the return type.
    /// Parmeters 1..Inf are for parameters.
    pub fn add_param_attribute(&self, param_idx: usize, param_ty: OpaqueType, attr: &str) {
        unsafe {
            if attr == "byval" {
                let func_attr = LLVMCreateEnumAttribute(self.ctx, get_attribute_by_name("align"), self.target.get_pointer_size() as _);
                LLVMAddAttributeAtIndex(self.func, param_idx as _, func_attr);
            }

            let func_attr = LLVMCreateTypeAttribute(self.ctx, get_attribute_by_name(attr), param_ty.0);
            LLVMAddAttributeAtIndex(self.func, param_idx as _, func_attr);
        }
    }

    pub fn as_val(&self) -> OpaqueFunctionValue {
        OpaqueFunctionValue(self.func)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BuilderBlock {
    func: LLVMValueRef,
    block: LLVMBasicBlockRef,
}

impl BuilderBlock {
    pub fn null() -> BuilderBlock {
        BuilderBlock {
            func: std::ptr::null_mut(),
            block: std::ptr::null_mut(),
        }
    }

    pub fn as_val(&self) -> OpaqueBasicBlock {
        OpaqueBasicBlock(self.block)
    }
}

pub struct InsnBuilder {
    ctx: LLVMContextRef,
    bdl: LLVMBuilderRef,
    func: LLVMValueRef,
    block_offset: usize,
    debug: LLVMDIBuilderRef,
    debug_file: LLVMMetadataRef,
    target: LLVMTargetData,
    pub is_clone: bool,
}

impl InsnBuilder {
    pub fn create_block(&mut self) -> BuilderBlock {
        unsafe {
            let block_name = format!("block.{}", self.block_offset);
            let block_name = CString::new(block_name).expect("invalid block name");
            let block_name = block_name.as_ptr();
            self.block_offset += 1;

            BuilderBlock {
                func: self.func,
                block: LLVMCreateBasicBlockInContext(self.ctx, block_name),
            }
        }
    }

    pub fn use_block_at_start(&self, block: &BuilderBlock) {
        if get_eval_only() {
            return;
        }
        unsafe {
            let first_insn = LLVMGetFirstInstruction(block.block);
            LLVMPositionBuilderBefore(self.bdl, first_insn);
        }
    }

    pub fn finish(&self) {
        unsafe {
            let current_block = LLVMGetInsertBlock(self.bdl);
            if !current_block.is_null() {
                if LLVMGetFirstInstruction(current_block).is_null() {
                    LLVMDeleteBasicBlock(current_block);
                }
            }
        }
    }

    pub fn append_block(&self, block: &BuilderBlock) {
        if get_eval_only() {
            return;
        }
        unsafe {
            LLVMAppendExistingBasicBlock(self.func, block.block);

            // if the current block is empty, then insert a break to jump to the current block
            // this guarantees that no blocks will ever be empty
            let current_block = LLVMGetInsertBlock(self.bdl);
            if !current_block.is_null() {
                let first = LLVMGetFirstInstruction(current_block);
                if first.is_null() {
                    self.emit(Insn::Br(block.as_val()));
                }
            }

            LLVMPositionBuilderAtEnd(self.bdl, block.block);
        }
    }

    pub fn get_allocated_type(&self, var: OpaqueValue) -> OpaqueType {
        unsafe { OpaqueType(LLVMGetAllocatedType(var.0)) }
    }

    pub fn emit(&self, insn: Insn) -> OpaqueValue {
        if get_eval_only() {
            return OpaqueValue(0xDEADBEEFusize as *mut _);
        }
        unsafe {
            let insn_name = CString::new("").expect("invalid insn name");
            let insn_name = insn_name.as_ptr();
            let ret = match insn {
                Insn::Unreachable => LLVMBuildUnreachable(self.bdl),
                Insn::Nop => panic!("attempted emit of nop"),
                Insn::RetVoid => LLVMBuildRetVoid(self.bdl),
                Insn::Ret(value) => LLVMBuildRet(self.bdl, value.0),
                Insn::Alloca(ty) => LLVMBuildAlloca(self.bdl, ty.0, insn_name),
                Insn::GlobalString(str) => {
                    let bytes = str.as_bytes();
                    let mut nul_terminated = vec![0; bytes.len() + 1];
                    for i in 0..bytes.len() {
                        nul_terminated[i] = bytes[i];
                    }

                    LLVMBuildGlobalStringPtr(self.bdl, nul_terminated.as_slice().as_ptr() as *const _, insn_name)
                }
                Insn::Store(src, dest) => LLVMBuildStore(self.bdl, src.0, dest.0),
                Insn::Load(src, ty) => LLVMBuildLoad2(self.bdl, ty.0, src.0, insn_name),
                Insn::Call(func, ty, args) => {
                    let mut args: Vec<LLVMValueRef> = args.iter().map(|arg| arg.0).collect();
                    let args = args.as_mut_slice();
                    LLVMBuildCall2(self.bdl, ty.0, func.0, args.as_mut_ptr(), args.len() as u32, insn_name)
                }
                Insn::GetElementPtr(struct_ref, value_type, value_idx) => {
                    LLVMBuildStructGEP2(self.bdl, value_type.0, struct_ref.0, value_idx, insn_name)
                }
                Insn::GetElementPtrDynamic(struct_ref, value_type, value_idx) => {
                    let indices = &mut [value_idx.0];
                    LLVMBuildInBoundsGEP2(self.bdl, value_type.0, struct_ref.0, indices.as_mut_ptr(), 1, insn_name)
                }
                Insn::CondBr(test, then, otherwise) => {
                    let current = LLVMGetInsertBlock(self.bdl);
                    if then.0 == current {
                        panic!("CondBr primary target attempted to reference current block, causing infinite recursion")
                    }
                    if otherwise.0 == current {
                        panic!("CondBr secondary target attempted to reference current block, causing infinite recursion")
                    }
                    LLVMBuildCondBr(self.bdl, test.0, then.0, otherwise.0)
                }
                Insn::Br(target) => LLVMBuildBr(self.bdl, target.0),
                Insn::ICmp(op, lhs, rhs) => LLVMBuildICmp(self.bdl, op, lhs.0, rhs.0, insn_name),
                Insn::Xor(lhs, rhs) => LLVMBuildXor(self.bdl, lhs.0, rhs.0, insn_name),
                Insn::Trunc(val, cast) => LLVMBuildTrunc(self.bdl, val.0, cast.0, insn_name),
                Insn::IAdd(lhs, rhs) => LLVMBuildAdd(self.bdl, lhs.0, rhs.0, insn_name),
                Insn::IAnd(lhs, rhs) => LLVMBuildAnd(self.bdl, lhs.0, rhs.0, insn_name),
                Insn::IOr(lhs, rhs) => LLVMBuildOr(self.bdl, lhs.0, rhs.0, insn_name),
                Insn::ISub(lhs, rhs) => LLVMBuildSub(self.bdl, lhs.0, rhs.0, insn_name),
                Insn::IMul(lhs, rhs) => LLVMBuildMul(self.bdl, lhs.0, rhs.0, insn_name),
                Insn::SDiv(lhs, rhs) => LLVMBuildSDiv(self.bdl, lhs.0, rhs.0, insn_name),
                Insn::SRem(lhs, rhs) => LLVMBuildSRem(self.bdl, lhs.0, rhs.0, insn_name),
                Insn::UDiv(lhs, rhs) => LLVMBuildUDiv(self.bdl, lhs.0, rhs.0, insn_name),
                Insn::URem(lhs, rhs) => LLVMBuildURem(self.bdl, lhs.0, rhs.0, insn_name),
                Insn::FAdd(lhs, rhs) => LLVMBuildFAdd(self.bdl, lhs.0, rhs.0, insn_name),
                Insn::FSub(lhs, rhs) => LLVMBuildFSub(self.bdl, lhs.0, rhs.0, insn_name),
                Insn::FMul(lhs, rhs) => LLVMBuildFMul(self.bdl, lhs.0, rhs.0, insn_name),
                Insn::FDiv(lhs, rhs) => LLVMBuildFDiv(self.bdl, lhs.0, rhs.0, insn_name),
                Insn::PointerCast(value, ty) => LLVMBuildPointerCast(self.bdl, value.0, ty.0, insn_name),
                Insn::Memset(ptr, val, len) => LLVMBuildMemSet(self.bdl, ptr.0, val.0, len.0, self.target.get_pointer_size() as _),
                Insn::Memmove(src, dst, count) => {
                    LLVMBuildMemMove(self.bdl, dst.0, self.target.get_pointer_size(), src.0, self.target.get_pointer_size(), count.0)
                }
                Insn::PtrToInt(ptr) => {
                    let size_type = LLVMInt64TypeInContext(self.ctx); // TODO make pointer size platform dependent
                    LLVMBuildPtrToInt(self.bdl, ptr.0, size_type, insn_name)
                }
                Insn::IntToPtr(int, ty) => LLVMBuildIntToPtr(self.bdl, int.0, ty.0, insn_name),
                Insn::BitCast(val, ty) => LLVMBuildBitCast(self.bdl, val.0, ty.0, insn_name),
                Insn::IntCast(val, ty, is_signed) => LLVMBuildIntCast2(self.bdl, val.0, ty.0, i32::from(is_signed), insn_name),
                Insn::FloatCast(val, ty) => LLVMBuildFPCast(self.bdl, val.0, ty.0, insn_name),
                Insn::IntToFloat(val, ty, is_signed) => {
                    if is_signed {
                        LLVMBuildSIToFP(self.bdl, val.0, ty.0, insn_name)
                    } else {
                        LLVMBuildUIToFP(self.bdl, val.0, ty.0, insn_name)
                    }
                }
                Insn::FloatToInt(val, ty, is_signed) => {
                    if is_signed {
                        LLVMBuildFPToSI(self.bdl, val.0, ty.0, insn_name)
                    } else {
                        LLVMBuildFPToUI(self.bdl, val.0, ty.0, insn_name)
                    }
                }
            };

            // let loc = llvm_sys::debuginfo::LLVMDIBuilderCreateDebugLocation(self.ctx, line, col, self.debug_file, std::ptr::null_mut());
            // llvm_sys::debuginfo::LLVMInstructionSetDebugLoc(ret, loc);

            OpaqueValue(ret)
        }
    }

    /// Adds a type attribute to the given parameter index.
    /// Parameter Index 0 is for the return type.
    /// Parmeters 1..Inf are for parameters.
    pub fn add_call_site_attribute(&self, call_site: OpaqueValue, param_idx: usize, param_ty: OpaqueType, attr: &str) {
        if get_eval_only() {
            return;
        }
        unsafe {
            if attr == "byval" {
                let func_attr = LLVMCreateEnumAttribute(self.ctx, get_attribute_by_name("align"), self.target.get_pointer_size() as _);
                LLVMAddCallSiteAttribute(call_site.0, param_idx as _, func_attr);
            }

            let func_attr = LLVMCreateTypeAttribute(self.ctx, get_attribute_by_name(attr), param_ty.0);
            LLVMAddCallSiteAttribute(call_site.0, param_idx as _, func_attr);
        }
    }
}

impl Clone for InsnBuilder {
    fn clone(&self) -> InsnBuilder {
        InsnBuilder {
            bdl: self.bdl,
            block_offset: self.block_offset,
            ctx: self.ctx,
            debug: self.debug,
            debug_file: self.debug_file,
            func: self.func,
            target: self.target.clone(),
            is_clone: true,
        }
    }
}

impl Drop for InsnBuilder {
    fn drop(&mut self) {
        if !self.is_clone {
            unsafe { LLVMDisposeBuilder(self.bdl) }
        }
    }
}

pub struct LLVMArray {
    buf: LLVMMemoryBufferRef,
    data_ptr: *mut u8,
    size: usize,
}

impl LLVMArray {
    pub fn as_slice(&self) -> &[u8] {
        unsafe { slice::from_raw_parts_mut(self.data_ptr, self.size) }
    }
}

impl Drop for LLVMArray {
    fn drop(&mut self) {
        unsafe { LLVMDisposeMemoryBuffer(self.buf) }
    }
}

#[derive(Debug)]
pub struct LLVMTargetData {
    data: LLVMTargetDataRef,
    machine: LLVMTargetMachineRef,
    is_clone: bool,
    pub is_debug: bool,
    pub is_opaque_pointers: bool,
}

impl Clone for LLVMTargetData {
    fn clone(&self) -> Self {
        Self {
            data: self.data,
            machine: self.machine,
            is_clone: true,
            is_debug: self.is_debug,
            is_opaque_pointers: self.is_opaque_pointers,
        }
    }
}

impl LLVMTargetData {
    pub fn new(target_triple: &str, debug: bool, opaque: bool) -> Result<LLVMTargetData> {
        if target_triple == "__llvm_ir" {
            return Ok(LLVMTargetData {
                data: std::ptr::null_mut(),
                machine: std::ptr::null_mut(),
                is_clone: false,
                is_debug: debug,
                is_opaque_pointers: opaque,
            });
        }
        unsafe {
            let cpu = CString::new("generic").expect("invalid cpu");
            let features = CString::new("").expect("invalid feature flags");

            let target_triple = CString::new(target_triple).expect("invalid cstr");
            let mut target: LLVMTargetRef = std::ptr::null_mut();
            let mut error_message: [u8; 255] = [0; 255];
            if LLVMGetTargetFromTriple(target_triple.as_ptr() as *const _, &mut target, error_message.as_mut_ptr() as *mut _) != 0 {
                return Err(anyhow!("error from LLVM: {}", std::str::from_utf8_unchecked(&error_message)));
            }

            let target_machine = LLVMCreateTargetMachine(
                target,
                target_triple.as_ptr() as *const _,
                cpu.as_ptr() as *const _,
                features.as_ptr() as *const _,
                LLVMCodeGenOptLevel::LLVMCodeGenLevelAggressive,
                LLVMRelocMode::LLVMRelocDefault,
                LLVMCodeModel::LLVMCodeModelDefault,
            );
            LLVMSetTargetMachineAsmVerbosity(target_machine, i32::from(debug));

            let target_layout = LLVMCreateTargetDataLayout(target_machine);
            Ok(LLVMTargetData {
                data: target_layout,
                machine: target_machine,
                is_clone: false,
                is_debug: debug,
                is_opaque_pointers: opaque,
            })
        }
    }

    pub fn is_llvm_ir(&self) -> bool {
        self.data.is_null()
    }

    pub fn get_pointer_size(&self) -> u32 {
        if self.is_llvm_ir() {
            return 8; // 64-bit by default for LLVM IR
        }
        unsafe { LLVMPointerSize(self.data) }
    }

    pub fn get_type_size(&self, ty: OpaqueType) -> u64 {
        if self.is_llvm_ir() {
            let target = LLVMTargetData::new("x86_64-unknown-linux-gnu", false, true).unwrap();
            target.get_type_size(ty)
        } else {
            unsafe { LLVMSizeOfTypeInBits(self.data, ty.0) / 8 }
        }
    }
}

impl Drop for LLVMTargetData {
    fn drop(&mut self) {
        if !self.is_llvm_ir() && !self.is_clone {
            unsafe {
                LLVMDisposeTargetData(self.data);
                LLVMDisposeTargetMachine(self.machine);
            }
        }
    }
}

pub struct PassManager {
    pm: LLVMPassManagerRef,
}

impl PassManager {
    pub fn new(_opt_level: u32) -> PassManager {
        // unsafe {
        let pm = std::ptr::null_mut();
        // let pm = LLVMCreatePassManager();

        // let pmb = LLVMPassManagerBuilderCreate();
        // LLVMPassManagerBuilderSetOptLevel(pmb, opt_level);
        // LLVMPassManagerBuilderSetSizeLevel(pmb, 0);
        // LLVMPassManagerBuilderSetDisableUnitAtATime(pmb, i32::from(false));
        // LLVMPassManagerBuilderSetDisableUnrollLoops(pmb, i32::from(false));

        // LLVMPassManagerBuilderPopulateModulePassManager(pmb, pm);
        // LLVMPassManagerBuilderDispose(pmb);

        PassManager {
            pm,
        }
        // }
    }

    pub fn run(&self, _module: LLVMModuleRef) {
        // unsafe {
        // LLVMRunPassManager(self.pm, module);
        // }
    }
}

// impl Drop for PassManager {
//     fn drop(&mut self) {
//         unsafe {
//             LLVMDisposePassManager(self.pm);
//         }
//     }
// }

pub struct ExecutionEngine {
    engine: LLVMExecutionEngineRef,
}

extern "C" {
    fn malloc(bytes: usize) -> *mut c_void;
    fn free(ptr: *mut c_void);
}

impl ExecutionEngine {
    pub fn new(main_module: LLVMModuleRef) -> ExecutionEngine {
        unsafe {
            LLVMLinkInMCJIT();

            let mut jit_opts = LLVMMCJITCompilerOptions {
                OptLevel: 0,
                CodeModel: LLVMCodeModel::LLVMCodeModelDefault,
                NoFramePointerElim: 0,
                EnableFastISel: 0,
                MCJMM: std::ptr::null_mut(),
            };
            LLVMInitializeMCJITCompilerOptions(&mut jit_opts, std::mem::size_of_val(&jit_opts));

            let mut engine = std::ptr::null_mut();
            let mut err = std::ptr::null_mut();
            if LLVMCreateMCJITCompilerForModule(&mut engine, main_module, &mut jit_opts, std::mem::size_of_val(&jit_opts), &mut err) != 0 {
                let cstr = CStr::from_ptr(err);
                eprintln!("LLVMCreateJITCompilerForModule: {}", cstr.to_str().unwrap());
                std::process::exit(1);
            }

            ExecutionEngine {
                engine,
            }
        }
    }

    pub fn get_error(&self) -> Option<String> {
        unsafe {
            let mut err = std::ptr::null_mut();
            if LLVMExecutionEngineGetErrMsg(self.engine, &mut err) == 0 {
                Some(CStr::from_ptr(err).to_str().unwrap().to_owned())
            } else {
                None
            }
        }
    }

    pub fn call_function(&self, name: &str) {
        unsafe {
            let cstr = CString::new(name).unwrap();
            let mut func = std::ptr::null_mut();
            if LLVMFindFunction(self.engine, cstr.as_ptr(), &mut func) != 0 {
                eprintln!("LLVMFindFunction: no such function {}", name);
                std::process::exit(1);
            }

            let args = &mut [];
            LLVMRunFunction(self.engine, func, 0, args.as_mut_ptr());
        }
    }
}

impl Drop for ExecutionEngine {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeExecutionEngine(self.engine);
        }
    }
}
