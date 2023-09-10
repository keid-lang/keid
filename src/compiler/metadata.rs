use std::fmt::Write;

use super::llvm::{Context, Linkage, Module, OpaqueValue};
use crate::{
    common::{GenericIdentifier, TypeProvider},
    func::utils,
    tree::{ClassType, GenericNode, ResolvedClassNode, ResolvedEnumNode},
};

pub struct VirtualMethodInfo {
    pub method_id: usize,
    pub func_ptr: OpaqueValue,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassInfoData {
    pub module_id: usize,
    pub source_id: usize,
    pub ident: GenericIdentifier,
    pub class_type: ClassType,
}

impl ClassInfoData {
    pub fn from_resolved_class(type_provider: &TypeProvider, class: &ResolvedClassNode) -> ClassInfoData {
        let base_name = type_provider.get_source_class(class).base_name.clone();
        ClassInfoData {
            module_id: class.module_id,
            source_id: class.source_id,
            ident: GenericIdentifier::from_name_with_args(&base_name, &class.generic_impls),
            class_type: class.class_type,
        }
    }

    pub fn from_resolved_enum(type_provider: &TypeProvider, class: &ResolvedEnumNode) -> ClassInfoData {
        let base_name = type_provider.get_source_enum(class).base_name.clone();
        ClassInfoData {
            module_id: class.module_id,
            source_id: class.source_id,
            ident: GenericIdentifier::from_name_with_args(&base_name, &class.generic_impls),
            class_type: ClassType::Enum,
        }
    }
}

pub struct ClassInfo {
    pub data: ClassInfoData,
    pub destructor_ptr: OpaqueValue,
    pub virtual_methods: Vec<VirtualMethodInfo>,
}

pub struct ClassInfoStorage {
    pub classes: Vec<ClassInfo>,
    pub module: Module,
}

const TYPE_STRUCT: i32 = 0x01;
const TYPE_ENUM: i32 = 0x02;

impl ClassInfoStorage {
    pub fn new(context: &mut Context) -> ClassInfoStorage {
        let module = context.create_module("keid/keid.metadata", "keid.metadata");

        ClassInfoStorage {
            classes: Vec::new(),
            module,
        }
    }

    pub fn create_class_info_storage(&mut self, context: &mut Context, type_provider: &TypeProvider) {
        if self.classes.is_empty() {
            return;
        }

        let info_array_type = context.get_abi_class_info_type();
        let vtable_item_type = context.get_pointer_type(context.get_void_type());

        let mut class_info_structs = Vec::with_capacity(self.classes.len());
        let mut interface_impl_structs = Vec::new();
        let mut vtable_pointers = Vec::new();

        // temporary vtable, replaced with global at the end of this function
        let vtable = context.const_array(vtable_item_type, &[]);

        let mut vtable_offset = 0;
        for i in 0..self.classes.len() {
            let class_info = &self.classes[i];
            let resolved_interface_impls = type_provider.get_resolved_interface_impls(&class_info.data.ident);

            for resolved_interface_impl in resolved_interface_impls {
                let interface_impl = type_provider.get_source_interface_impl(&resolved_interface_impl);

                let mut interface_name_str = interface_impl.interface_name.clone();
                if !resolved_interface_impl.interface_generic_impls.is_empty() {
                    write!(&mut interface_name_str, "<{}>", utils::iter_join(&resolved_interface_impl.interface_generic_impls)).unwrap();
                }

                let interface_name_array = context.const_string(&interface_name_str);
                let interface_name_var = format!("keid.interface_name.{}", interface_name_str);
                let interface_name_global = self.module.get_global(&interface_name_var).unwrap_or_else(|| {
                    self.module.create_global(
                        context,
                        &interface_name_var,
                        context.get_array_type(context.get_i8_type(), interface_name_str.len() + 1),
                    )
                });
                self.module.initialize_global(interface_name_global, interface_name_array);

                interface_impl_structs.push(context.const_struct(
                    context.get_abi_interface_impl_type(),
                    &mut [
                        context.const_int(
                            context.get_i32_type(),
                            type_provider.get_resolved_interface_id(&GenericIdentifier::from_name_with_args(
                                &interface_impl.interface_name,
                                &resolved_interface_impl.interface_generic_impls,
                            )) as _,
                        ), // interface ID
                        interface_name_global, // interface name
                        context.const_get_element_ptr_dynamic(vtable_item_type, vtable, vtable_offset), // points to the start of this impl's functions in the vtable
                    ],
                ));
                vtable_offset += interface_impl.functions.len();
            }
        }

        let global_interface_impl = self.module.create_global(
            context,
            "keid.interface_impls",
            context.get_array_type(context.get_abi_interface_impl_type(), interface_impl_structs.len()),
        );
        let global_interface_impl_array = context.const_array(context.get_abi_interface_impl_type(), &interface_impl_structs);
        self.module.initialize_global(global_interface_impl, global_interface_impl_array);

        let mut interface_impl_offset = 0;
        for i in 0..self.classes.len() {
            let class_info = &self.classes[i];

            let resolved_interface_impls = type_provider.get_resolved_interface_impls(&class_info.data.ident);
            for resolved_interface_impl in &resolved_interface_impls {
                let interface_impl = type_provider.get_source_interface_impl(resolved_interface_impl);

                // the impls get added here, thus the vtable pointer points to their start
                let source_interface = type_provider.get_impl_source_interface(interface_impl);

                let mut functions: Vec<usize> = Vec::new();
                functions.extend(&source_interface.functions);
                functions.extend(source_interface.accessors.iter().map(|acc| acc.function_id));
                functions.dedup();

                for source_function_id in functions {
                    let source_function_name =
                        type_provider.get_function_node(source_interface.module_id, source_function_id).unwrap().base_name.clone();
                    for impl_function_id in &interface_impl.functions {
                        let node = type_provider.get_function_node(resolved_interface_impl.module_id, *impl_function_id).unwrap();

                        // only add the function if it has the correct name from the source interface
                        // this maintains the order of the functions regardless of implementation
                        if node.base_name != source_function_name
                            && !node.base_name.starts_with(&format!("{}#__impl#", source_function_name))
                        {
                            continue;
                        }

                        let mut name = node.base_name.clone();
                        if !resolved_interface_impl.generic_impls.is_empty() {
                            write!(&mut name, "<{}>", utils::iter_join(&resolved_interface_impl.generic_impls)).unwrap();
                        }

                        let node_impl = node.create_impl(type_provider, &resolved_interface_impl.generic_impls).unwrap();
                        let externed_function = self
                            .module
                            .add_function(
                                &node_impl.external_name,
                                context.get_function_type(&[], node.varargs, context.get_void_type()),
                                0,
                            )
                            .as_val()
                            .to_value();
                        context.set_linkage(externed_function, Linkage::LLVMExternalLinkage);
                        vtable_pointers.push(externed_function);
                    }
                }
            }

            // if no interfaces are implemented, the interfaces pointer is null
            let interfaces_ptr = if resolved_interface_impls.is_empty() {
                context.const_null_ptr(context.get_abi_interface_impl_type())
            } else {
                context.const_get_element_ptr_dynamic(context.get_abi_interface_impl_type(), global_interface_impl, interface_impl_offset)
            };
            interface_impl_offset += resolved_interface_impls.len();

            // if there are no virtual methods, the vtable pointer is null
            // let vtable_pointer = if vtable_pointers.len() == vtable_len {
            //     context.const_null_ptr(vtable_item_type)
            // } else {
            //     context.const_get_element_ptr_dynamic(vtable_item_type, vtable, vtable_len)
            // };
            let vtable_pointer = context.const_null_ptr(vtable_item_type); // there are no class-level virtual methods currently

            let class_name_str = class_info.data.ident.to_string();
            let class_name_array = context.const_string(&class_name_str);
            let class_name_global = self.module.create_global(
                context,
                &format!("keid.class_name.{}", class_name_str),
                context.get_array_type(context.get_i8_type(), class_name_str.len() + 1),
            );
            self.module.initialize_global(class_name_global, class_name_array);

            let mut class_bitflags = 0;
            if class_info.data.class_type == ClassType::Struct {
                class_bitflags |= TYPE_STRUCT;
            } else if class_info.data.class_type == ClassType::Enum {
                class_bitflags |= TYPE_ENUM;
            }
            let class_bitflags = context.const_int(context.get_i32_type(), class_bitflags as _);

            class_info_structs.push(context.const_struct(
                info_array_type,
                &mut [
                    class_info.destructor_ptr,                                                        // destructor
                    vtable_pointer,                                                                   // vtable
                    context.const_int(context.get_i32_type(), resolved_interface_impls.len() as u64), // interfaces length
                    interfaces_ptr,                                                                   // interfaces
                    class_name_global,                                                                // class name
                    class_bitflags,
                ],
            ));
        }

        let array_type = context.get_array_type(context.get_abi_class_info_type(), class_info_structs.len());
        let global_class_info = self.module.create_global(context, "keid.classinfo", array_type);
        let global_class_info_array = context.const_array(info_array_type, &class_info_structs);
        self.module.initialize_global(global_class_info, global_class_info_array);

        let vtable_value = context.const_array(vtable_item_type, &vtable_pointers);
        let replacement_vtable =
            self.module.create_global(context, "keid.vtable", context.get_array_type(vtable_item_type, vtable_pointers.len()));
        context.replace_all_uses(vtable, replacement_vtable);
        self.module.initialize_global(replacement_vtable, vtable_value);
    }

    pub fn get_abi_class_info_offset(&mut self, context: &Context, data: &ClassInfoData) -> usize {
        self.classes.iter().position(|cls| &cls.data == data).unwrap_or_else(|| {
            let new_index = self.classes.len();
            self.classes.push(ClassInfo {
                data: data.clone(),
                destructor_ptr: context.const_null_ptr(context.get_void_type()),
                virtual_methods: Vec::new(),
            });

            new_index
        })
    }

    pub fn get_abi_class_info_ptr(&mut self, context: &Context, module: &Module, data: &ClassInfoData) -> OpaqueValue {
        let classinfo_index = self.get_abi_class_info_offset(context, data);
        let info_array_type = context.get_abi_class_info_type();
        context.const_get_element_ptr_dynamic(
            info_array_type,
            module.get_global("keid.classinfo").expect("missing keid.classinfo"),
            classinfo_index,
        )
    }

    pub fn set_destructor(&mut self, module_id: usize, class_id: usize, destructor_ptr: OpaqueValue) {
        if let Some(class) = self.classes.iter_mut().find(|class| class.data.module_id == module_id && class.data.source_id == class_id) {
            class.destructor_ptr = destructor_ptr;
        } else {
            panic!("no class found")
        }
    }
}
