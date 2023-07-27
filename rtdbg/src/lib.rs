use std::{
    collections::HashMap,
    ffi::{c_char, c_void, CStr},
};

pub mod intrinsics;

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct KeidAbiClassData {
    class_info: *const KeidAbiClassInfo,
    ref_count: i64,
}

const TYPE_STRUCT: u32 = 0x01;

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct KeidAbiClassInfo {
    destructor: *const c_void,
    virtual_methods: *const *const c_void,
    interface_impls_count: u32,
    interface_impls: *const KeidAbiInterfaceImpl,
    class_name: *const c_char,
    class_bitflags: u32,
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct KeidAbiInterfaceImpl {
    interface_id: u32,
    virtual_methods: *const *const c_void,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy)]
pub enum ScopeType {
    Scope = 0,
    Unscope = 1,
}

#[derive(Debug, Clone, Copy)]
pub struct DebugObject {
    pub object_id: u64,
    pub ref_count: i64,
}

#[derive(Debug, Clone)]
pub struct DeadObject {
    pub object_id: u64,
    pub class_name: String,
}

#[derive(Debug)]
pub struct RuntimeDebugState {
    pub object_nonce: u64,
    pub objects: HashMap<*mut KeidAbiClassData, DebugObject>,
    pub dead_objects: HashMap<*mut KeidAbiClassData, DeadObject>,
}

#[no_mangle]
pub extern "C" fn rtdbg_range(start: usize, end: usize, current: usize) {
    println!("rtdbg_range(start: {}, end: {}, current: {})", start, end, current);
}

#[no_mangle]
pub extern "C" fn rtdbg_fail() -> ! {
    println!("  [librtdbg] Exiting due to fatal runtime error");
    unsafe {
        keid_core_runtime_print_stack_frames();
    }
    std::process::exit(1)
}

#[no_mangle]
pub extern "C" fn rtdbg_initialize() -> *mut RuntimeDebugState {
    Box::into_raw(Box::new(RuntimeDebugState {
        object_nonce: 1,
        objects: HashMap::new(),
        dead_objects: HashMap::new(),
    }))
}

#[no_mangle]
pub extern "C" fn rtdbg_finish(rtdbg: *mut RuntimeDebugState) {
    println!("\n  [librtdbg] Keid program execution finished.");
    let rtdbg = unsafe { Box::from_raw(rtdbg) };
    for (key, object) in &rtdbg.objects {
        let (_, class_name) = read_object_instance(&rtdbg, *key);
        println!(
            "  [librtdbg] Memory leak detected: object #{} (type {}) (at {:p}) with reference count of {}",
            object.object_id, class_name, key, object.ref_count
        );
    }
}

fn read_object_instance(rtdbg: &RuntimeDebugState, data: *mut KeidAbiClassData) -> (bool, String) {
    if data.is_null() {
        println!("  [librtdbg] Fatal error: object instance at {:p} cannot be null", data);
        rtdbg_fail();
    }
    if let Some(dead_object) = rtdbg.dead_objects.get(&(data as *mut _)) {
        println!(
            "  [librtdbg] Fatal error: attempted reference of deallocated object #{} (at {:p}) (type {})",
            dead_object.object_id, data, dead_object.class_name
        );
        rtdbg_fail();
    }
    let (is_struct, class_name) = unsafe {
        let class_info = (*data).class_info;
        if class_info.is_null() {
            println!("  [librtdbg] Fatal error: object instance at {:p} class info cannot be null", data);
            rtdbg_fail();
        }
        let class_info = *class_info;
        ((class_info.class_bitflags & TYPE_STRUCT) == TYPE_STRUCT, class_info.class_name)
    };
    let class_name = unsafe { CStr::from_ptr(class_name) };
    let class_name = class_name.to_str().unwrap().to_owned();
    (is_struct, class_name)
}

#[no_mangle]
pub extern "C" fn rtdbg_register_object(rtdbg: *mut RuntimeDebugState, data: *mut KeidAbiClassData) {
    if rtdbg.is_null() {
        println!("  [librtdbg] Fatal error: rtdbg instance cannot be null");
        rtdbg_fail();
    }
    let mut rtdbg = unsafe { Box::from_raw(rtdbg) };

    if rtdbg.dead_objects.contains_key(&(data as *mut _)) {
        rtdbg.dead_objects.remove(&(data as *mut _));
    }

    let (is_struct, class_name) = read_object_instance(&rtdbg, data);
    if is_struct {
        println!("  [librtdbg] Ignoring registry of stack-allocated struct (at {:p}) with type {}", data, class_name);
        return;
    }
    println!("  [librtdbg] Registered object #{} (at {:p}) with type {}", rtdbg.object_nonce, data, class_name);
    rtdbg.objects.insert(
        data,
        DebugObject {
            object_id: rtdbg.object_nonce,
            ref_count: 0,
        },
    );
    rtdbg.object_nonce += 1;
    Box::into_raw(rtdbg);
}

#[no_mangle]
pub extern "C" fn rtdbg_change_scope(
    rtdbg: *mut RuntimeDebugState,
    data: *mut KeidAbiClassData,
    scope_type: ScopeType,
) {
    if rtdbg.is_null() {
        println!("  [librtdbg] Fatal error: rtdbg instance cannot be null");
        rtdbg_fail();
    }
    let mut rtdbg = unsafe { Box::from_raw(rtdbg) };
    let (is_struct, class_name) = read_object_instance(&rtdbg, data);
    if is_struct {
        println!(
            "  [librtdbg] Ignoring scope change of stack-allocated struct (at {:p}) with type {}",
            data, class_name
        );
        return;
    }
    let debug_obj = match rtdbg.objects.get(&(data as *mut _)) {
        Some(debug_obj) => debug_obj,
        None => {
            println!(
                "  [librtdbg] Fatal runtime error: attempted changing scope of unregistered object of type {} at {:p}",
                class_name, data as *mut _
            );
            rtdbg_fail();
        }
    };
    let real_ref_count = unsafe { (*data).ref_count };
    if real_ref_count != debug_obj.ref_count {
        println!("  [librtdbg] Fatal runtime error: librtdbg tracked reference count = {}, but the real memory value = {} for object #{} (type {})", debug_obj.ref_count, real_ref_count, debug_obj.object_id, class_name);
        rtdbg_fail();
    }
    let new_ref_count = match scope_type {
        ScopeType::Scope => debug_obj.ref_count + 1,
        ScopeType::Unscope => debug_obj.ref_count - 1,
    };
    // println!(
    //     "  [librtdbg] Object #{} (type {}) ref count {} from {} to {}",
    //     debug_obj.object_id,
    //     class_name,
    //     match scope_type {
    //         ScopeType::Scope => "scoping",
    //         ScopeType::Unscope => "unscoping",
    //     },
    //     debug_obj.ref_count,
    //     new_ref_count,
    // );

    if new_ref_count == 0 {
        println!(
            "  [librtdbg] Deregistering object #{} (type {}) as its ref count is now 0",
            debug_obj.object_id, class_name
        );
        rtdbg.dead_objects.insert(
            data as *mut _,
            DeadObject {
                object_id: debug_obj.object_id,
                class_name,
            },
        );
        rtdbg.objects.remove(&(data as *mut _));
    } else {
        rtdbg.objects.get_mut(&(data as *mut _)).unwrap().ref_count = new_ref_count;
    }
    Box::into_raw(rtdbg);
}

extern "C" {
    fn keid_core_runtime_print_stack_frames();
}
