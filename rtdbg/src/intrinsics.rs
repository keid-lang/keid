use lazy_static::lazy_static;
use std::fmt::Write;
use std::{ffi::*, sync::Mutex};

lazy_static! {
    static ref STACK_FRAMES: Mutex<Vec<String>> = Mutex::new(Vec::new());
}

#[no_mangle]
extern "C" fn keid_core_runtime_push_stack_frame(frame: *const c_char) {
    let frame = unsafe { CStr::from_ptr(frame) };
    let frame = frame.to_str().unwrap().to_owned();
    let mut frames = STACK_FRAMES.lock().unwrap();
    frames.push(frame);
}

#[no_mangle]
extern "C" fn keid_core_runtime_pop_stack_frame() {
    let mut frames = STACK_FRAMES.lock().unwrap();
    frames.pop().expect("keid_core_runtime_pop_stack_frame: stack underflow");
}

#[no_mangle]
extern "C" fn keid_core_runtime_get_stack_frames() -> *mut c_char {
    let mut stacktrace = String::new();
    let frames = STACK_FRAMES.lock().unwrap();
    for frame in frames.iter().rev() {
        write!(&mut stacktrace, "  at {}\n", frame).unwrap();
    }
    let cstring = CString::new(stacktrace).unwrap();
    cstring.into_raw()
}

#[no_mangle]
extern "C" fn keid_core_runtime_free_stack_frames(frames: *mut c_char) {
    unsafe {
        std::mem::drop(CString::from_raw(frames));
    }
}

#[no_mangle]
extern "C" fn keid_core_runtime_print_stack_frames() {
    let frames = STACK_FRAMES.lock().unwrap();
    for frame in frames.iter().rev() {
        println!("  at {}", frame);
    }
}
