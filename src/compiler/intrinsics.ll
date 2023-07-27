source_filename = "intrinsics.ll"

; heap data for a class: ptr (classinfo pointer), i64 (ref count)
%KeidAbiClassData = type { ptr, i64 }

; pointer to either a stack-allocated struct or a heap-allocated class: ptr (classinfo pointer)
%KeidAbiStructData = type { ptr }

; constant class metadata:
; 0  ptr (pointer to class destructor),
; 1  ptr (pointer to global virtual method array),
; 2  i32 (length of interface impls)
; 3  ptr to [%KeidAbiInterfaceImpl] (n-length interface impls)
; 4  ptr (class name string)
; 5  i32 (class flags)
;      TYPE_CLASS  = 0x00
;      TYPE_STRUCT = 0x01
; 6  ptr to %KeidAbiClassInfo (superclass pointer, or null if this represents the core::object::Object type)
%KeidAbiClassInfo = type { ptr, ptr, i32, ptr, ptr, i32, ptr }

; interface implementation metadata:
;  i32 (interface unique ID)
;  ptr (interface name string)
;  ptr (pointer to global virtual method array containing interface implementations)
%KeidAbiInterfaceImpl = type { i32, ptr, ptr }

%"core::mem::Pointer<char>" = type { ptr, i64 }
%"[char]#Slice" = type { i64, i64, ptr }
%"[core::string::String]#Heap" = type { i64, ptr }

@null_value_error = private unnamed_addr constant [62 x i8] c"NullValueError: attempted non-null assertion on a null value\0A\00", align 1
@missing_class_info_error = private unnamed_addr constant [91 x i8] c"MissingClassInfoError: attempted retrieval of null class info pointer in object or struct\0A\00", align 1
@find_interface_method_null_error = private unnamed_addr constant [85 x i8] c"NullValueError: attempted retrieval of interface method pointer from a null pointer\0A\00", align 1
@illegal_ref_count_error = private unnamed_addr constant [43 x i8] c"InternalError: illegal reference count %i\0A\00", align 1
@no_such_method_error = private unnamed_addr constant [104 x i8] c"NoSuchMethodError: attempted to invoke an invalid interface method (interface id = %i, method id = %i)\0A\00", align 1
@index_out_of_bounds_error = private unnamed_addr constant [84 x i8] c"IndexOutOfBoundsError: attempted to access index %i of a collection with length %i\0A\00", align 1
@error_already_thrown_error = private unnamed_addr constant [100 x i8] c "ErrorAlreadyThrownError: attempted to throw error when current thread already is handling an error\0A\00", align 1
@allocator = global ptr null, align 8
@deallocator = global ptr null, align 8
@current_error = thread_local global ptr null, align 8
@rtdbg_instance = global ptr null, align 8

@"core::runtime::hasInit" = external global i1
@keid.classinfo = external global [0 x %KeidAbiClassInfo]

; This is an implementation of strlen() in LLVM IR that works in no-std environments.
define i64 @keid_cstrlen(ptr %str) {
block.start:
  %ptr_bits_ptr = alloca i64, align 8
  %start_ptr_bits = ptrtoint ptr %str to i64
  store i64 %start_ptr_bits, ptr %ptr_bits_ptr
  br label %block.loop_head
block.loop_head:
  %ptr_bits = load i64, ptr %ptr_bits_ptr
  %current_ptr = inttoptr i64 %ptr_bits to ptr
  %current_char = load i8, ptr %current_ptr
  %is_null_char = icmp eq i8 %current_char, 0
  br i1 %is_null_char, label %block.return, label %block.next
block.next:
  %next_ptr_bits = add i64 %ptr_bits, 1 ; add one byte
  store i64 %next_ptr_bits, ptr %ptr_bits_ptr
  br label %block.loop_head
block.return:
  %end_ptr_bits = load i64, ptr %ptr_bits_ptr
  %len = sub i64 %end_ptr_bits, %start_ptr_bits ; subtract the length difference of the pointers
  ret i64 %len
}

; Creates a new `core::string::String` object given a pointer to the string's UTF-8 bytes and total byte length.
define ptr @keid_new_string(ptr %bytes, i64 %length) {
block.main:
  %ptr_bits = ptrtoint ptr %bytes to i64
  %"core::mem::Pointer<char> ptr" = alloca %"core::mem::Pointer<char>", align 8
  call void @"core::mem::Pointer::to<char>(usize)"(i64 %ptr_bits, ptr %"core::mem::Pointer<char> ptr")
  %"[char] slice" = alloca %"[char]#Slice", align 8
  call void @"core::array::copyFromPtr<char>(core::mem::Pointer<char>, usize)"(ptr byval(%"core::mem::Pointer<char>") %"core::mem::Pointer<char> ptr", i64 %length, ptr %"[char] slice")
  %"core::string::String str" = call ptr @"core::string::String::fromUtf8Slice([char])"(ptr byval(%"[char]#Slice") %"[char] slice")

  ret ptr %"core::string::String str"
}

; Utility function for throwing out-of-bounds errors.
define void @keid.throw_out_of_bounds(i64 %index, i64 %length) {
  call i32 @printf(ptr @index_out_of_bounds_error, i64 %index, i64 %length)
  call void @"core::runtime::printStackFrames()"()
  call void @exit(i32 1)
  unreachable
}

define ptr @keid.find_static_interface_method(ptr %class_info, i32 %interface_id, i32 %method_id) {
block.check_null:
  %is_null = icmp eq ptr %class_info, null
  br i1 %is_null, label %block.break_null, label %block.main
block.break_null:
  call i32 @printf(ptr @find_interface_method_null_error)
  call void @"core::runtime::printStackFrames()"()
  call void @exit(i32 1)
  unreachable
block.main:
  %interface_len_ptr = getelementptr inbounds %KeidAbiClassInfo, ptr %class_info, i32 0, i32 2
  %interface_len = load i32, ptr %interface_len_ptr, align 4 ; load the amount of interfaces to iterate through

  %i = alloca i32 ; loop iterator value, represents the current array index
  store i32 0, ptr %i
  br label %block.loop_top

block.loop_top: ; loop test (tests if i < interface_len)
  %i_value = load i32, ptr %i, align 4
  %has_finished_iterating = icmp uge i32 %i_value, %interface_len ; i >= interface_len is the inverse of i < interface_len
  br i1 %has_finished_iterating, label %block.fail, label %block.loop_body ; jump if the above inverse is true

block.loop_body: ; loop body
  %interface_list_ptr = getelementptr inbounds %KeidAbiClassInfo, ptr %class_info, i32 0, i32 3 ; get a pointer to interfaces array pointer
  %interface_list = load ptr, ptr %interface_list_ptr, align 4 ; load the pointer to the interface array pointer
  %current_interface = getelementptr inbounds [0 x %KeidAbiInterfaceImpl], ptr %interface_list, i32 0, i32 %i_value ; get a pointer to the specific array element
  %info_interface_id_ptr = getelementptr inbounds %KeidAbiInterfaceImpl, ptr %current_interface, i32 0, i32 0 ; get a pointer to the interface ID
  %info_interface_id = load i32, ptr %info_interface_id_ptr, align 4 ; load the interface ID pointer

  %is_correct = icmp eq i32 %info_interface_id, %interface_id ; compare the interface ID parameter and the one in the array
  br i1 %is_correct, label %block.find_method, label %block.loop_footer

block.loop_footer: ; end of loop, increments the counter and then goes to the top
  %next_i = add i32 %i_value, 1
  store i32 %next_i, ptr %i
  br label %block.loop_top

block.find_method:
  %method_array_ptr_ptr = getelementptr inbounds %KeidAbiInterfaceImpl, ptr %current_interface, i32 0, i32 2 ; get a pointer to the method array pointer
  %method_array_ptr = load ptr, ptr %method_array_ptr_ptr, align 4 ; load the pointer to the method array pointer
  %method_array_item_ptr = getelementptr inbounds [0 x ptr], ptr %method_array_ptr, i32 0, i32 %method_id ; get a pointer to the method pointer
  %method_array_item = load ptr, ptr %method_array_item_ptr, align 4 ; load the pointer to the method pointer
  %method_array_item_is_null = icmp eq ptr %method_array_item, null
  br i1 %method_array_item_is_null, label %block.fail, label %block.success
  
block.success:
  ret ptr %method_array_item ; return the method pointer

block.fail: ; the method pointer was not found -- fatal unrecoverable error
  call i32 @printf(ptr @no_such_method_error, i32 %interface_id, i32 %method_id)
  call void @"core::runtime::printStackFrames()"()
  call void @exit(i32 1)
  unreachable
}

; Returns the pointer to the implementation of an interface method, or a null pointer if one cannot be found.
;  %object - the object or struct instance that the interface method is being invoked on
;  %interface_id - the unique ID of the interface that the method is defined in
;  %method_id - the unique ID of the method within the interface declaration
define ptr @keid.find_virtual_interface_method(ptr %object, i32 %interface_id, i32 %method_id) {
block.check_null:
  %is_null = icmp eq ptr %object, null
  br i1 %is_null, label %block.break_null, label %block.main
block.break_null:
  call i32 @printf(ptr @find_interface_method_null_error)
  call void @"core::runtime::printStackFrames()"()
  call void @exit(i32 1)
  unreachable
block.main:
  %class_info_ptr = getelementptr inbounds %KeidAbiStructData, ptr %object, i32 0, i32 0
  %class_info = load ptr, ptr %class_info_ptr, align 4 ; load the class info pointer from the class data
  %res = call ptr @keid.find_static_interface_method(ptr %class_info, i32 %interface_id, i32 %method_id)
  ret ptr %res 
}

; Asserts that the parameter is not zero.
; If the parameter is zero, then an error is thrown and the program exits early.
; In the future, once error handling is fully implemented,
; this should throw a handleable error instead of exiting the program early.
;  %nullability - the parameter to assert as non-zero, typically the nullability parameter of a nullable type
define void @keid.assert_non_null(i8 %nullability) {
block.check_null:
  %is_null = icmp eq i8 %nullability, 0
  br i1 %is_null, label %block.is_null, label %block.is_not_null

block.is_null:
  call i32 @printf(ptr @null_value_error)
  call void @"core::runtime::printStackFrames()"()
  call void @exit(i32 1)
  unreachable

block.is_not_null:
  ret void
}

define i1 @keid.is_struct(ptr %object) {
block.check:
  %class_info_ptr_ptr = getelementptr inbounds %KeidAbiClassData, ptr %object, i32 0, i32 0
  %class_info_ptr = load ptr, ptr %class_info_ptr_ptr, align 8
  ; null check, i.e. `if %class_info_ptr == NULL`
  %class_info_is_null = icmp eq ptr %class_info_ptr, null
  br i1 %class_info_is_null, label %block.class_info_is_null, label %block.class_info_is_not_null

block.class_info_is_not_null:
  %class_bitflags_ptr = getelementptr inbounds %KeidAbiClassInfo, ptr %class_info_ptr, i32 0, i32 5
  %class_bitflags = load i32, ptr %class_bitflags_ptr, align 4
  %bitflags_and_type_struct = and i32 %class_bitflags, 1 ; (flags & 0x01) == 0x01, where 0x01 is TYPE_STRUCT
  %is_struct = icmp eq i32 %bitflags_and_type_struct, 1
  ret i1 %is_struct

block.class_info_is_null:
  ; call i32 @printf(ptr @missing_class_info_error)
  ; call void @"core::runtime::printStackFrames()"()
  ; call void @exit(i32 1)
  ; unreachable
  ret i1 1
}

; Invoked once immediately after the instantiation of every object type (including structs).
;   %object - pointer to the reference counted object or stack-allocated struct
define void @keid.register_object(ptr %object) {
block.main:
$IF(RTDBG, ```
  %rtdbg = load ptr, ptr @rtdbg_instance, align 4
  call void @rtdbg_register_object(ptr %rtdbg, ptr %object)
```)
  ret void
}

; Increases the reference count of an object by 1.
;   %object - pointer to the reference counted object
define void @keid.scope(ptr %object) {
block.check_null:
;   ; null check, i.e. `if %object == NULL`
;   %object_is_null = icmp eq ptr %object, null
;   br i1 %object_is_null, label %block.is_null, label %block.is_not_null

; block.is_null:
;   ret void ; exit early if %object is a null pointer

; block.is_not_null:
;   %is_struct = call i1 @keid.is_struct(ptr %object)
;   br i1 %is_struct, label %block.is_struct, label %block.is_not_struct

; block.is_struct:
;   ret void ; exit early if %object is a struct type (i.e. has no ref count)

; block.is_not_struct:
; $IF(RTDBG, ```
;   %rtdbg = load ptr, ptr @rtdbg_instance, align 4
;   call void @rtdbg_change_scope(ptr %rtdbg, ptr %object, i8 0)
; ```)
;   %ref_count_ptr = getelementptr inbounds %KeidAbiClassData, ptr %object, i32 0, i32 1
;   %ref_count = load i64, ptr %ref_count_ptr, align 4 ; load the ref count from %ref_count_ptr
;   %new_count = add i64 %ref_count, 1                 ; add one to the ref count
;   store i64 %new_count, ptr %ref_count_ptr, align 4  ; store the (ref count + 1) into %ref_count_ptr
  ret void
}

; Decreases the reference count of an object by 1.
; If the new reference count is 0, the object's memory will be freed.
;   %object - pointer to the reference counted object
define void @keid.unscope(ptr %object) {
block.check_null:
;   ; null check, i.e. `if %object == NULL`
;   %object_is_null = icmp eq ptr %object, null
;   br i1 %object_is_null, label %block.is_null, label %block.is_not_invalid_null

; block.is_null:                          
;   ret void ; exit early if %object is a null pointer

; block.is_struct:
;   ret void ; exit early if %object is a struct type (i.e. has no ref count)

; block.is_not_invalid_null:
;   %is_struct = call i1 @keid.is_struct(ptr %object)
;   br i1 %is_struct, label %block.is_struct, label %block.is_not_struct

; block.is_not_struct:
; $IF(RTDBG, ```
;   %rtdbg = load ptr, ptr @rtdbg_instance, align 4
;   call void @rtdbg_change_scope(ptr %rtdbg, ptr %object, i8 1)
; ```)
;   %ref_count_ptr = getelementptr inbounds %KeidAbiClassData, ptr %object, i32 0, i32 1
;   %ref_count = load i64, ptr %ref_count_ptr, align 4 ; load the ref count from %object
;   %ref_count_is_invalid = icmp slt i64 %ref_count, 1
;   br i1 %ref_count_is_invalid, label %block.is_invalid, label %block.is_not_null

; block.is_invalid:
;   call i32 @printf(ptr @illegal_ref_count_error, i64 %ref_count)
;   call void @"core::runtime::printStackFrames()"()
;   call void @exit(i32 1)
;   unreachable

; block.is_not_null:
;   %ref_count_is_null = icmp eq i64 %ref_count, 0
;   br i1 %ref_count_is_null, label %block.is_null, label %block.test_should_free

; block.test_should_free:
;   %should_free = icmp eq i64 %ref_count, 1     ; check if should free; i.e. `if ref count == 1`
;   br i1 %should_free, label %block.should_free, label %block.should_not_free

; block.should_free:                     
;   ; if ref count == 1 call the object's destructor and then free it
;   %classinfo_ptr_offset = getelementptr inbounds %KeidAbiClassData, ptr %object, i32 0, i32 0
;   %classinfo_ptr = load ptr, ptr %classinfo_ptr_offset, align 4 ; load the classinfo pointer from the class data

;   %destructor_ptr_offset = getelementptr inbounds %KeidAbiClassInfo, ptr %classinfo_ptr, i32 0, i32 0
;   %destructor_ptr = load ptr, ptr %destructor_ptr_offset, align 4 ; dereference the classinfo pointer to get the destructor pointer

;   call void %destructor_ptr(ptr %object) ; invoke the object's destructor method

;   %raw_pointer = bitcast ptr %object to ptr
;   tail call void @keid_free(ptr %raw_pointer) ; free the object's heap memory
;   ret void

; block.should_not_free:
;   ; if ref count != 1 (i.e. is > 1):
;   %new_count = sub i64 %ref_count, 1          ; decrease the ref count by 1
;   store i64 %new_count, ptr %ref_count_ptr, align 4 ; store the (ref count - 1) into %ref_count_ptr
  ret void
}

define void @keid.throw_error(ptr %error) {
block.check_error:
  %has_error = call i1 @keid.check_unhandled_error()
  br i1 %has_error, label %block.has_error, label %block.set_error
block.has_error:
  call i32 @printf(ptr @error_already_thrown_error)
  call void @"core::runtime::printStackFrames()"()
  call void @exit(i32 1)
  unreachable
block.set_error:
  store ptr %error, ptr @current_error
  ret void
}

; Immediately invoked at the start of all catch statements.
define void @keid.clear_unhandled_error() {
block.main:
  store ptr null, ptr @current_error
  ret void
}

define i1 @keid.check_unhandled_error() {
block.main:
  %current_error = load ptr, ptr @current_error, align 4
  %current_error_is_not_null = icmp ne ptr %current_error, null
  ret i1 %current_error_is_not_null
}

define ptr @keid.get_unhandled_error() {
block.main:
  %current_error = load ptr, ptr @current_error, align 4
  ret ptr %current_error
}

; Only works for testing for object assignability via direct single inheritance.
; Use `keid.is_implementer_of` to check object assignability to interface types. 
define i1 @keid.is_assignable_to(ptr %object, ptr %target_class_info) {
block.start:
  %current_object_class_info_ptr = alloca ptr, align 4
  %initial_class_info_ptr_ptr = getelementptr inbounds %KeidAbiClassData, ptr %object, i32 0, i32 0
  %initial_class_info_ptr = load ptr, ptr %initial_class_info_ptr_ptr, align 8
  store ptr %initial_class_info_ptr, ptr %current_object_class_info_ptr
  br label %block.loop_top
block.loop_top:
  %current_object_class_info = load ptr, ptr %current_object_class_info_ptr, align 4
  %is_current_class_info_assignable = icmp eq ptr %current_object_class_info, %target_class_info
  br i1 %is_current_class_info_assignable, label %block.return_true, label %block.take_superclass
block.take_superclass:
  %superclass_ptr_ptr = getelementptr inbounds %KeidAbiClassInfo, ptr %current_object_class_info, i32 0, i32 6
  %superclass_ptr = load ptr, ptr %superclass_ptr_ptr, align 8
  store ptr %superclass_ptr, ptr %current_object_class_info_ptr
  %superclass_is_null = icmp eq ptr %superclass_ptr, null
  br i1 %superclass_is_null, label %block.return_false, label %block.loop_top
block.return_false:
  ret i1 false
block.return_true:
  ret i1 true
}

define void @_keid_start() {
block.main:
$IF(RTDBG, ```
  %rtdbg = call ptr @rtdbg_initialize()
  store ptr %rtdbg, ptr @rtdbg_instance
```)

  call void @"keid.init()"()
 
  ; now that initialization has finished, notify the runtime
  store i1 1, ptr @"core::runtime::hasInit", align 1

  ; invoke the user-defined main function
  call void @"keid.main()"()

  ; check if there is an unhandled error from the main function
  %unhandled_error = call ptr @keid.get_unhandled_error()
  %check_unhandled_error = icmp ne ptr %unhandled_error, null
  br i1 %check_unhandled_error, label %block.print_error, label %block.exit_gracefully
block.print_error:
  ; clear the error so that Error::print can execute
  call void @keid.clear_unhandled_error()
  call void @"core::error::Error::print(core::error::Error)"(ptr %unhandled_error)
  call void @keid_exit(i32 1)
  unreachable
block.exit_gracefully:
  call void @keid_exit(i32 0)
  unreachable
}

define void @keid_exit(i32 %code) {
block.main:
$IF(RTDBG, ```
  %rtdbg = load ptr, ptr @rtdbg_instance, align 4
  call void @rtdbg_finish(ptr %rtdbg)
```)

  call void @exit(i32 %code)
  unreachable
}

; wrappers of libc fuctions
define ptr @keid_malloc(i64 %bytes) {
block.main:
  %malloc = load ptr, ptr @allocator, align 4
  %res = call ptr %malloc(i64 %bytes)
  ret ptr %res
}

define void @keid_free(ptr %mem) {
block.main:
  %free = load ptr, ptr @deallocator, align 4
  call ptr %free(ptr %mem)
  ret void
}

; libc functions
define void @main() {
block.main:
  store ptr bitcast (ptr @malloc to ptr), ptr @allocator, align 8
  store ptr bitcast (ptr @free to ptr), ptr @deallocator, align 8

  call void @_keid_start()
  unreachable
}

declare void @exit(i32)
declare i32 @printf(ptr, ...)
declare void @free(ptr)
declare ptr @malloc(i32)

$IF(RTDBG, ```
; librtdbg functions
declare void @rtdbg_change_scope(ptr, ptr, i8)
declare ptr @rtdbg_initialize()
declare void @rtdbg_finish(ptr)
declare void @rtdbg_register_object(ptr, ptr)
declare void @rtdbg_clear_reference_count(ptr, ptr)
```)
declare void @rtdbg_dump_basic_type(ptr)

; keid core library functions
declare void @"keid.init()"()
declare void @"keid.main()"()
declare void @"core::array::copyFromPtr<char>(core::mem::Pointer<char>, usize)"(ptr, i64, ptr)
declare void @"core::mem::Pointer::to<char>(usize)"(i64, ptr)
declare ptr @"core::string::String::fromUtf8Slice([char])"(ptr)
declare void @"core::runtime::printStackFrames()"()
declare void @"core::error::Error::print(core::error::Error)"(ptr)
