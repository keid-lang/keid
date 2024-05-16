; This file contains implementations of the ABI-dependent functionality of the Keid reflection API.
source_filename = "abi.ll"

; heap data for a class: ptr (classinfo pointer), i64 (ref count)
%KeidAbiClassData = type { ptr, i64 }

; constant class metadata:
;  ptr (pointer to class destructor),
;  ptr (pointer to global virtual method array),
;  i32 (length of interface impls)
;  ptr to [%KeidAbiInterfaceImpl] (n-length interface impls)
;  ptr (class name string)
;  i32 (class flags)
;    TYPE_CLASS  = 0x00
;    TYPE_STRUCT = 0x01
;  ptr to %KeidAbiClassInfo (superclass pointer, or null if this represents the core::object::Object type)
%KeidAbiClassInfo = type { ptr, ptr, i32, ptr, ptr, i32, ptr }

; const char* keid_reflect_get_object_class_name(KeidAbiClass*)
define ptr @keid_reflect_get_object_class_name(ptr %object) {
  %class_info_ptr = getelementptr inbounds %KeidAbiClassData, ptr %object, i32 0, i32 0
  %class_info = load ptr, ptr %class_info_ptr, align 4 ; load the class info pointer from the class data
  %class_name_ptr = getelementptr inbounds %KeidAbiClassInfo, ptr %class_info, i32 0, i32 4
  %class_name = load ptr, ptr %class_name_ptr, align 4 ; load the class name pointer from the class info

  ret ptr %class_name
}
