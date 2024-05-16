static mut EVAL_COUNTER: u32 = 0;

pub fn set_eval_only(eval_only: bool) {
    unsafe {
        if eval_only {
            EVAL_COUNTER += 1;
        } else {
            EVAL_COUNTER -= 1;
        }
    }
}

pub fn get_eval_only() -> bool {
    unsafe { EVAL_COUNTER > 0 }
}

pub enum Insn {
    Unreachable,
    Nop,
    RetVoid,
    Ret(OpaqueValue),
    Alloca(OpaqueType),
    GlobalString(String),                                            // Returns pointer to global string
    Store(OpaqueValue, OpaqueValue),                                 // Store src par0 -> dest par1
    Load(OpaqueValue, OpaqueType),                                   // pointer, element type of pointer
    Call(OpaqueFunctionValue, OpaqueFunctionType, Vec<OpaqueValue>), // func ref, func type, args, return type
    GetElementPtr(OpaqueValue, OpaqueType, u32),                     // struct ref, struct type, value index
    GetElementPtrDynamic(OpaqueValue, OpaqueType, OpaqueValue),      // struct ref, struct type, dynamic value index
    CondBr(OpaqueValue, OpaqueBasicBlock, OpaqueBasicBlock),         // test, then, else
    Br(OpaqueBasicBlock),                                            // break to target
    ICmp(IntPredicate, OpaqueValue, OpaqueValue),                    // operator, lhs, rhs
    Xor(OpaqueValue, OpaqueValue),
    Trunc(OpaqueValue, OpaqueType), // truncate par0 per the type of par1
    IAdd(OpaqueValue, OpaqueValue),
    IAnd(OpaqueValue, OpaqueValue),
    ISub(OpaqueValue, OpaqueValue),
    IMul(OpaqueValue, OpaqueValue),
    IOr(OpaqueValue, OpaqueValue),
    IXor(OpaqueValue, OpaqueValue),
    SDiv(OpaqueValue, OpaqueValue),
    SRem(OpaqueValue, OpaqueValue),
    UDiv(OpaqueValue, OpaqueValue),
    URem(OpaqueValue, OpaqueValue),
    FAdd(OpaqueValue, OpaqueValue),
    FSub(OpaqueValue, OpaqueValue),
    FMul(OpaqueValue, OpaqueValue),
    FDiv(OpaqueValue, OpaqueValue),
    PointerCast(OpaqueValue, OpaqueType),
    Memset(OpaqueValue, OpaqueValue, OpaqueValue),  // Pointer, Value, Length
    Memmove(OpaqueValue, OpaqueValue, OpaqueValue), // src, dst, count
    PtrToInt(OpaqueValue),
    IntToPtr(OpaqueValue, OpaqueType), // value, target pointer type
    BitCast(OpaqueValue, OpaqueType),
    IntCast(OpaqueValue, OpaqueType, bool),    // value, dest type, is signed
    FloatCast(OpaqueValue, OpaqueType),        // value, dest type
    IntToFloat(OpaqueValue, OpaqueType, bool), // value, dest type, is signed
    FloatToInt(OpaqueValue, OpaqueType, bool), // value, dest type, is signed
    INeg(OpaqueValue),
    FNeg(OpaqueValue),
}

#[cfg(target_arch = "wasm32")]
#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum _Linkage {
    LLVMExternalLinkage = 0,
    LLVMAvailableExternallyLinkage = 1,
    LLVMLinkOnceAnyLinkage = 2,
    LLVMLinkOnceODRLinkage = 3,
    LLVMLinkOnceODRAutoHideLinkage = 4,
    LLVMWeakAnyLinkage = 5,
    LLVMWeakODRLinkage = 6,
    LLVMAppendingLinkage = 7,
    LLVMInternalLinkage = 8,
    LLVMPrivateLinkage = 9,
    LLVMDLLImportLinkage = 10,
    LLVMDLLExportLinkage = 11,
    LLVMExternalWeakLinkage = 12,
    LLVMGhostLinkage = 13,
    LLVMCommonLinkage = 14,
    LLVMLinkerPrivateLinkage = 15,
    LLVMLinkerPrivateWeakLinkage = 16,
}
#[cfg(not(target_arch = "wasm32"))]
use llvm_sys::LLVMLinkage as _Linkage;

pub type Linkage = _Linkage;

#[cfg(target_arch = "wasm32")]
#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum _IntPredicate {
    LLVMIntEQ = 32,
    LLVMIntNE = 33,
    LLVMIntUGT = 34,
    LLVMIntUGE = 35,
    LLVMIntULT = 36,
    LLVMIntULE = 37,
    LLVMIntSGT = 38,
    LLVMIntSGE = 39,
    LLVMIntSLT = 40,
    LLVMIntSLE = 41,
}
#[cfg(target_arch = "wasm32")]
#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum _Opcode {
    LLVMTrunc = 30,
    LLVMZExt = 31,
    LLVMSExt = 32,
    LLVMFPToUI = 33,
    LLVMFPToSI = 34,
    LLVMUIToFP = 35,
    LLVMSIToFP = 36,
    LLVMFPTrunc = 37,
    LLVMFPExt = 38,
}
#[cfg(not(target_arch = "wasm32"))]
use llvm_sys::LLVMIntPredicate as _IntPredicate;
#[cfg(not(target_arch = "wasm32"))]
use llvm_sys::LLVMOpcode as _Opcode;

pub type IntPredicate = _IntPredicate;
pub type Opcode = _Opcode;

#[cfg_attr(target_arch = "wasm32", path = "llvm_stub.rs")]
mod llvm_native;
pub use llvm_native::*;
