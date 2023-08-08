use crate::{
    common::CompilerError,
    compiler::llvm::{BuilderBlock, OpaqueFunctionValue},
    tree::ast::TokenLocation,
};

use super::LocalVar;

#[derive(Debug, Clone)]
pub struct ExternedFunction {
    pub external_name: String,
    pub value_ref: OpaqueFunctionValue,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TryBlock {
    pub catch_block: BuilderBlock,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BlockType {
    Generic,
    Unsafe,
    Try(TryBlock),
    Loop {
        head: BuilderBlock,
        after: BuilderBlock,
    },
}

#[derive(Debug, Clone)]
pub struct ScopeBlock {
    pub locals: Vec<LocalVar>,
    pub block_type: BlockType,
}

impl ScopeBlock {
    pub fn new(block_type: BlockType) -> ScopeBlock {
        ScopeBlock {
            locals: Vec::new(),
            block_type,
        }
    }
}

#[derive(Debug)]
pub struct FunctionCompilerState {
    pub errors: Vec<CompilerError>,
    pub current_token: TokenLocation,
    pub block_stack: Vec<ScopeBlock>,
}

impl FunctionCompilerState {
    pub fn new() -> FunctionCompilerState {
        FunctionCompilerState {
            errors: Vec::new(),
            current_token: TokenLocation {
                start: 0,
                end: 0,
            },
            block_stack: Vec::new(),
        }
    }

    pub fn push_block(&mut self, block: ScopeBlock) {
        self.block_stack.push(block);
    }

    pub fn get_current_block(&self) -> &ScopeBlock {
        self.block_stack.last().unwrap()
    }

    pub fn get_current_block_mut(&mut self) -> &mut ScopeBlock {
        self.block_stack.last_mut().unwrap()
    }
}
