use crate::{
    common::CompilerError,
    compiler::llvm::{BuilderBlock, InsnBuilder, OpaqueFunctionValue},
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
    pub llvm_block: BuilderBlock,
    pub block_type: BlockType,
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

    pub fn new_block(&mut self, bdl: &mut InsnBuilder) -> ScopeBlock {
        ScopeBlock {
            llvm_block: bdl.create_block(),
            locals: Vec::new(),
            block_type: BlockType::Generic,
        }
    }

    pub fn new_rotated_parent(&mut self, bdl: &mut InsnBuilder) -> ScopeBlock {
        let current_block = self.get_current_block();
        ScopeBlock {
            llvm_block: bdl.create_block(),
            locals: current_block.locals.clone(),
            block_type: current_block.block_type.clone(),
        }
    }

    pub fn push_block(&mut self, bdl: &InsnBuilder, block: ScopeBlock) {
        bdl.use_block(&block.llvm_block);
        self.block_stack.push(block);
    }

    pub fn get_current_block(&self) -> &ScopeBlock {
        self.block_stack.last().unwrap()
    }

    pub fn get_current_block_mut(&mut self) -> &mut ScopeBlock {
        self.block_stack.last_mut().unwrap()
    }
}
