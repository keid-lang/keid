use crate::{
    common::types::BasicType,
    common::Result,
    compiler::llvm::{BuilderBlock, Insn},
    compiler_error,
    func::*,
};

use super::{AssignmentCompiler, CallCompiler, ExprCompiler, ReturnCompiler};

pub trait BlockCompiler {
    fn compile_block(&mut self, block: &[Token<Statement>]) -> bool;
    fn compile_if_chain(&mut self, if_chain: &IfChain) -> Result<()>;
    fn compile_for_loop(&mut self, for_loop: &ForLoop) -> Result<()>;
    fn compile_while_loop(&mut self, while_loop: &WhileLoop) -> Result<()>;
    fn compile_indefinite_loop(&mut self, indef_loop: &Vec<Token<Statement>>) -> Result<()>;
    fn compile_block_statement(
        &mut self,
        block: &[Token<Statement>],
        block_type: BlockType,
    ) -> bool;
    fn compile_fixed_block(&mut self, fixed: &FixedBlock) -> Result<bool>;
    fn compile_throw(&mut self, expr: &Token<Expr>) -> Result<()>;
    fn compile_try_catch(&mut self, try_catch: &TryCatch) -> Result<()>;
}

impl<'a> BlockCompiler for FunctionCompiler<'a> {
    /// Returns true if the block ends with a return statement, otherwise false.
    fn compile_block(&mut self, block: &[Token<Statement>]) -> bool {
        let mut returns = false;
        for tkn in block {
            self.loc(&tkn.loc);
            if returns && tkn.token != Statement::Unreachable {
                self.state.errors.push(compiler_error!(
                    self,
                    "Illegal statement: after a return statement in the same block"
                ));
                return true;
            }
            let result = match &tkn.token {
                Statement::Unreachable => {
                    self.emit(Insn::Unreachable);
                    self.state.block_stack.pop();

                    Ok(())
                }
                Statement::Return(return_val) => {
                    // popping the block will generate keid.unscope() calls
                    // thus, `compile_return` needs to be compiled after it (the return instruction must be the last)
                    // `compile_return` expects that there be an active block though
                    // to fix this, we first clone the top-level block's locals
                    let cloned_locals = self.state.get_current_block().locals.clone();

                    // then we generate all the keid.unscope() calls
                    let block_vars: Vec<Vec<LocalVar>> = self
                        .state
                        .block_stack
                        .iter()
                        .map(|block| block.locals.clone())
                        .collect();
                    for vars in block_vars {
                        for var in vars {
                            match self.try_unscope(&var.value) {
                                Ok(_) => (),
                                Err(e) => {
                                    self.state.errors.push(e);
                                    return true;
                                }
                            }
                        }
                    }

                    // pop the cloned block without keid.scope() by popping the stack directly
                    let block_type = self.state.block_stack.pop().unwrap().block_type;

                    // create a new block and put all the locals in it
                    self.state.block_stack.push(ScopeBlock {
                        locals: cloned_locals,
                        llvm_block: BuilderBlock::null(),
                        block_type,
                    });

                    // compile the return statement
                    let res = self.compile_return(return_val);
                    returns = true;
                    // pop the cloned block without keid.scope() by popping the stack directly
                    self.state.block_stack.pop();
                    res
                }
                Statement::Let(lt) => self.compile_let(lt),
                Statement::StaticFuncCall(sfc) => self.compile_static_func_call(sfc).map(|_| (())),
                Statement::Assign(assign) => {
                    self.compile_assign(&assign.lhs, assign.op, &assign.rhs, assign.deref)
                }
                Statement::IfChain(if_chain) => self.compile_if_chain(if_chain),
                Statement::Expr(expr) => self.compile_expr(expr, None).map(|_| ()),
                Statement::ForLoop(for_loop) => self.compile_for_loop(for_loop),
                Statement::WhileLoop(while_loop) => self.compile_while_loop(while_loop),
                Statement::IndefiniteLoop(indef_loop) => self.compile_indefinite_loop(indef_loop),
                Statement::Block(block) => {
                    returns = self.compile_block_statement(block, BlockType::Generic);
                    Ok(())
                }
                Statement::UnsafeBlock(block) => {
                    returns = self.compile_block_statement(block, BlockType::Unsafe);
                    Ok(())
                }
                Statement::FixedBlock(fixed) => match self.compile_fixed_block(fixed) {
                    Ok(r) => {
                        returns = r;
                        Ok(())
                    }
                    Err(e) => Err(e),
                },
                Statement::Throw(throw) => {
                    let res = self.compile_throw(throw);
                    returns = true;
                    res
                },
                Statement::TryCatch(try_catch) => self.compile_try_catch(try_catch),
                x => unimplemented!("{:#?}", x),
            };
            match result {
                Ok(_) => continue,
                Err(e) => self.state.errors.push(e),
            }
        }

        returns
    }

    fn compile_fixed_block(&mut self, block: &FixedBlock) -> Result<bool> {
        self.compile_let(&block.variable)?;
        let res = self.compile_block_statement(&block.block, BlockType::Generic);

        Ok(res)
    }

    fn compile_block_statement(
        &mut self,
        block: &[Token<Statement>],
        block_type: BlockType,
    ) -> bool {
        // for this we make a "virtual" block that has its own scope
        // but a new block is not added to the LLVM IR

        let new_block = self.builder.create_block();
        let rotated_parent_block = self.builder.create_block();

        self.emit(Insn::Br(new_block.as_val())); // unconditionally break to the new block

        self.builder.use_block(&new_block);
        new_block.append();

        self.state.block_stack.push(ScopeBlock {
            locals: Vec::new(),
            llvm_block: new_block,
            block_type,
        });
        let result = self.compile_block(&block);
        if !result {
            self.pop_block().unwrap();
            self.emit(Insn::Br(rotated_parent_block.as_val())); // unconditionally break to the rotated parent block
            self.builder.use_block(&rotated_parent_block);
            rotated_parent_block.append();
        }

        result
    }

    fn compile_if_chain(&mut self, if_chain: &IfChain) -> Result<()> {
        let mut blocks = Vec::new();

        // TODO: support else blocks

        for conditional in &if_chain.conditionals {
            let compiled_test =
                self.compile_expr(&conditional.test, Some(&BasicType::Bool.to_complex()))?;
            self.loc(&conditional.test.loc);
            self.assert_assignable_to(&compiled_test.ty, &BasicType::Bool.to_complex())?;

            let if_block = self.state.new_block(&mut self.builder);
            blocks.push((compiled_test, if_block, &conditional.body));
        }

        let rotated_parent = self.state.new_rotated_parent(&mut self.builder);
        let block_len = blocks.len();
        for i in 0..block_len {
            let (test, then_block, body) = blocks.remove(0);

            // if the next block is in the chain (i.e. an `else if` or `else` block)
            // then conditionally branch to that block as the fallback
            // otherwise, if it's the last block in the chain, branch to the parent block
            let else_block = if i == block_len - 1 {
                rotated_parent.clone()
            } else {
                blocks[i + 1].1.clone()
            };

            self.emit(Insn::CondBr(
                test.val,
                then_block.llvm_block.as_val(),
                else_block.llvm_block.as_val(),
            ));

            self.state.push_block(&self.builder, then_block);
            self.state.get_current_block().llvm_block.append(); // append the rotated parent block

            if !self.compile_block(body) {
                self.emit(Insn::Br(else_block.llvm_block.as_val()));
                self.pop_block()?;
            }

            self.state.block_stack.pop(); // pop the parent block off the stack

            self.state.push_block(&self.builder, else_block);
            self.state.get_current_block().llvm_block.append(); // append the rotated parent block
        }

        Ok(())
    }

    fn compile_while_loop(&mut self, while_loop: &WhileLoop) -> Result<()> {
        let rotated_parent = self.state.new_rotated_parent(&mut self.builder); // create a copy ("rotation") of the current parent block
        let test_block = self.state.new_rotated_parent(&mut self.builder);
        let loop_block = self.state.new_rotated_parent(&mut self.builder);

        self.emit(Insn::Br(test_block.llvm_block.as_val()));

        self.builder.use_block(&test_block.llvm_block);
        test_block.llvm_block.append();

        let test_expr =
            self.compile_expr(&while_loop.condition, Some(&BasicType::Bool.to_complex()))?;
        self.assert_assignable_to(&test_expr.ty, &BasicType::Bool.to_complex())?;

        self.emit(Insn::CondBr(
            test_expr.val,
            loop_block.llvm_block.as_val(),
            rotated_parent.llvm_block.as_val(),
        ));

        self.builder.use_block(&loop_block.llvm_block);
        loop_block.llvm_block.append();

        self.compile_block(&while_loop.block);
        self.pop_block()?; // pop `loop_block` and all of its variables

        self.emit(Insn::Br(test_block.llvm_block.as_val())); // after the main body and unscope calls, go back to the top of the loop

        self.state.push_block(&self.builder, rotated_parent);
        self.state.get_current_block().llvm_block.append(); // append the rotated parent block

        Ok(())
    }

    fn compile_throw(&mut self, expr: &Token<Expr>) -> Result<()> {
        let error_type =
            BasicType::Object(GenericIdentifier::from_name("core::error::Error")).to_complex();
        let error = self.compile_expr(expr, Some(&error_type))?;
        self.assert_assignable_to(&error.ty, &error_type)?;

        let throw_error_callable = ResolvedFunctionNode::externed(
            "keid.throw_error",
            &[error_type],
            Varargs::None,
            BasicType::Void.to_complex(),
        );
        let throw_error_callable_ref = self.get_function_ref(&throw_error_callable)?;
        self.call_function(throw_error_callable_ref, &throw_error_callable, &[error])?;
        self.handle_unhandled_error(false)?;

        Ok(())
    }

    fn compile_for_loop(&mut self, for_loop: &ForLoop) -> Result<()> {
        let mut source_iterator_ptr = self.compile_expr(&for_loop.iterator, None)?;

        match &source_iterator_ptr.ty {
            ComplexType::Array(element_type) => {
                let iterator_type = self
                    .cpl
                    .type_provider
                    .get_class_by_name(&GenericIdentifier::from_name_with_args(
                        "core::collections::ArrayIterator",
                        &[*element_type.clone()],
                    ))
                    .unwrap();
                // retrieving the classinfo pointer ensures that it gets included in the runtime metadata
                self.cpl.class_info.get_abi_class_info_ptr(
                    &self.cpl.context,
                    &self.unit.mdl,
                    &iterator_type,
                );

                let create_impl = self
                    .cpl
                    .type_provider
                    .get_function_by_name(
                        &GenericIdentifier::from_name_with_args(
                            "core::collections::ArrayIterator::create",
                            &[*element_type.clone()],
                        ),
                        &[(*element_type.clone()).to_array()],
                    )
                    .unwrap();

                let create_ref = self.get_function_ref(&create_impl)?;
                source_iterator_ptr = TypedValue::new(
                    create_impl.return_type.clone(),
                    self.call_function(create_ref, &create_impl, &[source_iterator_ptr])?,
                );
            }
            _ => (),
        }

        let iterator_ptr = match self.upcast(
            source_iterator_ptr.clone(),
            "core::collections::Iterator",
            None,
        )? {
            Some(iterator_ptr) => iterator_ptr,
            None => {
                return Err(compiler_error!(
                    self,
                    "Type `{}` must be an instance of `core::collections::Iterator<T>`",
                    source_iterator_ptr.ty.to_string()
                ))
            }
        };

        let iterator_interface_impl = self
            .cpl
            .type_provider
            .get_class_by_name(&GenericIdentifier::from_complex_type(&iterator_ptr.ty))
            .unwrap();
        let iterator_interface = self
            .cpl
            .type_provider
            .get_source_class(&iterator_interface_impl);

        let interface_id = self.cpl.type_provider.get_resolved_interface_id(
            &GenericIdentifier::from_name_with_args(
                &iterator_interface.base_name,
                &iterator_interface_impl.generic_impls,
            ),
        );
        let get_next_id = 0; // there's only one function (__get_next) in core::collections::Iterator<T>

        let get_next_method_ptr = self.get_interface_method_ptr(
            &InterfaceInvocation::Instance(iterator_ptr.clone()),
            interface_id,
            get_next_id,
        )?;

        let element_type = match &iterator_ptr.ty {
            ComplexType::Basic(BasicType::Object(ident)) => ident.generic_args[0].clone(),
            _ => unreachable!(),
        };
        let nullable_element_type = ComplexType::Nullable(Box::new(element_type.clone()));

        let rotated_parent = self.state.new_rotated_parent(&mut self.builder); // create a copy ("rotation") of the current parent block

        // GET NEXT BLOCK -- this block calls __get_next and checks if it is null.
        // If the value is null, then the loop exits and jumps out to `rotated_parent`.
        // Otherwise, if the value is non-null, the loop executes its block `loop_block`.
        let get_next_block = self.state.new_block(&mut self.builder);
        let loop_block = self.state.new_block(&mut self.builder);

        self.emit(Insn::Br(get_next_block.llvm_block.as_val()));

        self.builder.use_block(&get_next_block.llvm_block);
        get_next_block.llvm_block.append();

        let get_next_method_impl = self
            .cpl
            .type_provider
            .get_function_by_name(
                &GenericIdentifier::from_name_with_args(
                    "core::collections::Iterator::__get_next",
                    &[element_type.clone()],
                ),
                &[iterator_ptr.ty.clone()],
            )
            .unwrap();

        let next_element =
            self.call_function(get_next_method_ptr, &get_next_method_impl, &[iterator_ptr])?; // calls __get_next

        let nullability_ptr = self.emit(Insn::GetElementPtr(
            next_element,
            nullable_element_type.as_llvm_type(&self.cpl),
            1,
        )); // nullable type nullability
        let nullability = self.emit(Insn::Load(nullability_ptr, self.cpl.context.get_i8_type()));
        let const_zero = self
            .cpl
            .context
            .const_int(self.cpl.context.get_i8_type(), 0);
        let is_null = self.emit(Insn::ICmp(IntPredicate::LLVMIntEQ, nullability, const_zero));

        self.emit(Insn::CondBr(
            is_null,
            rotated_parent.llvm_block.as_val(),
            loop_block.llvm_block.as_val(),
        ));

        // "loop_block" is a psuedo-block that just jumps back to the `get_next_block`
        self.state.push_block(&self.builder, loop_block);
        self.state.get_current_block().llvm_block.append();

        let value_ptr = self.emit(Insn::GetElementPtr(
            next_element,
            nullable_element_type.as_llvm_type(&self.cpl),
            0,
        )); // nullable type value
        let value = self.emit(Insn::Load(value_ptr, element_type.as_llvm_type(&self.cpl)));

        self.state.get_current_block_mut().locals.push(LocalVar {
            name: for_loop.variable.token.0.clone(),
            source: LocalVarSource::Scalar,
            value: TypedValue {
                ty: element_type.clone(),
                val: value,
            },
        });

        // compile the `loop_block`
        let footer_block = self.builder.create_block();

        self.compile_block(&for_loop.block);
        self.pop_block()?; // pop `loop_block` and all of its variables

        self.try_unscope(&source_iterator_ptr)?;
        self.emit(Insn::Br(footer_block.as_val())); // after the main body and unscope calls, break to the `footer_block`

        footer_block.append();
        self.builder.use_block(&footer_block); // switch to the `footer_block` so we can loop it back up to the `get_next_block`
        self.emit(Insn::Br(get_next_block.llvm_block.as_val()));

        self.state.block_stack.pop(); // pop the parent block
        self.state.push_block(&self.builder, rotated_parent);
        self.state.get_current_block().llvm_block.append(); // append the rotated parent block

        Ok(())
    }

    fn compile_indefinite_loop(&mut self, indef_loop: &Vec<Token<Statement>>) -> Result<()> {
        let rotated_parent = self.state.new_rotated_parent(&mut self.builder); // create a copy ("rotation") of the current parent block

        let loop_block = self.state.new_block(&mut self.builder); // contains the body of the loop
        let loop_block_llvm = loop_block.llvm_block.as_val();

        self.emit(Insn::Br(loop_block_llvm)); // jump to the loop block

        self.state.push_block(&self.builder, loop_block);
        self.state.get_current_block().llvm_block.append();

        self.compile_block(indef_loop); // compile the contents of the loop
        self.pop_block()?; // pop the loop block

        self.emit(Insn::Br(loop_block_llvm)); // jump to the top of the loop block

        self.state.block_stack.pop(); // pop the parent block
        self.state.push_block(&self.builder, rotated_parent);
        self.state.get_current_block().llvm_block.append(); // append the rotated parent block

        Ok(())
    }

    fn compile_try_catch(&mut self, try_catch: &TryCatch) -> Result<()> {
        let rotated_parent = self.state.new_rotated_parent(&mut self.builder);

        let try_block = self.builder.create_block();
        let catch_block = self.builder.create_block();

        self.emit(Insn::Br(try_block.as_val()));

        // try block
        {
            let try_scope_block = ScopeBlock {
                llvm_block: try_block,
                locals: Vec::new(),
                block_type: BlockType::Try(TryBlock {
                    catch_block: catch_block.clone(),
                }),
            };

            self.state.push_block(&self.builder, try_scope_block);
            self.state.get_current_block().llvm_block.append();

            // compile the contents of the try block
            self.compile_block(&try_catch.try_block);
            self.pop_block()?; // pop the try block

            self.emit(Insn::Br(rotated_parent.llvm_block.as_val())); // jump to after the catch block
        }
        // catch block
        {
            let catch_scope_block = ScopeBlock {
                llvm_block: catch_block,
                locals: Vec::new(),
                block_type: BlockType::Generic,
            };
            self.state.push_block(&self.builder, catch_scope_block);
            self.state.get_current_block().llvm_block.append();

            let error_type =
                BasicType::Object(GenericIdentifier::from_name("core::error::Error")).to_complex();
            let get_unhandled_error_callable = ResolvedFunctionNode::externed(
                "keid.get_unhandled_error",
                &[],
                Varargs::None,
                error_type.clone(),
            );
            let get_unhandled_error_ref = self.get_function_ref(&get_unhandled_error_callable)?;
            let unhandled_error =
                self.call_function(get_unhandled_error_ref, &get_unhandled_error_callable, &[])?;

            let clear_unhandled_error_callable = ResolvedFunctionNode::externed(
                "keid.clear_unhandled_error",
                &[],
                Varargs::None,
                BasicType::Void.to_complex(),
            );
            let clear_unhandled_error_ref =
                self.get_function_ref(&clear_unhandled_error_callable)?;
            self.call_function(
                clear_unhandled_error_ref,
                &clear_unhandled_error_callable,
                &[],
            )?;

            self.state.get_current_block_mut().locals.push(LocalVar {
                name: try_catch.error_var.as_ref().unwrap().token.0.clone(),
                source: LocalVarSource::Scalar,
                value: TypedValue {
                    ty: error_type.clone(),
                    val: unhandled_error,
                },
            });

            // compile the contents of the catch block
            self.compile_block(&try_catch.catch_block);
            self.pop_block()?; // pop the catch block
        
            self.emit(Insn::Br(rotated_parent.llvm_block.as_val())); // jump to after the catch block
        }

        self.state.block_stack.pop(); // pop the parent block
        self.state.push_block(&self.builder, rotated_parent);
        self.state.get_current_block().llvm_block.append(); // append the rotated parent block

        Ok(())
    }
}
