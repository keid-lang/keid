use super::*;
use crate::compiler::llvm::Insn;

pub trait DestructorCompiler {
    fn compile_destructor(&mut self) -> Result<()>;
}

impl<'a> DestructorCompiler for FunctionCompiler<'a> {
    fn compile_destructor(&mut self) -> Result<()> {
        let metadata_func = self.cpl.class_info.module.add_function(
            &self.func.external_name,
            self.func.as_llvm_type(&self.cpl),
            0,
        );
        let metadata_func_ptr = self.cpl.context.const_func_ptr(metadata_func.as_val());

        let source = self.get_source_function();
        let (fields, destructor): (Vec<_>, _) = {
            let class = self.cpl.type_provider.get_declaring_class(&source).unwrap();
            self.cpl
                .class_info
                .set_destructor(class.module_id, class.id, metadata_func_ptr);
            (
                class
                    .fields
                    .iter()
                    .map(|field| {
                        crate::tree::extract_type(
                            &self.cpl.type_provider,
                            field.ty.clone(),
                            &class.generic_defs,
                            &self.func.generic_impls,
                        )
                    })
                    .collect::<anyhow::Result<_>>()
                    .map_err(|e| compiler_error!(&self, "{}", e))?,
                class.destructor.clone(),
            )
        };

        let this = self.state.block_stack[0].locals[0].value.clone();
        let this_type = match this.ty.get_root_type() {
            BasicType::Object(ident) => match self.cpl.type_provider.get_class_by_name(&ident) {
                Some(class_impl) => self
                    .cpl
                    .context
                    .get_abi_class_data_type(self.cpl, &class_impl),
                None => panic!("unresolved type: {}", ident.to_string()),
            },
            _ => unreachable!(),
        };

        if let Some(body) = destructor {
            self.compile_block(&body);
        }

        for i in 0..fields.len() {
            let field_type = &fields[i];
            if matches!(field_type, ComplexType::Basic(BasicType::Object { .. })) {
                let field_ptr = self.emit(Insn::GetElementPtr(this.val, this_type, 2 + i as u32)); // offset of 2 for (ref count) + (classinfo ptr)
                let field = TypedValueContainer(TypedValue::new(field_type.clone(), field_ptr))
                    .load(self)?;

                self.try_unscope(&TypedValue::new(field_type.clone(), field))?;
            } else if matches!(
                field_type,
                ComplexType::Nullable(box ComplexType::Basic(BasicType::Object { .. }))
            ) {
                let field_ptr = self.emit(Insn::GetElementPtr(this.val, this_type, 2 + i as u32)); // offset of 2 for (ref count) + (classinfo ptr)
                self.try_unscope(&TypedValue::new(field_type.clone(), field_ptr))?;
            }
        }

        self.emit(Insn::RetVoid);
        self.state.block_stack.clear();

        Ok(())
    }
}
