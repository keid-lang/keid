use super::*;
use crate::compiler::llvm::Insn;

pub trait GlobalInitializerCompiler {
    fn compile_global_initializer(&mut self) -> Result<()>;
}

impl<'a> GlobalInitializerCompiler for FunctionCompiler<'a> {
    fn compile_global_initializer(&mut self) -> Result<()> {
        let module_ids: Vec<usize> = self.cpl.units.iter().map(|unit| unit.module_id).collect();

        for module_id in module_ids {
            let (fields, imports) = {
                let module = self.cpl.type_provider.get_module(module_id);
                (module.fields.clone(), module.imports.clone())
            };
            self.import_map = utils::get_import_map(&imports, &self.cpl.type_provider);
            for field in fields {
                let field_type = self.resolve_type(&field.ty)?;
                let llvm_type = field_type.as_llvm_type(&self.cpl);

                {
                    let mdl = &mut self.cpl.units[module_id].mdl;
                    let global = mdl.create_global(&mut self.cpl.context, &field.name, llvm_type);
                    mdl.initialize_global(global, self.cpl.context.const_null(llvm_type));
                }

                match &field.initial_value {
                    Some(initial_value) => {
                        let compiled = self.compile_expr(initial_value, Some(&field_type))?;
                        self.assert_assignable_to(&compiled.ty, &field_type)?;

                        let global_ref = self.unit.mdl.extern_global(&GlobalVariable {
                            name: field.name.clone(),
                            ty: llvm_type,
                        });
                        self.store(Operator::Equals, compiled, &TypedValue::new(field_type.clone(), global_ref))?;
                    }
                    None => panic!("all global fields must have an initial value"),
                }
            }
        }

        self.emit(Insn::RetVoid);
        self.state.block_stack.clear();

        Ok(())
    }
}
