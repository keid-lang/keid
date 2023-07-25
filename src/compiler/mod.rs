mod linker;
pub use linker::*;

pub mod llvm;
use llvm::*;

mod metadata;
pub use metadata::*;

use crate::{
    common::{kpkg::KeidPackageData, types::*, *},
    func::*,
    parser::preprocessor::{self, PreprocessorContext},
    tree::{self, ast::*, *},
};

#[derive(Debug, Clone)]
pub struct CompilationUnit {
    pub module_id: usize,
    pub path_name: String,
    pub mdl: Module,
    pub externed_functions: Vec<ExternedFunction>,
}

impl CompilationUnit {
    pub fn new(module_id: usize, mdl: Module, path_name: String) -> CompilationUnit {
        CompilationUnit {
            module_id,
            mdl,
            path_name,
            externed_functions: Vec::new(),
        }
    }
}

pub struct SignatureCompiler {
    pub source_files: Vec<KeidFile>,
}

pub struct SignatureCompilation {
    pub units: Vec<CompilationUnit>,
    pub type_provider: TypeProvider,
    pub errors: Vec<(CompilerError, usize)>,
}

impl SignatureCompiler {
    pub fn new() -> SignatureCompiler {
        tree::reset(); // reset the type ID nonce

        SignatureCompiler {
            source_files: Vec::new(),
        }
    }

    pub fn add_file(&mut self, file: KeidFile) {
        if self
            .source_files
            .iter()
            .find(|src| src.source_path == file.source_path)
            .is_some()
        {
            // don't add files twice
            return;
        }
        self.source_files.push(file);
    }

    pub fn compile(&self, root: &str, context: &mut Context) -> SignatureCompilation {
        let mut units = Vec::with_capacity(self.source_files.len());
        let mut type_provider = TypeProvider::new();

        // add the init module as the very first module
        {
            let init_mdl = context.create_module("keid/keid_init", "keid_init");
            units.push(CompilationUnit::new(0, init_mdl, "keid_init".to_owned()));

            type_provider.roots.push(KeidModuleNode {
                classes: Vec::new(),
                fields: Vec::new(),
                imports: Vec::new(),
                interface_impls: Vec::new(),
                typedefs: Vec::new(),
                enums: Vec::new(),
                functions: vec![FunctionNode {
                    base_name: "keid.init".to_owned(),
                    external_name: "keid.init".to_owned(),
                    namespace_name: "".to_owned(),
                    body: None,
                    modifiers: vec![FunctionModifier::Internal],
                    function_type: FunctionType::Static,
                    generic_defs: Vec::new(),
                    id: 0,
                    module_id: 0,
                    params: Vec::new(),
                    return_type: BasicType::Void.to_complex(),
                    varargs: Varargs::None,
                }],
                namespace: "keid::init".to_owned(),
            });
        }

        let mut module_id = 1;
        for file in &self.source_files {
            type_provider.include_file(file.clone(), module_id);

            let mdl = context.create_module(
                &file.source_path,
                &utils::path_to_module_name(root, &file.source_path),
            );
            units.push(CompilationUnit::new(
                module_id,
                mdl,
                file.source_path.clone(),
            ));
            module_id += 1;
        }

        let errors = type_provider.resolve_signatures();

        SignatureCompilation {
            units,
            type_provider,
            errors,
        }
    }
}

#[derive(Clone)]
struct QueuedFunction {
    func_impl: ResolvedFunctionNode,
    llvm_func: Function,
    unit_id: usize,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CompilationArtifactType {
    KeidPackage,
    NativeObject,
    LlvmIr,
}

pub struct CompilationArtifact {
    pub kind: CompilationArtifactType,
    pub name: String,
    pub data: Vec<u8>,
}

pub struct Compiler {
    pub target: String,
    pub context: Context,
    pub type_provider: TypeProvider,
    pub class_info: ClassInfoStorage,
    pub units: Vec<CompilationUnit>,
    errors: Vec<(CompilerError, usize)>,
    function_queue: Vec<QueuedFunction>,
}

impl Compiler {
    pub fn new(target: &str, class_info: ClassInfoStorage, context: Context) -> Compiler {
        Compiler {
            target: target.to_owned(),
            class_info,
            context,
            units: Vec::new(),
            errors: Vec::new(),
            type_provider: TypeProvider::new(),
            function_queue: Vec::new(),
        }
    }

    pub fn add_function(
        &self,
        mdl: &Module,
        external_name: &str,
        func: &ResolvedFunctionNode,
    ) -> Function {
        let function = mdl.add_function(external_name, func.as_llvm_type(self), 0);
        for i in 0..func.params.len() {
            if func.params[i].is_struct(&self.type_provider) {
                if !func.params[i].to_string().starts_with("core::mem::Pointer")
                    || (func.module_id != usize::MAX && {
                        let source = self.type_provider.get_source_function(func);
                        !source.modifiers.contains(&FunctionModifier::Extern)
                    })
                {
                    function.add_param_attribute(i + 1, func.params[i].as_llvm_type(self), "byval");
                }
            }
        }
        function
    }

    pub fn queue_function_compilation(
        &mut self,
        func_impl: ResolvedFunctionNode,
    ) -> OpaqueFunctionValue {
        if let Some(func) = self
            .type_provider
            .get_compiled_function(&func_impl.external_name)
        {
            return func;
        }

        let unit = self
            .units
            .iter()
            .enumerate()
            .find(|(_, unit)| unit.module_id == func_impl.module_id)
            .unwrap();

        let llvm_func = self.add_function(&unit.1.mdl, &func_impl.external_name, &func_impl);
        let llvm_func_val = llvm_func.as_val();
        self.type_provider
            .add_compiled_function(&func_impl.external_name, llvm_func_val);

        self.function_queue.push(QueuedFunction {
            func_impl,
            llvm_func,
            unit_id: unit.0,
        });

        llvm_func_val
    }

    fn compile_loop(&mut self) -> bool {
        let mut has_error = false;
        loop {
            let len = self.function_queue.len();
            if len == 0 {
                return has_error;
            }

            for _ in 0..len {
                let queued_function = self.function_queue.remove(0);
                let unit_id = queued_function.unit_id;

                let import_map = {
                    let module = self.type_provider.get_module(self.units[unit_id].module_id);
                    utils::get_import_map(
                        &module.imports,
                        &self.type_provider,
                        Some(
                            &self
                                .type_provider
                                .get_module_namespace(self.units[unit_id].module_id),
                        ),
                    )
                };

                let unit = self.units[unit_id].clone();
                let unit = {
                    let mut func_cpl = FunctionCompiler::new(
                        self,
                        unit,
                        &queued_function.func_impl,
                        import_map,
                        queued_function.llvm_func,
                    );
                    func_cpl.compile();

                    let (errors, consumed_unit) = func_cpl.consume();
                    if !errors.is_empty() {
                        has_error = true;
                        for error in errors {
                            self.errors.push((error, unit_id));
                        }
                    }

                    consumed_unit
                };
                self.units[unit_id] = unit;
            }
        }
    }

    pub fn get_errors(&self) -> Vec<(String, CompilerError)> {
        let mut errors = Vec::with_capacity(self.errors.len());
        for (error, unit_id) in &self.errors {
            let path_name = &self.units[*unit_id].path_name;
            errors.push((path_name.clone(), error.clone()));
        }
        errors
    }

    pub fn compile(&mut self, resources: SignatureCompilation, target: &LLVMTargetData) -> bool {
        self.units = resources.units;
        self.type_provider = resources.type_provider;

        for unit in &mut self.units {
            let array_type = self
                .context
                .get_array_type(self.context.get_abi_class_info_type(), 0);
            unit.mdl.extern_global(&GlobalVariable {
                name: "keid.classinfo".to_string(),
                ty: array_type,
            });
        }

        if !resources.errors.is_empty() {
            for error in resources.errors {
                self.errors.push(error);
            }
            return true;
        }

        // Phase 0 -- compile core required functions
        self.queue_function_compilation(
            self.type_provider
                .get_function_by_name(&GenericIdentifier::from_name("keid.init"), &[])
                .unwrap(),
        );
        self.queue_function_compilation(
            self.type_provider
                .get_function_by_name(
                    &GenericIdentifier::from_name("core::runtime::printStackFrames"),
                    &[],
                )
                .unwrap(),
        );
        self.queue_function_compilation(
            self.type_provider
                .get_function_by_name(
                    &GenericIdentifier::from_name("core::error::Error::print"),
                    &[
                        BasicType::Object(GenericIdentifier::from_name("core::error::Error"))
                            .to_complex(),
                    ],
                )
                .unwrap(),
        );
        if self.compile_loop() {
            return true;
        }

        let module_ids: Vec<usize> = self.units.iter().map(|unit| unit.module_id).collect();
        for module_id in module_ids {
            if let Some(main_func_impl) = 'block: {
                let module = self.type_provider.get_module(module_id);

                for function in &module.functions {
                    if function.external_name == "keid.main" {
                        // start by just compiling the main function
                        break 'block Some(function.create_impl(&self.type_provider, &[]).unwrap());
                    }
                }

                None
            } {
                // Compiler Phase 1
                // queue main function for compilation
                self.queue_function_compilation(main_func_impl);

                if self.compile_loop() {
                    return true;
                }

                // Compiler Phase 2
                // queue interface implementations
                // since they can be called indirectly they need to be explicitly queued
                loop {
                    let interface_modules: Vec<Vec<(usize, Vec<usize>, Vec<ComplexType>)>> = self
                        .type_provider
                        .get_all_resolved_classes()
                        .iter()
                        .map(|class_impl| {
                            let source = self.type_provider.get_source_class(class_impl);
                            let class = GenericIdentifier::from_name_with_args(
                                &source.base_name,
                                &class_impl.generic_impls,
                            );
                            self.type_provider
                                .get_resolved_interface_impls(&class)
                                .iter()
                                .map(|resolved_interface_impl| {
                                    let interface_impl = self
                                        .type_provider
                                        .get_source_interface_impl(resolved_interface_impl);
                                    let mut all_functions = interface_impl.functions.clone();
                                    all_functions.extend(
                                        interface_impl
                                            .accessors
                                            .iter()
                                            .map(|accessor| accessor.function_id),
                                    );
                                    (
                                        interface_impl.module_id,
                                        all_functions,
                                        class_impl.generic_impls.clone(),
                                    )
                                })
                                .collect()
                        })
                        .collect();

                    let mut queued = 0;
                    for modules in interface_modules {
                        for (module_id, functions, generics) in modules {
                            for function_id in functions {
                                let node = self
                                    .type_provider
                                    .get_function_node(module_id, function_id)
                                    .unwrap();

                                let name = GenericIdentifier::from_name_with_args(
                                    &node.base_name,
                                    generics.as_slice(),
                                );
                                let params = node
                                    .params
                                    .iter()
                                    .map(|param| {
                                        extract_type(
                                            &self.type_provider,
                                            param.ty.clone(),
                                            &node.generic_defs,
                                            &generics,
                                        )
                                    })
                                    .collect::<anyhow::Result<Vec<ComplexType>>>()
                                    .unwrap();
                                match self.type_provider.get_function_by_name(&name, &params) {
                                    Some(func) => {
                                        if self
                                            .type_provider
                                            .get_compiled_function(&func.external_name)
                                            .is_none()
                                        {
                                            self.queue_function_compilation(func);
                                            queued += 1;
                                        }
                                    }
                                    None => {
                                        panic!(
                                            "no such function: {}({})",
                                            name.to_string(),
                                            utils::iter_join(&params)
                                        )
                                    }
                                }
                            }
                        }
                    }

                    if queued == 0 {
                        break;
                    }

                    if self.compile_loop() {
                        return true;
                    }
                }

                // Compiler Phase 3
                // queue class destructors for compilation
                let class_names: Vec<_> = self
                    .class_info
                    .classes
                    .iter()
                    .filter(|cls| cls.class_impl.class_type != ClassType::Struct)
                    .map(|cls| {
                        let source = self.type_provider.get_source_class(&cls.class_impl);
                        (
                            source.base_name.clone(),
                            cls.class_impl.generic_impls.clone(),
                        )
                    })
                    .collect();
                for (class_name, generic_impls) in class_names {
                    let instance_type = BasicType::Object(GenericIdentifier::from_name_with_args(
                        &class_name,
                        &generic_impls,
                    ))
                    .to_complex();
                    let mut destructor_impl = self
                        .type_provider
                        .get_function_by_name(
                            &GenericIdentifier::from_name_with_args(
                                &format!("{}::keid.destructor", class_name),
                                &generic_impls,
                            ),
                            &[instance_type.clone()],
                        )
                        .unwrap();
                    destructor_impl.params[0] = instance_type;

                    self.queue_function_compilation(destructor_impl);
                }

                if self.compile_loop() {
                    return true;
                }
            }
        }

        self.class_info
            .create_class_info_storage(&mut self.context, &self.type_provider);

        if target.is_opaque_pointers {
            let processed_intrinsics = preprocessor::preprocess(
                include_str!("./intrinsics.ll"),
                &PreprocessorContext { use_rtdbg: false },
            )
            .unwrap();

            let string_class = self
                .type_provider
                .get_class_by_name(&GenericIdentifier::from_name("core::string::String"))
                .unwrap();
            let string_classinfo_offset = self
                .class_info
                .get_abi_class_info_offset(&self.context, &string_class)
                .to_string();
            let processed_intrinsics =
                processed_intrinsics.replace("$STRING_CLASSINFO_OFFSET", &string_classinfo_offset);

            let intrinsics_module = self
                .context
                .parse_llvm_ir(&processed_intrinsics, "intrinsics.ll");
            self.units.push(CompilationUnit {
                module_id: self.units.len(),
                path_name: "keid_intrinsics".to_string(),
                mdl: intrinsics_module,
                externed_functions: Vec::new(),
            });
        }

        let reflect_abi_module = self
            .context
            .parse_llvm_ir(include_str!("../../assets/core/object/abi.ll"), "abi.ll");
        self.units.push(CompilationUnit {
            module_id: self.units.len(),
            path_name: "keid_core_reflect_abi".to_string(),
            mdl: reflect_abi_module,
            externed_functions: Vec::new(),
        });

        self.units.push(CompilationUnit {
            module_id: self.units.len(),
            path_name: "keid_metadata".to_string(),
            mdl: self.class_info.module.clone(),
            externed_functions: Vec::new(),
        });

        false
    }

    pub fn create_artifacts(
        &mut self,
        root: &str,
        target: &LLVMTargetData,
    ) -> Vec<CompilationArtifact> {
        let mut artifacts = Vec::with_capacity(self.units.len());
        let config = bincode::config::standard();
        for unit in &self.units {
            let name = utils::path_to_module_name(root, &unit.path_name);

            if name != "keid_intrinsics"
                && name != "keid_metadata"
                && name != "keid_core_reflect_abi"
            {
                let module = self.type_provider.get_module(unit.module_id);
                let kpkg = KeidPackageData::new(module);

                artifacts.push(CompilationArtifact {
                    kind: CompilationArtifactType::KeidPackage,
                    name: name.to_owned(),
                    data: bincode::encode_to_vec(kpkg, config).expect("unable to encode kpkg"),
                });
            }

            if target.is_llvm_ir() {
                let ir = unit.mdl.to_llvm_ir();
                artifacts.push(CompilationArtifact {
                    kind: CompilationArtifactType::LlvmIr,
                    name: name.to_owned(),
                    data: ir.into_bytes(),
                })
            } else {
                let code = unit
                    .mdl
                    .to_object_code(&utils::path_to_module_name(root, &unit.path_name), target)
                    .unwrap();
                artifacts.push(CompilationArtifact {
                    kind: CompilationArtifactType::NativeObject,
                    name: name.to_owned(),
                    data: code.as_slice().to_vec(),
                })
            }
        }
        artifacts
    }
}
