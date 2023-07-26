use anyhow::Result;
use std::{path::Path, process::Command};

pub enum ObjectFormat {
    Elf,
    // MachO,
    // Coff,
    // Wasm,
}

pub struct Linker {}

impl Linker {
    pub fn new() -> Linker {
        Linker {}
    }

    fn get_system_search_dirs(&self) -> Result<Vec<String>> {
        let cmd_output = String::from_utf8(Command::new("clang").arg("-print-search-dirs").output()?.stdout)?;
        for line in cmd_output.split('\n') {
            let line = line.trim();
            if line.starts_with("libraries: =") {
                return Ok(line["libraries: =".len()..line.len()]
                    .trim()
                    .split(':')
                    .filter(|dir| Path::new(dir).is_dir())
                    .map(String::from)
                    .collect());
            }
        }
        panic!("failed to parse output")
    }

    fn get_system_dynamic_linker(&self) -> Result<String> {
        for dir in self.get_system_search_dirs()? {
            for file in std::fs::read_dir(dir).unwrap() {
                let file = file.unwrap();
                if file.file_name().to_str().unwrap().starts_with("ld-linux-") {
                    return Ok(file.path().to_str().unwrap().to_owned());
                }
            }
        }
        panic!("no system linker found")
    }

    fn get_crt_libs_dir(&self) -> Result<String> {
        for dir in self.get_system_search_dirs()? {
            for file in std::fs::read_dir(&dir).unwrap() {
                let file = file.unwrap();
                if file.file_name().to_str().unwrap() == "crtn.o" {
                    return Ok(dir);
                }
            }
        }
        panic!("crt libs not found")
    }

    #[cfg(target_arch = "wasm32")]
    pub fn link<T: Into<std::path::PathBuf>>(&self, _: ObjectFormat, _: T) {
        panic!("linking is unsuported for this target")
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub fn link<T: Into<std::path::PathBuf>>(&self, target: ObjectFormat, output_dir: T) {
        use llvm_sys::linker::*;
        use std::ffi::CString;

        let output_dir = output_dir.into();
        // let llvm_path =
        //     std::env::var("LLVM_SYS_150_PREFIX").expect("no LLVM_SYS_150_PREFIX in env");
        let crt_dir = self.get_crt_libs_dir().unwrap();

        let mut args: Vec<String> = Vec::with_capacity(100);
        args.push("--error-limit=0".to_owned());
        args.push("-dynamic-linker".to_owned());
        args.push(self.get_system_dynamic_linker().expect("no system linker found"));
        args.push(format!("{}/crt1.o", crt_dir));
        args.push(format!("{}/crti.o", crt_dir));
        args.push("-o".to_owned());
        args.push(output_dir.join("exec").as_os_str().to_str().unwrap().to_owned());
        args.push("-e".to_owned());
        args.push("_start".to_owned());
        args.extend(self.get_system_search_dirs().expect("getting gcc search dirs").into_iter().map(|dir| format!("-L{}", dir)));
        args.push("-lc".to_owned());
        args.push("-lstdc++".to_owned());
        args.push("-lz".to_owned());
        args.push("-lrt".to_owned());
        args.push("-ldl".to_owned());
        args.push("-ltinfo".to_owned());
        args.push("-lpthread".to_owned());
        args.push("-lm".to_owned());
        args.push("-l:libunwind.a".to_owned());
        args.push(format!("-L{}", output_dir.as_os_str().to_str().unwrap().to_owned()));
        args.push("-l:librtdbg.a".to_owned());
        // args.push(format!("-L{}/lib", llvm_path));
        // for file in std::fs::read_dir(format!("{}/lib", llvm_path)).unwrap() {
        //     let file = file.unwrap();
        //     let name = file.file_name();
        //     let name = name.to_str().unwrap();
        //     if name.ends_with(".a") {
        //         if name == "libMLIRMlirOptMain.a"
        //             || name == "libFortran_main.a"
        //             || name == "libbolt_rt_instr_osx.a"
        //         {
        //             // libMLIRMlirOptMain and libFortran_main contain a `main` symbol and cannot be linked to
        //             // libbolt_rt_instr_osx causes weird errors
        //             continue;
        //         }
        //         args.push(format!("-l:{}", name));
        //     }
        // }
        // args.push(format!(
        //     "-L{}/lib/clang/15.0.5/lib/x86_64-unknown-linux-gnu",
        //     llvm_path
        // ));
        // args.push("-lclang_rt.builtins".to_owned());
        for file in std::fs::read_dir(&output_dir).unwrap() {
            let dir_name = file.unwrap().path().to_str().unwrap().to_owned();
            if !dir_name.ends_with(".o") {
                continue;
            }
            args.push(dir_name);
        }
        args.push(format!("{}/crtn.o", crt_dir));

        unsafe {
            println!("linker: ld.lld {}", args.join(" "));
            let args_slice: Vec<CString> = args.into_iter().map(|str| CString::new(str).expect("invalid arg")).collect();
            let args_slice: Vec<_> = args_slice.iter().map(|arg| arg.as_ptr()).collect();
            let len = args_slice.len() as u32;
            let args_ptr = args_slice.as_ptr();
            match target {
                ObjectFormat::Elf => LLD_LinkElf(args_ptr, len),
                // ObjectFormat::MachO => LLD_LinkMachO(args_ptr, len),
                // ObjectFormat::Coff => LLD_LinkCoff(args_ptr, len),
                // ObjectFormat::Wasm => LLD_LinkWasm(args_ptr, len),
            }
        }
    }
}
