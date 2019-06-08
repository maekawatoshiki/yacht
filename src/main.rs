extern crate yacht;
use yacht::{exec::jit, metadata::assembly};

use std::path::PathBuf;

extern crate clap;
use clap::{App, AppSettings, Arg};

extern crate ansi_term;
use ansi_term::Colour;

const VERSION_STR: &'static str = env!("CARGO_PKG_VERSION");

fn main() {
    let app = App::new("Yacht")
        .version(VERSION_STR)
        .author("uint256_t")
        .about("An ECMA-335 implementation written in Rust")
        .arg(Arg::with_name("file").help("Input file name").index(1))
        .setting(AppSettings::ArgRequiredElseHelp);
    let app_matches = app.clone().get_matches();

    let filename = match app_matches.value_of("file") {
        Some(filename) => PathBuf::from(filename),
        None => return,
    };

    #[rustfmt::skip]
    macro_rules! expect { ($expr:expr, $msg:expr) => {{ match $expr {
        Some(some) => some,
        None => { eprintln!("{}: {}", Colour::Red.bold().paint("error"), $msg); return }
    } }}; }

    let asm = expect!(
        assembly::Assembly::load(filename),
        "An error occurred while loading file"
    );
    let entry_method = expect!(
        asm.borrow_mut().image.get_entry_method(),
        "Entry method not found"
    );

    unsafe {
        let mut asm = asm.borrow_mut();
        let mut shared_env = jit::jit::SharedEnvironment::new();
        let mut jit = jit::jit::JITCompiler::new(&mut *asm, &mut shared_env);
        let main = jit.generate_method_as_main(&entry_method);
        jit.run_method(main);
    }
}

#[cfg(test)]
mod tests {
    use std::{fs, path::PathBuf};
    use yacht::{exec::jit, metadata::assembly};

    #[test]
    fn exec_examples() {
        let paths = fs::read_dir("./examples").unwrap();
        for entry in paths {
            let path = entry.unwrap().path();
            let filename = path.to_str().unwrap();
            if !filename.ends_with(".exe") || filename.ends_with("smallpt.exe") {
                continue;
            }
            let asm = assembly::Assembly::load(PathBuf::from(filename)).unwrap();
            let method = asm.borrow_mut().image.get_entry_method().unwrap();
            unsafe {
                let mut asm = asm.borrow_mut();
                let mut shared_env = jit::jit::SharedEnvironment::new();
                let mut jit = jit::jit::JITCompiler::new(&mut asm, &mut shared_env);
                let main = jit.generate_method_as_main(&method);
                jit.run_method(main);
            }
        }
    }
}
