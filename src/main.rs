extern crate yacht;
use yacht::{exec::jit, metadata::image};

extern crate clap;
use clap::{App, Arg};

extern crate ansi_term;
use ansi_term::Colour;

const VERSION_STR: &'static str = env!("CARGO_PKG_VERSION");

fn main() {
    let app = App::new("Yacht")
        .version(VERSION_STR)
        .author("uint256_t")
        .about("An ECMA-335 implementation written in Rust")
        .arg(Arg::with_name("file").help("Input file name").index(1));
    let app_matches = app.clone().get_matches();

    let filename = match app_matches.value_of("file") {
        Some(filename) => filename,
        None => return,
    };

    #[rustfmt::skip]
    macro_rules! expect { ($expr:expr, $msg:expr) => {{ match $expr {
        Some(some) => some,
        None => { eprintln!("{}: {}", Colour::Red.bold().paint("error"), $msg); return }
    } }}; }

    let mut image = expect!(
        image::Image::from_file(filename),
        "Error occurred while loading file"
    );
    let method = image.get_entry_method();

    unsafe {
        let mut jit = jit::jit::JITCompiler::new(&mut image);
        let main = jit.generate_main(&method);
        jit.run_main(main);
    }
}

#[cfg(test)]
mod tests {
    use std::fs;
    use yacht::{exec::jit, metadata::image};

    #[test]
    fn pe_file_reader() {
        let paths = fs::read_dir("./examples").unwrap();
        for entry in paths {
            let path = entry.unwrap().path();
            let filename = path.to_str().unwrap();
            if !filename.ends_with(".exe") || filename == "smallpt.exe" {
                continue;
            }
            let mut image = image::Image::from_file(filename).unwrap();
            let method = image.get_entry_method();
            unsafe {
                let mut jit = jit::jit::JITCompiler::new(&mut image);
                let main = jit.generate_main(&method);
                jit.run_main(main);
            }
        }
    }
}
