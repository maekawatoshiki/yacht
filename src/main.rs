extern crate yacht;
use yacht::pe::reader;

extern crate clap;
use clap::{App, Arg};

const VERSION_STR: &'static str = env!("CARGO_PKG_VERSION");

fn main() {
    let app = App::new("Yacht")
        .version(VERSION_STR)
        .author("uint256_t")
        .about("An ECMA-335 Implementation written in Rust")
        .arg(Arg::with_name("file").help("Input file name").index(1));
    let app_matches = app.clone().get_matches();

    let filename = match app_matches.value_of("file") {
        Some(filename) => filename,
        None => return,
    };

    reader::PEFileReader::new(filename);
}
