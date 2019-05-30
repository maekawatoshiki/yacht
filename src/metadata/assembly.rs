use super::{image::*, pe_parser::*};
use std::{cell::RefCell, rc::Rc};

// pub type Assembly

#[derive(Debug, Clone)]
pub struct Assembly {
    pub name: String,
    pub image: Image,
}

impl Assembly {
    pub fn load(filename: &str) -> Option<Self> {
        let mut pe_parser = PEParser::new(filename)?;
        let mut assembly = pe_parser.create_assembly()?;
        assembly.image.pe_parser = Some(Rc::new(RefCell::new(pe_parser)));
        assembly.image.setup_all_asmref();
        assembly.image.setup_all_class();
        Some(assembly)
    }
}
