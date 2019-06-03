use super::{image::*, pe_parser::*};
use rustc_hash::FxHashMap;
use std::{cell::RefCell, path::PathBuf, rc::Rc};

pub type AssemblyRef = Rc<RefCell<Assembly>>;

#[derive(Debug, Clone)]
pub struct Assembly {
    pub name: String,
    pub image: Image,
}

impl Assembly {
    pub fn load(filename: PathBuf) -> Option<AssemblyRef> {
        let mut pe_parser = PEParser::new(filename)?;
        let asmref = Rc::new(RefCell::new(pe_parser.create_assembly()?));
        let mut loaded = FxHashMap::default();

        loaded.insert(asmref.borrow().name.clone(), asmref.clone());

        {
            let mut asm = asmref.borrow_mut();
            asm.image.pe_parser = Some(Rc::new(RefCell::new(pe_parser)));
            asm.image.setup_all_asmref(&mut loaded);
            asm.image.define_all_class();
            asm.image.setup_all_typeref();
            asm.image.setup_all_class();
        }

        for (_, asm) in &loaded {
            let mut ok = asm.borrow_mut();
            ok.image.setup_all_typeref();
            ok.image.setup_all_class();
        }

        Some(asmref)
    }

    pub fn load_exclusive(
        filename: PathBuf,
        loaded: &mut FxHashMap<String, AssemblyRef>,
    ) -> Option<AssemblyRef> {
        let mut pe_parser = PEParser::new(filename)?;
        let asmref = Rc::new(RefCell::new(pe_parser.create_assembly()?));

        loaded.insert(asmref.borrow().name.clone(), asmref.clone());

        {
            let mut asm = asmref.borrow_mut();
            asm.image.pe_parser = Some(Rc::new(RefCell::new(pe_parser)));
            asm.image.setup_all_asmref(loaded);
            asm.image.define_all_class();
        }

        Some(asmref)
    }
}
