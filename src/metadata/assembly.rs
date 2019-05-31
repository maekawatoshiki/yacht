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

        asmref.borrow_mut().image.pe_parser = Some(Rc::new(RefCell::new(pe_parser)));
        asmref.borrow_mut().image.setup_all_asmref(&mut loaded);
        asmref.borrow_mut().image.define_all_class();
        asmref.borrow_mut().image.setup_all_typeref();
        asmref.borrow_mut().image.setup_all_class();

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

        asmref.borrow_mut().image.pe_parser = Some(Rc::new(RefCell::new(pe_parser)));
        asmref.borrow_mut().image.setup_all_asmref(loaded);
        asmref.borrow_mut().image.define_all_class();

        Some(asmref)
    }
}
