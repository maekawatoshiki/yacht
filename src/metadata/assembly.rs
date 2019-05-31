use super::{image::*, pe_parser::*};
use rustc_hash::FxHashMap;
use std::{cell::RefCell, rc::Rc};

pub type AssemblyRef = Rc<RefCell<Assembly>>;

#[derive(Debug, Clone)]
pub struct Assembly {
    pub name: String,
    pub image: Image,
}

impl Assembly {
    pub fn load(filename: &str) -> Option<AssemblyRef> {
        let mut pe_parser = PEParser::new(filename)?;
        let mut assembly = pe_parser.create_assembly()?;
        let asmref = Rc::new(RefCell::new(assembly));
        let mut loaded = FxHashMap::default();
        loaded.insert(asmref.borrow().name.clone(), asmref.clone());
        asmref.borrow_mut().image.pe_parser = Some(Rc::new(RefCell::new(pe_parser)));
        asmref.borrow_mut().image.setup_all_asmref(&mut loaded);
        asmref.borrow_mut().image.register_all_class();
        asmref.borrow_mut().image.setup_all_typeref();
        asmref.borrow_mut().image.setup_all_class();
        for (_, asm) in &loaded {
            let mut ok = asm.borrow_mut();
            ok.image.setup_all_typeref();
            ok.image.setup_all_class();
            if ok.name == "c" {
                println!("{:?}", ok.image.method_cache);
            }
        }
        println!("{:?}", loaded.len());
        Some(asmref)
    }

    pub fn load_exclusive(
        filename: &str,
        loaded: &mut FxHashMap<String, AssemblyRef>,
    ) -> Option<AssemblyRef> {
        let mut pe_parser = PEParser::new(filename)?;
        let mut assembly = pe_parser.create_assembly()?;
        let asmref = Rc::new(RefCell::new(assembly));
        loaded.insert(asmref.borrow().name.clone(), asmref.clone());
        asmref.borrow_mut().image.pe_parser = Some(Rc::new(RefCell::new(pe_parser)));
        asmref.borrow_mut().image.setup_all_asmref(loaded);
        asmref.borrow_mut().image.register_all_class();
        Some(asmref)
    }
}
