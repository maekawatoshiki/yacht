use crate::metadata::{method::*, signature::*};
use std::{cell::RefCell, rc::Rc};

pub type ClassInfoRef = Rc<RefCell<ClassInfo>>;
pub type VTablePtr = *mut *mut ::std::ffi::c_void;

#[derive(Clone, PartialEq)]
pub struct ClassInfo {
    pub name: String,
    pub namespace: String,
    pub fields: Vec<ClassField>,
    pub methods: Vec<MethodInfoRef>,
    pub parent: Option<ClassInfoRef>,
    pub virtual_methods: Vec<MethodInfoRef>,
    pub vtable_ptr: VTablePtr,
}

#[derive(Clone, PartialEq, Debug)]
pub struct ClassField {
    pub name: String,
    pub ty: Type,
}

impl ::std::fmt::Debug for ClassInfo {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(
            f,
            "ClassInfo {{ name: {}, namespace: {}, fields: {:?}, methods: [omitted], parent: {:?}, vtable: [omitted] }}",
            self.name, self.namespace, self.fields, self.parent
        )
    }
}
