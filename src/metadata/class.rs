use crate::metadata::{method::*, signature::*};
use std::{cell::RefCell, fmt::Debug, rc::Rc};

pub type ClassInfoRef = Rc<RefCell<ClassInfo>>;
pub type VTablePtr = *mut *mut ::std::ffi::c_void;
pub type MethodTablePtr = *mut *mut ::std::ffi::c_void;

#[derive(Clone, PartialEq)]
pub struct ClassInfo {
    pub name: String,
    pub namespace: String,
    pub fields: Vec<ClassField>,
    pub methods: Vec<MethodInfoRef>,
    pub parent: Option<ClassInfoRef>,
    pub method_table: Vec<MethodInfoRef>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct ClassField {
    pub name: String,
    pub ty: Type,
}

impl ClassInfo {
    pub fn new(
        namespace: &str,
        name: &str,
        fields: Vec<ClassField>,
        methods: Vec<MethodInfoRef>,
        parent: Option<ClassInfoRef>,
    ) -> Self {
        Self {
            name: name.to_string(),
            namespace: namespace.to_string(),
            fields,
            methods,
            parent,
            method_table: vec![],
        }
    }

    pub fn new_ref(
        namespace: &str,
        name: &str,
        fields: Vec<ClassField>,
        methods: Vec<MethodInfoRef>,
        parent: Option<ClassInfoRef>,
    ) -> ClassInfoRef {
        Rc::new(RefCell::new(Self {
            name: name.to_string(),
            namespace: namespace.to_string(),
            fields,
            methods,
            parent,
            method_table: vec![],
        }))
    }
}

impl Debug for ClassInfo {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(
            f,
            "ClassInfo {{ name: {}, namespace: {}, fields: {:?}, methods: [omitted], parent: {:?}, vtable: [omitted] }}",
            self.name, self.namespace, self.fields, self.parent
        )
    }
}
