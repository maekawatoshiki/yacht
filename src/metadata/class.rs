use crate::metadata::{method::*, signature::*};
use std::{cell::RefCell, fmt, rc::Rc};

pub type ClassInfoRef = Rc<RefCell<ClassInfo>>;

#[derive(Clone)]
pub struct ClassInfo {
    pub resolution_scope: ResolutionScope,
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

#[derive(Clone, PartialEq, Debug)]
pub enum ResolutionScope {
    AssemblyRef { name: String },
    None, // TODO
}

impl PartialEq for ClassInfo {
    fn eq(&self, other: &Self) -> bool {
        self.resolution_scope == other.resolution_scope
            && self.name == other.name
            && self.namespace == other.namespace
    }
}

impl ClassInfo {
    pub fn new(
        resolution_scope: ResolutionScope,
        namespace: &str,
        name: &str,
        fields: Vec<ClassField>,
        methods: Vec<MethodInfoRef>,
        parent: Option<ClassInfoRef>,
    ) -> Self {
        Self {
            resolution_scope,
            name: name.to_string(),
            namespace: namespace.to_string(),
            fields,
            methods,
            parent,
            method_table: vec![],
        }
    }

    pub fn new_ref(
        resolution_scope: ResolutionScope,
        namespace: &str,
        name: &str,
        fields: Vec<ClassField>,
        methods: Vec<MethodInfoRef>,
        parent: Option<ClassInfoRef>,
    ) -> ClassInfoRef {
        Rc::new(RefCell::new(Self {
            resolution_scope,
            name: name.to_string(),
            namespace: namespace.to_string(),
            fields,
            methods,
            parent,
            method_table: vec![],
        }))
    }

    pub fn new_ref_empty() -> ClassInfoRef {
        Rc::new(RefCell::new(Self {
            resolution_scope: ResolutionScope::None,
            name: "".to_string(),
            namespace: "".to_string(),
            fields: vec![],
            methods: vec![],
            parent: None,
            method_table: vec![],
        }))
    }

    pub fn get_method_index(&self, name: &str) -> Option<usize> {
        self.method_table
            .iter()
            .position(|m| m.borrow().get_name() == name)
    }

    pub fn get_field_index(&self, name: &str) -> Option<usize> {
        self.fields.iter().position(|f| f.name == name)
    }

    pub fn is_enum(&self) -> bool {
        match self.parent {
            Some(ref parent) => {
                let parent = parent.borrow();
                (match parent.resolution_scope {
                    ResolutionScope::AssemblyRef { ref name } if name == "mscorlib" => true,
                    _ => false,
                }) && parent.namespace == "System"
                    && parent.name == "Enum"
            }
            None => false,
        }
    }
}

impl ClassField {
    pub fn new(name: String, ty: Type) -> Self {
        ClassField { name, ty }
    }

    pub fn new_ty(ty: Type) -> Self {
        ClassField {
            name: "".to_string(),
            ty,
        }
    }
}

impl ResolutionScope {
    pub fn asm_ref(name: &str) -> Self {
        ResolutionScope::AssemblyRef {
            name: name.to_string(),
        }
    }

    pub fn get_name<'a>(&'a self) -> &'a str {
        match self {
            ResolutionScope::AssemblyRef { name } => name.as_str(),
            ResolutionScope::None => "",
        }
    }
}

impl fmt::Debug for ClassInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "ClassInfo {{ resolution_scope: {:?}, name: {}, namespace: {}, fields: {:?}, methods: [omitted], parent: {:?}, vtable: [omitted] }}",
            self.resolution_scope, self.name, self.namespace, self.fields, self.parent
        )
    }
}
