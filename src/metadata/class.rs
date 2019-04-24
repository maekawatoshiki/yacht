use crate::metadata::signature::*;

#[derive(Debug, Clone, PartialEq)]
pub struct ClassInfo {
    pub name: String,
    pub namespace: String,
    pub fields: Vec<ClassField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassField {
    pub name: String,
    pub ty: Type,
}
