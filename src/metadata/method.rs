use crate::{
    exec::instruction::Instruction,
    metadata::{class::*, signature::Type},
};
use std::{cell::RefCell, rc::Rc};

pub const TINY_FORMAT: u8 = 0x2;
pub const FAT_FORMAT: u8 = 0x3;

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum MethodHeaderType {
    TinyFormat {
        bytes: usize,
    },
    FatFormat {
        flags: u16,
        size: u8,
        max_stack: u16,
        code_size: u32,
        local_var_sig_tok: u32,
    },
}

pub type MethodInfoRef = Rc<RefCell<MethodInfo>>;

#[derive(Debug, Clone, PartialEq)]
pub enum MethodInfo {
    MDef(MethodDefInfo),
    MRef(MemberRefInfo),
}

#[derive(Debug, Clone, PartialEq)]
pub struct MethodDefInfo {
    pub rva: u32,
    pub impl_flags: u16,
    pub flags: u16,
    pub name: String,
    pub header_ty: MethodHeaderType,
    pub ty: Type,
    pub locals_ty: Vec<Type>,
    pub body: Vec<Instruction>,
    pub class: ClassInfoRef,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemberRefInfo {
    pub name: String,
    pub ty: Type,
    pub class: ClassInfoRef,
}

impl MethodInfo {
    pub fn into_mdef(self) -> MethodDefInfo {
        match self {
            MethodInfo::MDef(m) => m,
            MethodInfo::MRef(_) => panic!(),
        }
    }

    pub fn into_mref(self) -> MemberRefInfo {
        match self {
            MethodInfo::MRef(m) => m,
            MethodInfo::MDef(_) => panic!(),
        }
    }

    pub fn as_mdef(&self) -> &MethodDefInfo {
        match self {
            MethodInfo::MDef(ref m) => m,
            MethodInfo::MRef(_) => panic!(),
        }
    }

    pub fn as_mref(&self) -> &MemberRefInfo {
        match self {
            MethodInfo::MRef(ref m) => m,
            MethodInfo::MDef(_) => panic!(),
        }
    }

    pub fn get_name(&self) -> &str {
        match self {
            MethodInfo::MDef(ref m) => m.name.as_str(),
            MethodInfo::MRef(ref m) => m.name.as_str(),
        }
    }

    pub fn get_class(&self) -> &ClassInfoRef {
        match self {
            MethodInfo::MDef(ref m) => &m.class,
            MethodInfo::MRef(ref m) => &m.class,
        }
    }
}

impl MethodDefInfo {
    pub fn is_virtual(&self) -> bool {
        self.flags & method_attributes_flags::VIRTUAL > 0
    }

    pub fn is_new_slot(&self) -> bool {
        self.flags & method_attributes_flags::NEW_SLOT > 0
    }

    pub fn is_reuse_slot(&self) -> bool {
        self.flags & method_attributes_flags::NEW_SLOT == 0
    }
}

#[rustfmt::skip]
pub mod method_attributes_flags {
    // TODO: Implement all the flags
    pub const VIRTUAL : u16 = 0x0040;
    pub const NEW_SLOT: u16 = 0x0100;
}

// #[derive(Debug, Clone)]
// pub struct MemberRef {
//     name: String,
// }
