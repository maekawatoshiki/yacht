use crate::{exec::instruction::Instruction, metadata::signature::Type};
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

impl MethodHeaderType {
    pub fn max_stack(&self) -> usize {
        match self {
            MethodHeaderType::TinyFormat { .. } => 8,
            MethodHeaderType::FatFormat { max_stack, .. } => *max_stack as usize,
        }
    }
}

pub type MethodBodyRef = Rc<RefCell<MethodBody>>;

#[derive(Debug, Clone)]
pub struct MethodBody {
    pub header_ty: MethodHeaderType,
    pub ty: Option<Type>,
    pub locals_ty: Vec<Type>,
    pub body: Vec<Instruction>,
}

// #[derive(Debug, Clone)]
// pub struct MemberRef {
//     name: String,
// }
