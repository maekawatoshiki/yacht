use crate::{exec::instruction::Instruction, metadata::signature::Type};
use std::{cell::RefCell, rc::Rc};

pub const TINY_FORMAT: u8 = 0x2;
pub const FAT_FORMAT: u8 = 0x3;

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum MethodHeaderType {
    TinyFormat { bytes: usize },
    FatFormat,
}

impl MethodHeaderType {
    pub fn check(n: u8) -> Option<MethodHeaderType> {
        match n & 0b00000011 {
            TINY_FORMAT => Some(MethodHeaderType::TinyFormat {
                bytes: n as usize >> 2,
            }),
            FAT_FORMAT => Some(MethodHeaderType::FatFormat),
            _ => None,
        }
    }
}

pub type MethodBodyRef = Rc<RefCell<MethodBody>>;

#[derive(Debug, Clone)]
pub struct MethodBody {
    pub header_ty: MethodHeaderType,
    pub ty: Option<Type>,
    pub body: Vec<Instruction>,
}

// #[derive(Debug, Clone)]
// pub struct MemberRef {
//     name: String,
// }
