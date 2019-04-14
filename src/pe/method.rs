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

#[derive(Debug, Clone)]
pub struct MethodBody {
    pub ty: MethodHeaderType,
    pub body: Vec<u8>,
}
