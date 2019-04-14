#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Instruction {
    Ldstr { us_offset: u32 },
    Call { table: usize, entry: usize },
    Ret,
}
