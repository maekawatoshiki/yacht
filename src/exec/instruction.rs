#[derive(Debug, Clone, Copy, PartialEq)]
#[allow(non_camel_case_types)]
pub enum Instruction {
    Ldstr { us_offset: u32 },
    Ldc_I4_1,
    Call { table: usize, entry: usize },
    Ret,
}
