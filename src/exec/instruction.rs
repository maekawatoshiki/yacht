#[derive(Debug, Clone, Copy, PartialEq)]
#[allow(non_camel_case_types)]
pub enum Instruction {
    Ldstr { us_offset: u32 },
    Ldc_I4_1,
    Ldc_I4_S { n: i32 },
    Ldarg_0,
    Ldarg_1,
    Add,
    Call { table: usize, entry: usize },
    Ret,
}
