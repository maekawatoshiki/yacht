#[derive(Debug, Clone, Copy, PartialEq)]
#[allow(non_camel_case_types)]
pub enum Instruction {
    Ldstr { us_offset: u32 },
    Ldc_I4_0,
    Ldc_I4_1,
    Ldc_I4_2,
    Ldc_I4_S { n: i32 },
    Ldarg_0,
    Ldarg_1,
    Ldloc_0,
    Stloc_0,
    Pop,
    Bge { target: usize },
    Add,
    Sub,
    Call { table: usize, entry: usize },
    Ret,
}

#[rustfmt::skip]
pub mod il_instr {
    pub const LDSTR    : u8 = 0x72;
    pub const CALL     : u8 = 0x28;
    pub const LDC_I4_0 : u8 = 0x16;
    pub const LDC_I4_1 : u8 = 0x17;
    pub const LDC_I4_2 : u8 = 0x18;
    pub const LDC_I4_S : u8 = 0x1f;
    pub const LDARG_0  : u8 = 0x02;
    pub const LDARG_1  : u8 = 0x03;
    pub const LDLOC_0  : u8 = 0x06;
    pub const STLOC_0  : u8 = 0x0a;
    pub const POP      : u8 = 0x26;
    pub const BGE      : u8 = 0x3c;
    pub const ADD      : u8 = 0x58;
    pub const SUB      : u8 = 0x59;
    pub const RET      : u8 = 0x2A;

    pub fn get_instr_size(instr: u8) -> usize {
        match instr {
            LDSTR | CALL | BGE => 5,
            LDC_I4_0 | LDC_I4_1 | LDC_I4_2 | 
            LDARG_0 | LDARG_1 |
            LDLOC_0 |
            STLOC_0 |
            ADD | SUB | RET | POP => 1,
            LDC_I4_S => 2,
            e => panic!("Not an instruction: {}", e),
        }
    }
}
