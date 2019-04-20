#[derive(Debug, Clone, Copy, PartialEq)]
#[allow(non_camel_case_types)]
pub enum Instruction {
    Ldstr { us_offset: u32 },
    Ldc_I4_0,
    Ldc_I4_1,
    Ldc_I4_S { n: i32 },
    Ldarg_0,
    Ldarg_1,
    Pop,
    Bge { target: usize },
    Add,
    Call { table: usize, entry: usize },
    Ret,
}

#[rustfmt::skip]
pub mod il_instr {
    const LDSTR    : u8 = 0x72;
    const CALL     : u8 = 0x28;
    const LDC_I4_0 : u8 = 0x16;
    const LDC_I4_1 : u8 = 0x17;
    const LDC_I4_S : u8 = 0x1f;
    const LDARG_0  : u8 = 0x02;
    const LDARG_1  : u8 = 0x03;
    const POP      : u8 = 0x26;
    const BGE      : u8 = 0x3c;
    const ADD      : u8 = 0x58;
    const RET      : u8 = 0x2A;

    pub fn get_instr_size(instr: u8) -> usize {
        match instr {
            LDSTR | CALL | BGE => 5,
            LDC_I4_0 | LDC_I4_1 | LDARG_0 | LDARG_1
            | ADD | RET | POP => 1,
            LDC_I4_S => 2,
            e => panic!("Not an instruction: {}", e),
        }
    }
}
