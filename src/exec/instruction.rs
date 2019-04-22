#[derive(Debug, Clone, Copy, PartialEq)]
#[allow(non_camel_case_types)]
pub enum Instruction {
    Ldstr { us_offset: u32 },
    Ldc_I4_0,
    Ldc_I4_1,
    Ldc_I4_2,
    Ldc_I4_3,
    Ldc_I4_S { n: i32 },
    Ldc_I4 { n: i32 },
    Ldarg_0,
    Ldarg_1,
    Ldloc_0,
    Stloc_0,
    Pop,
    Bne_un { target: usize },
    Bge { target: usize },
    Bgt { target: usize },
    Blt { target: usize },
    Ble { target: usize },
    Brfalse { target: usize },
    Brtrue { target: usize },
    Br { target: usize },
    Add,
    Sub,
    Mul,
    Rem,
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
    pub const LDC_I4_3 : u8 = 0x19;
    pub const LDC_I4_S : u8 = 0x1f;
    pub const LDC_I4   : u8 = 0x20;
    pub const LDARG_0  : u8 = 0x02;
    pub const LDARG_1  : u8 = 0x03;
    pub const LDLOC_0  : u8 = 0x06;
    pub const STLOC_0  : u8 = 0x0a;
    pub const POP      : u8 = 0x26;
    pub const BR       : u8 = 0x38;
    pub const BGE      : u8 = 0x3c;
    pub const BGT      : u8 = 0x3d;
    pub const BLE      : u8 = 0x3e;
    pub const BLT      : u8 = 0x3f;
    pub const BNE_UN   : u8 = 0x40;
    pub const BRFALSE  : u8 = 0x39;
    pub const BRTRUE   : u8 = 0x3a;
    pub const ADD      : u8 = 0x58;
    pub const SUB      : u8 = 0x59;
    pub const MUL      : u8 = 0x5a;
    pub const REM      : u8 = 0x5d;
    pub const RET      : u8 = 0x2a;

    pub fn get_instr_size(instr: u8) -> usize {
        match instr {
            LDSTR | CALL | 
            BGE | BR | BLT | BNE_UN | BRFALSE | BGT
             | BRTRUE | BLE |
            LDC_I4 => 5, 
            LDC_I4_0 | LDC_I4_1 | LDC_I4_2 | LDC_I4_3 |
            LDARG_0 | LDARG_1 |
            LDLOC_0 |
            STLOC_0 |
            ADD | SUB | MUL | REM |
            RET | POP => 1,
            LDC_I4_S => 2,
            e => panic!("Not an instruction: {}", e),
        }
    }
}
