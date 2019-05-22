use crate::{exec::instruction::*, metadata::token::*};
use rustc_hash::FxHashMap;
use std::{iter::Enumerate, slice::Iter};

#[derive(Debug, Clone)]
pub struct BytesToInstructions<'a> {
    iter: Enumerate<Iter<'a, u8>>,
    target_map: FxHashMap<i32, usize>,
}

impl<'a> BytesToInstructions<'a> {
    pub fn new(bytes: &'a [u8]) -> Self {
        Self {
            iter: bytes.iter().enumerate(),
            target_map: FxHashMap::default(),
        }
    }

    pub fn convert(&mut self) -> Option<Vec<Instruction>> {
        let mut iseq = vec![];

        self.make_target_map();

        while let Some((i, byte)) = self.iter.next() {
            match *byte {
                il_instr::LDNULL => iseq.push(Instruction::Ldnull),
                il_instr::LDSTR => {
                    let token = self.read_u32()?;
                    assert!(token & 0xff000000 == 0x70000000);
                    let us_offset = token & 0x00ffffff;
                    iseq.push(Instruction::Ldstr(us_offset))
                }
                il_instr::CALL => iseq.push(Instruction::Call(Token(self.read_u32()?))),
                il_instr::CALLVIRT => iseq.push(Instruction::CallVirt(Token(self.read_u32()?))),
                il_instr::BOX => iseq.push(Instruction::Box(Token(self.read_u32()?))),
                il_instr::NEWOBJ => iseq.push(Instruction::Newobj(Token(self.read_u32()?))),
                il_instr::NEWARR => iseq.push(Instruction::Newarr(Token(self.read_u32()?))),
                il_instr::LDC_I4_0 => iseq.push(Instruction::Ldc_I4_0),
                il_instr::LDC_I4_1 => iseq.push(Instruction::Ldc_I4_1),
                il_instr::LDC_I4_2 => iseq.push(Instruction::Ldc_I4_2),
                il_instr::LDC_I4_3 => iseq.push(Instruction::Ldc_I4_3),
                il_instr::LDC_I4_4 => iseq.push(Instruction::Ldc_I4_4),
                il_instr::LDC_I4_5 => iseq.push(Instruction::Ldc_I4_5),
                il_instr::LDC_I4_6 => iseq.push(Instruction::Ldc_I4_6),
                il_instr::LDC_I4_7 => iseq.push(Instruction::Ldc_I4_7),
                il_instr::LDC_I4_8 => iseq.push(Instruction::Ldc_I4_8),
                il_instr::LDC_I4_S => iseq.push(Instruction::Ldc_I4_S(self.read_u8()? as i32)),
                il_instr::LDC_I4 => iseq.push(Instruction::Ldc_I4(self.read_u32()? as i32)),
                il_instr::LDARG_0 => iseq.push(Instruction::Ldarg_0),
                il_instr::LDARG_1 => iseq.push(Instruction::Ldarg_1),
                il_instr::LDARG_2 => iseq.push(Instruction::Ldarg_2),
                il_instr::LDARG_3 => iseq.push(Instruction::Ldarg_3),
                il_instr::LDLOC_0 => iseq.push(Instruction::Ldloc_0),
                il_instr::LDLOC_1 => iseq.push(Instruction::Ldloc_1),
                il_instr::LDLOC_2 => iseq.push(Instruction::Ldloc_2),
                il_instr::LDLOC_3 => iseq.push(Instruction::Ldloc_3),
                il_instr::LDLOC_S => iseq.push(Instruction::Ldloc_S(self.read_u8()?)),
                il_instr::LDFLD => iseq.push(Instruction::Ldfld(Token(self.read_u32()?))),
                il_instr::LDELEM_U1 => iseq.push(Instruction::Ldelem_U1),
                il_instr::LDELEM_I1 => iseq.push(Instruction::Ldelem_I1),
                il_instr::LDELEM_I4 => iseq.push(Instruction::Ldelem_I4),
                il_instr::STLOC_0 => iseq.push(Instruction::Stloc_0),
                il_instr::STLOC_1 => iseq.push(Instruction::Stloc_1),
                il_instr::STLOC_2 => iseq.push(Instruction::Stloc_2),
                il_instr::STLOC_3 => iseq.push(Instruction::Stloc_3),
                il_instr::STLOC_S => iseq.push(Instruction::Stloc_S(self.read_u8()?)),
                il_instr::STFLD => iseq.push(Instruction::Stfld(Token(self.read_u32()?))),
                il_instr::STELEM_I1 => iseq.push(Instruction::Stelem_I1),
                il_instr::STELEM_I4 => iseq.push(Instruction::Stelem_I4),
                il_instr::LDLEN => iseq.push(Instruction::Ldlen),
                il_instr::CONV_I4 => iseq.push(Instruction::Conv_I4),
                il_instr::POP => iseq.push(Instruction::Pop),
                il_instr::DUP => iseq.push(Instruction::Dup),
                il_instr::BGE => iseq.push(Instruction::Bge({
                    let target = self.read_u32()? as i32;
                    *self.target_map.get(&(i as i32 + 1 + 4 + target)).unwrap()
                })),
                il_instr::BGT => iseq.push(Instruction::Bgt({
                    let target = self.read_u32()? as i32;
                    *self.target_map.get(&(i as i32 + 1 + 4 + target)).unwrap()
                })),
                il_instr::BLT => iseq.push(Instruction::Blt({
                    let target = self.read_u32()? as i32;
                    *self.target_map.get(&(i as i32 + 1 + 4 + target)).unwrap()
                })),
                il_instr::BLE => iseq.push(Instruction::Ble({
                    let target = self.read_u32()? as i32;
                    *self.target_map.get(&(i as i32 + 1 + 4 + target)).unwrap()
                })),
                il_instr::BEQ => iseq.push(Instruction::Beq({
                    let target = self.read_u32()? as i32;
                    *self.target_map.get(&(i as i32 + 1 + 4 + target)).unwrap()
                })),
                il_instr::BNE_UN => iseq.push(Instruction::Bne_un({
                    let target = self.read_u32()? as i32;
                    *self.target_map.get(&(i as i32 + 1 + 4 + target)).unwrap()
                })),
                il_instr::BRFALSE => iseq.push(Instruction::Brfalse({
                    let target = self.read_u32()? as i32;
                    *self.target_map.get(&(i as i32 + 1 + 4 + target)).unwrap()
                })),
                il_instr::BRTRUE => iseq.push(Instruction::Brtrue({
                    let target = self.read_u32()? as i32;
                    *self.target_map.get(&(i as i32 + 1 + 4 + target)).unwrap()
                })),
                il_instr::BR => iseq.push(Instruction::Br({
                    let target = self.read_u32()? as i32;
                    *self.target_map.get(&(i as i32 + 1 + 4 + target)).unwrap()
                })),
                0xfe => match self.iter.next()?.1 {
                    &il_instr::CLT => iseq.push(Instruction::Clt),
                    &il_instr::CEQ => iseq.push(Instruction::Ceq),
                    _ => unimplemented!(),
                },
                il_instr::ADD => iseq.push(Instruction::Add),
                il_instr::SUB => iseq.push(Instruction::Sub),
                il_instr::MUL => iseq.push(Instruction::Mul),
                il_instr::DIV => iseq.push(Instruction::Div),
                il_instr::REM => iseq.push(Instruction::Rem),
                il_instr::RET => iseq.push(Instruction::Ret),
                e => unimplemented!("{:?}", e),
            }
        }

        Some(iseq)
    }

    fn make_target_map(&mut self) {
        let mut iter = self.iter.clone();
        let mut iseq_size = 0;
        while let Some((i, byte)) = iter.next() {
            self.target_map.insert(i as i32, iseq_size);
            if *byte == 0xfe {
                // 2 bytes instruction
                let byte = iter.next().unwrap().1;
                for _ in 0..il_instr::get_instr2_size(*byte) - 2 {
                    iter.next();
                }
            } else {
                for _ in 0..il_instr::get_instr_size(*byte) - 1 {
                    iter.next();
                }
            }
            iseq_size += 1;
        }
    }
}

impl<'a> BytesToInstructions<'a> {
    fn read_u8(&mut self) -> Option<u8> {
        let x = *self.iter.next()?.1;
        Some(x)
    }

    fn read_u32(&mut self) -> Option<u32> {
        let x = *self.iter.next()?.1 as u32;
        let y = *self.iter.next()?.1 as u32;
        let z = *self.iter.next()?.1 as u32;
        let u = *self.iter.next()?.1 as u32;
        Some((u << 24) + (z << 16) + (y << 8) + x)
    }
}
