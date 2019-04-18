use crate::exec::instruction::*;
// mdt.rva
use std::slice::Iter;

#[derive(Debug, Clone)]
pub struct BytesToInstructions<'a> {
    iter: Iter<'a, u8>,
}

impl<'a> BytesToInstructions<'a> {
    pub fn new(bytes: &'a [u8]) -> Self {
        Self { iter: bytes.iter() }
    }

    pub fn convert(&mut self) -> Option<Vec<Instruction>> {
        let mut iseq = vec![];

        while let Some(byte) = self.iter.next() {
            match byte {
                // ldstr
                0x72 => {
                    let token = self.read_u32()?;
                    assert!(token & 0xff000000 == 0x70000000);
                    let us_offset = token & 0x00ffffff;
                    iseq.push(Instruction::Ldstr { us_offset })
                }
                // call
                0x28 => {
                    let token = self.read_u32()?;
                    let table = token as usize >> (32 - 8);
                    let entry = token as usize & 0x00ff_ffff;
                    iseq.push(Instruction::Call { table, entry })
                }
                // ldc.i4.1
                0x17 => {
                    iseq.push(Instruction::Ldc_I4_1);
                }
                // ldc.i4.s
                0x1f => {
                    let n = self.read_u8()?;
                    iseq.push(Instruction::Ldc_I4_S { n: n as i32 })
                }
                // ldarg.0
                0x02 => iseq.push(Instruction::Ldarg_0),
                // ldarg.1
                0x03 => iseq.push(Instruction::Ldarg_1),
                // add
                0x58 => iseq.push(Instruction::Add),
                // ret
                0x2a => iseq.push(Instruction::Ret),
                _ => {}
            }
        }

        Some(iseq)
    }
}

impl<'a> BytesToInstructions<'a> {
    fn read_u8(&mut self) -> Option<u8> {
        let x = *self.iter.next()?;
        Some(x)
    }

    fn read_u32(&mut self) -> Option<u32> {
        let x = *self.iter.next()? as u32;
        let y = *self.iter.next()? as u32;
        let z = *self.iter.next()? as u32;
        let u = *self.iter.next()? as u32;
        Some((u << 24) + (z << 16) + (y << 8) + x)
    }
}
