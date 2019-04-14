use crate::exec::instruction::*;
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
                    let entry = token as usize & 0x00ffffff;
                    iseq.push(Instruction::Call { table, entry })
                }
                // ret
                0x2a => iseq.push(Instruction::Ret),
                _ => {}
            }
        }

        Some(iseq)
    }
}

impl<'a> BytesToInstructions<'a> {
    fn read_u32(&mut self) -> Option<u32> {
        let x = *self.iter.next()? as u32;
        let y = *self.iter.next()? as u32;
        let z = *self.iter.next()? as u32;
        let u = *self.iter.next()? as u32;
        Some((u << 24) + (z << 16) + (y << 8) + x)
    }
}
