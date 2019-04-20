use crate::exec::instruction::*;
// mdt.rva
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
                // ldc.i4.0
                0x16 => iseq.push(Instruction::Ldc_I4_0),
                // ldc.i4.1
                0x17 => iseq.push(Instruction::Ldc_I4_1),
                // ldc.i4.2
                0x18 => iseq.push(Instruction::Ldc_I4_2),
                // ldc.i4.s
                0x1f => {
                    let n = self.read_u8()?;
                    iseq.push(Instruction::Ldc_I4_S { n: n as i32 })
                }
                // ldarg.0
                0x02 => iseq.push(Instruction::Ldarg_0),
                // ldarg.1
                0x03 => iseq.push(Instruction::Ldarg_1),
                // pop
                0x26 => iseq.push(Instruction::Pop),
                // bge
                0x3c => {
                    let target = self.read_u32()? as i32;
                    iseq.push(Instruction::Bge {
                        target: *self.target_map.get(&(i as i32 + 1 + 4 + target)).unwrap(),
                    })
                }
                // add
                0x58 => iseq.push(Instruction::Add),
                // sub
                il_instr::SUB => iseq.push(Instruction::Sub),
                // ret
                0x2a => iseq.push(Instruction::Ret),
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
            for _ in 0..il_instr::get_instr_size(*byte) - 1 {
                iter.next();
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
