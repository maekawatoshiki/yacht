use crate::exec::instruction::*;
use std::collections::BTreeMap;

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub code: Vec<Instruction>,
    pub start: usize,
    pub kind: BrKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BrKind {
    ConditionalJmp { destinations: Vec<usize> },
    UnconditionalJmp { destination: usize },
    ImplicitJmp { destination: usize },
    BlockStart,
}

#[derive(Debug, Clone)]
pub struct CFGMaker {}

impl CFGMaker {
    pub fn new() -> Self {
        CFGMaker {}
    }
}

impl CFGMaker {
    pub fn make_basic_blocks(&mut self, code: &[Instruction]) -> Vec<BasicBlock> {
        let mut map = BTreeMap::new();

        macro_rules! jmp_at {
            ($k:expr, $v:expr) => {{
                map.entry($k).or_insert_with(|| vec![]).push($v)
            }};
        }

        macro_rules! new_block_starts_at {
            ($k:expr) => {{
                map.entry($k)
                    .or_insert_with(|| vec![])
                    .push(BrKind::BlockStart)
            }};
        }

        for (pc, instr) in code.iter().enumerate() {
            match instr {
                Instruction::Bge(target)
                | Instruction::Bge_un(target)
                | Instruction::Bgt(target)
                | Instruction::Ble(target)
                | Instruction::Ble_un(target)
                | Instruction::Blt(target)
                | Instruction::Beq(target)
                | Instruction::Bne_un(target)
                | Instruction::Brfalse(target)
                | Instruction::Brtrue(target) => {
                    jmp_at!(
                        pc,
                        BrKind::ConditionalJmp {
                            destinations: vec![*target, pc + 1]
                        }
                    );
                    new_block_starts_at!(*target);
                    new_block_starts_at!(pc + 1);
                }
                Instruction::Br(target) => {
                    jmp_at!(
                        pc,
                        BrKind::UnconditionalJmp {
                            destination: *target,
                        }
                    );
                    new_block_starts_at!(*target);
                }
                _ => {}
            }
        }

        let mut start = Some(0);
        let mut blocks = vec![];

        macro_rules! create_block {
            ($range:expr, $kind:expr) => {{
                blocks.push(BasicBlock {
                    code: code[$range].to_vec(),
                    start: $range.start,
                    kind: $kind,
                });
            }};
        }

        for (key, kind_list) in map {
            for kind in kind_list {
                match kind {
                    BrKind::BlockStart => {
                        if let Some(start) = start {
                            if start < key {
                                create_block!(start..key, BrKind::ImplicitJmp { destination: key })
                            }
                        }
                        start = Some(key)
                    }
                    BrKind::ConditionalJmp { .. } | BrKind::UnconditionalJmp { .. } => {
                        if let Some(start) = start {
                            if start <= key {
                                create_block!(start..key + 1, kind)
                            }
                        }
                        start = None;
                    }
                    _ => {}
                }
            }
        }

        if let Some(start) = start {
            create_block!(start..code.len(), BrKind::BlockStart);
        }

        blocks
    }
}

impl BrKind {
    pub fn get_conditional_jump_destinations(&self) -> &Vec<usize> {
        match self {
            BrKind::ConditionalJmp { destinations } => destinations,
            _ => panic!(),
        }
    }

    pub fn get_unconditional_jump_destination(&self) -> usize {
        match self {
            BrKind::UnconditionalJmp { destination } => *destination,
            BrKind::ImplicitJmp { destination } => *destination,
            _ => panic!(),
        }
    }
}
