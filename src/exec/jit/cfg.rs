use crate::exec::instruction::*;
use std::collections::BTreeMap;

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub code: Vec<Instruction>,
    pub start: usize,
    pub kind: BrKind,
    pub generated: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BrKind {
    ConditionalJmp { destinations: Vec<usize> },
    UnconditionalJmp { destination: usize },
    JmpRequired { destination: usize },
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

        for (pc, instr) in code.iter().enumerate() {
            match instr {
                Instruction::Bge { target }
                | Instruction::Bgt { target }
                | Instruction::Ble { target }
                | Instruction::Blt { target }
                | Instruction::Beq { target }
                | Instruction::Bne_un { target }
                | Instruction::Brfalse { target }
                | Instruction::Brtrue { target } => {
                    map.entry(pc)
                        .or_insert_with(|| vec![])
                        .push(BrKind::ConditionalJmp {
                            destinations: vec![*target, pc + 1],
                        });
                    map.entry(*target)
                        .or_insert_with(|| vec![])
                        .push(BrKind::BlockStart);
                    map.entry(pc + 1)
                        .or_insert_with(|| vec![])
                        .push(BrKind::BlockStart);
                }
                Instruction::Br { target } => {
                    map.entry(pc)
                        .or_insert_with(|| vec![])
                        .push(BrKind::UnconditionalJmp {
                            destination: *target,
                        });
                    map.entry(*target)
                        .or_insert_with(|| vec![])
                        .push(BrKind::BlockStart);
                }
                _ => {}
            }
        }

        let mut start = Some(0);
        let mut blocks = vec![];

        for (key, kind_list) in map {
            for kind in kind_list {
                match kind {
                    BrKind::BlockStart => {
                        if start.is_some() && start.unwrap() < key {
                            blocks.push(BasicBlock {
                                code: code[start.unwrap()..key].to_vec(),
                                start: start.unwrap(),
                                kind: BrKind::JmpRequired { destination: key },
                                generated: false,
                            });
                        }
                        start = Some(key);
                    }
                    BrKind::ConditionalJmp { .. } | BrKind::UnconditionalJmp { .. }
                        if start.is_some() && start.unwrap() <= key =>
                    {
                        blocks.push(BasicBlock {
                            code: code[start.unwrap()..key + 1].to_vec(),
                            start: start.unwrap(),
                            kind,
                            generated: false,
                        });
                        start = None;
                    }
                    _ => {}
                }
            }
        }

        if start.is_some() && start.unwrap() < code.len() {
            blocks.push(BasicBlock {
                code: code[start.unwrap()..code.len()].to_vec(),
                start: start.unwrap(),
                kind: BrKind::BlockStart,
                generated: false,
            });
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
            BrKind::JmpRequired { destination } => *destination,
            _ => panic!(),
        }
    }
}

impl BasicBlock {
    pub fn code_end_position(&self) -> usize {
        self.start + self.code.len()
    }
}
