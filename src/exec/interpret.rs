use crate::{
    exec::instruction::*,
    pe::{metadata::Image, method::MethodBodyRef},
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Int32(i32),
    String(u32),
}

#[derive(Clone)]
pub struct Interpreter {
    stack: [Value; 1024],
    base_ptr: usize,
    stack_ptr: usize,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            stack: [Value::Int32(0); 1024],
            base_ptr: 0,
            stack_ptr: 0,
        }
    }

    pub fn interpret(&mut self, image: &mut Image, method: MethodBodyRef) {
        let iseq = &method.borrow().body;

        for instr in iseq {
            match instr {
                Instruction::Ldstr { us_offset } => {
                    self.stack_push(Value::String(*us_offset));
                }
                Instruction::Call { table, entry } => {
                    let table = &image.metadata.metadata_stream.tables[*table][*entry - 1];
                    println!("call table {:?}", table);
                }
                _ => {}
            }
            println!("{:?}", &self.stack[0..8]);
        }
    }

    #[inline]
    pub fn stack_push(&mut self, v: Value) {
        self.stack[self.base_ptr + self.stack_ptr] = v;
        self.stack_ptr += 1;
    }
}
