use crate::{
    exec::instruction::*,
    pe::{metadata::*, method::MethodBodyRef},
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
                    // TODO: Refacotr
                    let table = &image.metadata.metadata_stream.tables[*table][*entry - 1];
                    match table {
                        Table::MemberRef(mrt) => {
                            println!("call:");
                            let name = image.metadata.strings.get(&(mrt.name as u32)).unwrap();
                            let (table, entry) = mrt.class_table_and_entry();
                            let class = &image.metadata.metadata_stream.tables[table][entry - 1];
                            match class {
                                Table::TypeRef(trt) => {
                                    let (table, entry) = trt.resolution_scope_table_and_entry();
                                    println!(
                                        "  resolution:\n    \
                                         {:?}\n    \
                                         {:?}\n    \
                                         {:?}",
                                        image.metadata.metadata_stream.tables[table][entry - 1],
                                        image
                                            .metadata
                                            .strings
                                            .get(&(trt.type_name as u32))
                                            .unwrap(),
                                        image
                                            .metadata
                                            .strings
                                            .get(&(trt.type_namespace as u32))
                                            .unwrap(),
                                    );
                                }
                                _ => unimplemented!(),
                            }
                            println!("  name: {:?}, class: {:?}", name, class);
                        }
                        _ => unimplemented!(),
                    }
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
