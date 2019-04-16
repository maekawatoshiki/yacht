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
        dprintln!("Interpreter starts");
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
                            dprint!("Inst: call:");
                            let (table, entry) = mrt.class_table_and_entry();
                            let class = &image.metadata.metadata_stream.tables[table][entry - 1];
                            match class {
                                Table::TypeRef(trt) => {
                                    let (table, entry) = trt.resolution_scope_table_and_entry();
                                    let art = match image.metadata.metadata_stream.tables[table]
                                        [entry - 1]
                                    {
                                        Table::AssemblyRef(art) => art,
                                        _ => unimplemented!(),
                                    };
                                    let ar_name = image.get_string(art.name);
                                    let ty_namespace = image.get_string(trt.type_namespace);
                                    let ty_name = image.get_string(trt.type_name);
                                    let name = image.get_string(mrt.name);

                                    dprintln!(
                                        " [{}]{}.{}::{}",
                                        ar_name,
                                        ty_namespace,
                                        ty_name,
                                        name
                                    );

                                    if ar_name == "mscorlib"
                                        && ty_namespace == "System"
                                        && ty_name == "Console"
                                        && name == "WriteLine"
                                    {
                                        let val = self.stack_pop();
                                        println!(
                                            "{}",
                                            String::from_utf16_lossy(
                                                image
                                                    .metadata
                                                    .user_strings
                                                    .get(&val.as_string().unwrap())
                                                    .unwrap()
                                            )
                                        );
                                    }
                                }
                                _ => unimplemented!(),
                            }
                        }
                        e => unimplemented!("call: unimplemented: {:?}", e),
                    }
                }
                _ => {}
            }
            dprintln!("stack: {:?}", &self.stack[0..8]);
        }
    }

    #[inline]
    pub fn stack_push(&mut self, v: Value) {
        self.stack[self.base_ptr + self.stack_ptr] = v;
        self.stack_ptr += 1;
    }

    #[inline]
    pub fn stack_pop(&mut self) -> Value {
        self.stack_ptr -= 1;
        self.stack[self.base_ptr + self.stack_ptr]
    }
}

impl Value {
    pub fn as_string(&self) -> Option<u32> {
        match self {
            Value::String(n) => Some(*n),
            _ => None,
        }
    }
}
