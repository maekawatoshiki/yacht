use crate::{
    exec::instruction::*,
    metadata::{metadata::*, method::MethodBodyRef, signature::*},
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
                Instruction::Ldstr { us_offset } => self.stack_push(Value::String(*us_offset)),
                Instruction::Ldc_I4_1 => self.stack_push(Value::Int32(1)),
                Instruction::Call { table, entry } => self.instr_call(image, *table, *entry),
                Instruction::Ret => break,
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

impl Interpreter {
    fn instr_call(&mut self, image: &mut Image, table: usize, entry: usize) {
        // TODO: Refacotr
        let table = &image.metadata.metadata_stream.tables[table][entry - 1];
        match table {
            Table::MemberRef(mrt) => {
                let (table, entry) = mrt.class_table_and_entry();
                let class = &image.metadata.metadata_stream.tables[table][entry - 1];
                match class {
                    Table::TypeRef(trt) => {
                        let (table, entry) = trt.resolution_scope_table_and_entry();
                        let art = match image.metadata.metadata_stream.tables[table][entry - 1] {
                            Table::AssemblyRef(art) => art,
                            _ => unimplemented!(),
                        };
                        let ar_name = image.get_string(art.name);
                        let ty_namespace = image.get_string(trt.type_namespace);
                        let ty_name = image.get_string(trt.type_name);
                        let name = image.get_string(mrt.name);
                        let sig = image.metadata.blob.get(&(mrt.signature as u32)).unwrap();
                        let ty = SignatureParser::new(sig).parse_method_ref_sig().unwrap();

                        dprintln!(" [{}]{}.{}::{}", ar_name, ty_namespace, ty_name, name);

                        dprintln!("Method type: {:?}", ty);

                        if ar_name == "mscorlib"
                            && ty_namespace == "System"
                            && ty_name == "Console"
                            && name == "WriteLine"
                        {
                            let val = self.stack_pop();
                            if ty.equal_method(ElementType::Void, &[ElementType::String]) {
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
                            } else if ty.equal_method(ElementType::Void, &[ElementType::I4]) {
                                println!("{}", val.as_int32().unwrap());
                            }
                        }
                    }
                    _ => unimplemented!(),
                }
            }
            Table::MethodDef(mdt) => {
                let reader = image.reader.as_mut().unwrap().clone();
                let method_ref = reader.borrow_mut().read_method(image, mdt.rva).unwrap();
                self.interpret(image, method_ref);
            }
            e => unimplemented!("call: unimplemented: {:?}", e),
        }
    }
}

impl Value {
    pub fn as_string(&self) -> Option<u32> {
        match self {
            Value::String(n) => Some(*n),
            _ => None,
        }
    }

    pub fn as_int32(&self) -> Option<i32> {
        match self {
            Value::Int32(n) => Some(*n),
            _ => None,
        }
    }
}
