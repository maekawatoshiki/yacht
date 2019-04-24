use crate::{
    exec::instruction::*,
    metadata::{metadata::*, method::*, signature::*},
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
    program_counter: usize,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            stack: [Value::Int32(0); 1024],
            base_ptr: 0,
            stack_ptr: 0,
            program_counter: 0,
        }
    }

    pub fn interpret(&mut self, image: &mut Image, method: &MethodBody, arguments: &[Value]) {
        macro_rules! numeric_op {
            ($op:ident) => {{
                let y = self.stack_pop();
                let x = self.stack_pop();
                self.stack_push(x.$op(y))
            }};
        }

        let (iseq, mut locals) = {
            (
                &method.body,
                vec![Value::Int32(0); method.header_ty.max_stack()],
            )
        };

        loop {
            let instr = &iseq[self.program_counter];
            match instr {
                Instruction::Ldstr { us_offset } => self.stack_push(Value::String(*us_offset)),
                Instruction::Ldc_I4_0 => self.stack_push(Value::Int32(0)),
                Instruction::Ldc_I4_1 => self.stack_push(Value::Int32(1)),
                Instruction::Ldc_I4_2 => self.stack_push(Value::Int32(2)),
                Instruction::Ldc_I4_3 => self.stack_push(Value::Int32(3)),
                Instruction::Ldc_I4_S { n } => self.stack_push(Value::Int32(*n)),
                Instruction::Ldc_I4 { n } => self.stack_push(Value::Int32(*n)),
                Instruction::Ldarg_0 => self.stack_push(arguments[0]),
                Instruction::Ldarg_1 => self.stack_push(arguments[1]),
                Instruction::Ldloc_0 => self.stack_push(locals[0]),
                Instruction::Stloc_0 => locals[0] = self.stack_pop(),
                Instruction::Pop => self.stack_ptr -= 1,
                Instruction::Bge { target } => self.instr_bge(image, *target),
                Instruction::Bgt { target } => self.instr_bgt(image, *target),
                Instruction::Blt { target } => self.instr_blt(image, *target),
                Instruction::Ble { target } => self.instr_ble(image, *target),
                Instruction::Bne_un { target } => self.instr_bne_un(image, *target),
                Instruction::Brtrue { target } => self.instr_brtrue(image, *target),
                Instruction::Brfalse { target } => self.instr_brfalse(image, *target),
                Instruction::Br { target } => self.program_counter = target - 1,
                Instruction::Add => numeric_op!(add),
                Instruction::Sub => numeric_op!(sub),
                Instruction::Mul => numeric_op!(mul),
                Instruction::Rem => numeric_op!(rem),
                Instruction::Call { table, entry } => self.instr_call(image, *table, *entry),
                Instruction::Newobj { table, entry } => self.instr_newobj(image, *table, *entry),
                Instruction::Ret => break,
            }
            self.program_counter += 1;
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

    #[inline]
    pub fn stack_pop_last_elements(&mut self, n: usize) -> Vec<Value> {
        self.stack_ptr -= n;
        self.stack[(self.base_ptr + self.stack_ptr)..].to_vec()
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
                        let ty = SignatureParser::new(sig)
                            .parse_method_ref_sig(image)
                            .unwrap();

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
                let saved_program_counter = self.program_counter;
                self.program_counter = 0;

                let method = image.get_method(mdt.rva);
                let params = {
                    let method_sig = method.ty.as_fnptr().unwrap();
                    self.stack_pop_last_elements(method_sig.params.len() as usize)
                };

                self.interpret(image, &method, &params);

                self.program_counter = saved_program_counter;
            }
            e => unimplemented!("call: unimplemented: {:?}", e),
        }
    }

    fn instr_newobj(&mut self, image: &mut Image, table: usize, entry: usize) {
        self.stack_push(Value::Int32(0))
    }

    fn instr_bge(&mut self, _image: &mut Image, target: usize) {
        let val2 = self.stack_pop();
        let val1 = self.stack_pop();
        if val1.ge(val2) {
            self.program_counter = target /* interpret() everytime increments pc */- 1
        }
    }

    fn instr_bgt(&mut self, _image: &mut Image, target: usize) {
        let val2 = self.stack_pop();
        let val1 = self.stack_pop();
        if val1.gt(val2) {
            self.program_counter = target /* interpret() everytime increments pc */- 1
        }
    }

    fn instr_ble(&mut self, _image: &mut Image, target: usize) {
        let val2 = self.stack_pop();
        let val1 = self.stack_pop();
        if val1.le(val2) {
            self.program_counter = target /* interpret() everytime increments pc */- 1
        }
    }

    fn instr_blt(&mut self, _image: &mut Image, target: usize) {
        let val2 = self.stack_pop();
        let val1 = self.stack_pop();
        if val1.lt(val2) {
            self.program_counter = target /* interpret() everytime increments pc */- 1
        }
    }

    fn instr_bne_un(&mut self, _image: &mut Image, target: usize) {
        let val2 = self.stack_pop();
        let val1 = self.stack_pop();
        if val1.ne(val2) {
            self.program_counter = target /* interpret() everytime increments pc */- 1
        }
    }

    fn instr_brtrue(&mut self, _image: &mut Image, target: usize) {
        let val1 = self.stack_pop();
        if val1.is_true() {
            self.program_counter = target /* interpret() everytime increments pc */- 1
        }
    }

    fn instr_brfalse(&mut self, _image: &mut Image, target: usize) {
        let val1 = self.stack_pop();
        if val1.is_false() {
            self.program_counter = target /* interpret() everytime increments pc */- 1
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

    pub fn is_true(&self) -> bool {
        !self.is_false()
    }

    pub fn is_false(&self) -> bool {
        match self {
            Value::Int32(n) => *n == 0,
            _ => false,
        }
    }

    pub fn add(self, y: Value) -> Value {
        match (self, y) {
            (Value::Int32(x), Value::Int32(y)) => Value::Int32(x + y),
            _ => panic!(),
        }
    }

    pub fn sub(self, y: Value) -> Value {
        match (self, y) {
            (Value::Int32(x), Value::Int32(y)) => Value::Int32(x - y),
            _ => panic!(),
        }
    }

    pub fn mul(self, y: Value) -> Value {
        match (self, y) {
            (Value::Int32(x), Value::Int32(y)) => Value::Int32(x * y),
            _ => panic!(),
        }
    }

    pub fn rem(self, y: Value) -> Value {
        match (self, y) {
            (Value::Int32(x), Value::Int32(y)) => Value::Int32(x % y),
            _ => panic!(),
        }
    }

    pub fn ge(self, y: Value) -> bool {
        match (self, y) {
            (Value::Int32(x), Value::Int32(y)) => x >= y,
            _ => panic!(),
        }
    }

    pub fn gt(self, y: Value) -> bool {
        match (self, y) {
            (Value::Int32(x), Value::Int32(y)) => x > y,
            _ => panic!(),
        }
    }

    pub fn le(self, y: Value) -> bool {
        match (self, y) {
            (Value::Int32(x), Value::Int32(y)) => x <= y,
            _ => panic!(),
        }
    }

    pub fn lt(self, y: Value) -> bool {
        match (self, y) {
            (Value::Int32(x), Value::Int32(y)) => x < y,
            _ => panic!(),
        }
    }

    pub fn ne(self, y: Value) -> bool {
        match (self, y) {
            (Value::Int32(x), Value::Int32(y)) => x != y,
            _ => panic!(),
        }
    }
}
