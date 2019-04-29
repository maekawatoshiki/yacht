use crate::{
    exec::instruction::*,
    metadata::{class::*, metadata::*, method::*, signature::*},
};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::iter::repeat_with;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Value {
    Int32(i32),
    String(u32),
    Array(Rc<RefCell<Vec<Value>>>),
    Object(Rc<RefCell<ObjectValue>>),
}

#[derive(Debug, Clone)]
pub struct ObjectValue {
    fields: FxHashMap<String, Value>,
}

type AssemblyMap = FxHashMap<String, TypeNamespaceMap>;
type TypeNamespaceMap = FxHashMap<String, TypeNameMap>;
type TypeNameMap = FxHashMap<String, FunctionMap>;
type FunctionMap = FxHashMap<String, Vec<Function>>;

#[derive(Clone)]
pub struct Function {
    pub ty: Type,
    pub function: fn(&mut Interpreter),
}

#[derive(Clone)]
pub struct BuiltinFunctions {
    map: AssemblyMap,
}

pub struct Interpreter<'a> {
    pub image: &'a mut Image,
    builtins: BuiltinFunctions,
    stack: Vec<Value>,
    base_ptr: usize,
    stack_ptr: usize,
    program_counter: usize,
}

impl<'a> Interpreter<'a> {
    pub fn new(image: &'a mut Image) -> Self {
        Self {
            image,
            builtins: BuiltinFunctions::new(),
            stack: repeat_with(|| Value::Int32(0)).take(1024).collect(),
            base_ptr: 0,
            stack_ptr: 0,
            program_counter: 0,
        }
    }

    pub fn interpret(&mut self, method: &MethodBody, arguments: &[Value]) {
        macro_rules! numeric_op {
            ($op:ident) => {{
                let y = self.stack_pop();
                let x = self.stack_pop();
                self.stack_push(x.$op(y))
            }};
        }

        let (iseq, mut locals) = { (&method.body, vec![Value::Int32(0); method.locals_ty.len()]) };

        loop {
            let instr = &iseq[self.program_counter];
            match instr {
                Instruction::Ldstr { us_offset } => self.stack_push(Value::String(*us_offset)),
                Instruction::Ldc_I4_0 => self.stack_push(Value::Int32(0)),
                Instruction::Ldc_I4_1 => self.stack_push(Value::Int32(1)),
                Instruction::Ldc_I4_2 => self.stack_push(Value::Int32(2)),
                Instruction::Ldc_I4_3 => self.stack_push(Value::Int32(3)),
                Instruction::Ldc_I4_4 => self.stack_push(Value::Int32(4)),
                Instruction::Ldc_I4_8 => self.stack_push(Value::Int32(8)),
                Instruction::Ldc_I4_S { n } => self.stack_push(Value::Int32(*n)),
                Instruction::Ldc_I4 { n } => self.stack_push(Value::Int32(*n)),
                Instruction::Ldarg_0 => self.stack_push(arguments[0].clone()),
                Instruction::Ldarg_1 => self.stack_push(arguments[1].clone()),
                Instruction::Ldarg_2 => self.stack_push(arguments[2].clone()),
                Instruction::Ldarg_3 => self.stack_push(arguments[3].clone()),
                Instruction::Ldloc_0 => self.stack_push(locals[0].clone()),
                Instruction::Ldloc_1 => self.stack_push(locals[1].clone()),
                Instruction::Ldloc_2 => self.stack_push(locals[2].clone()),
                Instruction::Ldloc_3 => self.stack_push(locals[3].clone()),
                Instruction::Ldloc_S { n } => self.stack_push(locals[*n as usize].clone()),
                Instruction::Ldfld { table, entry } => self.instr_ldfld(*table, *entry),
                Instruction::Ldelem_U1 => self.instr_ldelem_in(),
                Instruction::Ldelem_I1 => self.instr_ldelem_in(),
                Instruction::Ldelem_I4 => self.instr_ldelem_in(),
                Instruction::Stloc_0 => locals[0] = self.stack_pop(),
                Instruction::Stloc_1 => locals[1] = self.stack_pop(),
                Instruction::Stloc_2 => locals[2] = self.stack_pop(),
                Instruction::Stloc_3 => locals[3] = self.stack_pop(),
                Instruction::Stloc_S { n } => locals[*n as usize] = self.stack_pop(),
                Instruction::Stfld { table, entry } => self.instr_stfld(*table, *entry),
                Instruction::Stelem_I1 => self.instr_stelem_in(),
                Instruction::Stelem_I4 => self.instr_stelem_in(),
                Instruction::Ldlen => self.instr_ldlen(),
                Instruction::Conv_I4 => {} // TODO
                Instruction::Pop => self.stack_ptr -= 1,
                Instruction::Dup => self.stack_dup(),
                Instruction::Bge { target } => self.instr_bge(*target),
                Instruction::Bgt { target } => self.instr_bgt(*target),
                Instruction::Blt { target } => self.instr_blt(*target),
                Instruction::Ble { target } => self.instr_ble(*target),
                Instruction::Beq { target } => self.instr_beq(*target),
                Instruction::Bne_un { target } => self.instr_bne_un(*target),
                Instruction::Brtrue { target } => self.instr_brtrue(*target),
                Instruction::Brfalse { target } => self.instr_brfalse(*target),
                Instruction::Br { target } => self.program_counter = target - 1,
                Instruction::Clt => self.instr_clt(),
                Instruction::Ceq => self.instr_ceq(),
                Instruction::Add => numeric_op!(add),
                Instruction::Sub => numeric_op!(sub),
                Instruction::Mul => numeric_op!(mul),
                Instruction::Rem => numeric_op!(rem),
                Instruction::Call { table, entry } => self.instr_call(*table, *entry),
                Instruction::CallVirt { table, entry } => self.instr_callvirt(*table, *entry),
                Instruction::Newobj { table, entry } => self.instr_newobj(*table, *entry),
                Instruction::Newarr { table, entry } => self.instr_newarr(*table, *entry),
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
    pub fn stack_top(&mut self) -> Value {
        self.stack[self.base_ptr + self.stack_ptr - 1].clone()
    }

    #[inline]
    pub fn stack_dup(&mut self) {
        let top = self.stack_top();
        self.stack_push(top);
    }

    #[inline]
    pub fn stack_pop(&mut self) -> Value {
        self.stack_ptr -= 1;
        self.stack[self.base_ptr + self.stack_ptr].clone()
    }

    #[inline]
    pub fn stack_pop_last_elements(&mut self, n: usize) -> Vec<Value> {
        self.stack_ptr -= n;
        self.stack[(self.base_ptr + self.stack_ptr)..].to_vec()
    }

    fn instr_ldfld(&mut self, table: usize, entry: usize) {
        let obj = self.stack_pop();
        let table = &self.image.metadata.metadata_stream.tables[table][entry - 1];
        match table {
            Table::Field(f) => {
                let name = self.image.get_string(f.name);
                self.stack_push(
                    obj.as_object()
                        .unwrap()
                        .borrow_mut()
                        .fields
                        .get(name)
                        .unwrap()
                        .clone(),
                )
            }
            e => unimplemented!("{:?}", e),
        }
    }

    fn instr_stfld(&mut self, table: usize, entry: usize) {
        let value = self.stack_pop();
        let obj = self.stack_pop();
        let table = &self.image.metadata.metadata_stream.tables[table][entry - 1];
        match table {
            Table::Field(f) => {
                let name = self.image.get_string(f.name);
                *obj.as_object()
                    .unwrap()
                    .borrow_mut()
                    .fields
                    .get_mut(name)
                    .unwrap() = value;
            }
            e => unimplemented!("{:?}", e),
        }
    }

    fn instr_ldelem_in(&mut self) {
        let index = self.stack_pop();
        let array = self.stack_pop();
        let value = array.as_array().unwrap().borrow()[index.as_int32().unwrap() as usize].clone();
        self.stack_push(value);
    }

    fn instr_stelem_in(&mut self) {
        let value = self.stack_pop();
        let index = self.stack_pop();
        let array = self.stack_pop();
        array.as_array().unwrap().borrow_mut()[index.as_int32().unwrap() as usize] = value;
    }

    fn instr_ldlen(&mut self) {
        let array = self.stack_pop();
        self.stack_push(Value::Int32(
            array.as_array().unwrap().borrow_mut().len() as i32
        ))
    }

    fn instr_call(&mut self, table: usize, entry: usize) {
        // TODO: Refacotr
        let table = &self.image.metadata.metadata_stream.tables[table][entry - 1];
        match table {
            Table::MemberRef(mrt) => {
                let (table, entry) = mrt.class_table_and_entry();
                let class = &self.image.metadata.metadata_stream.tables[table][entry - 1];
                match class {
                    Table::TypeRef(trt) => {
                        let (asm_ref_name, ty_namespace, ty_name) =
                            self.image.get_info_from_type_ref_table(trt);
                        let name = self.image.get_string(mrt.name);
                        let ty = self.image.get_method_ref_type_from_signature(mrt.signature);

                        dprintln!("Method type: {:?}", ty);
                        dprintln!("[{}]{}.{}::{}", asm_ref_name, ty_namespace, ty_name, name);

                        if let Some(func) = self.builtins.get_function(
                            asm_ref_name.as_str(),
                            ty_namespace.as_str(),
                            ty_name.as_str(),
                            name.as_str(),
                            &ty,
                        ) {
                            (func.function)(self);
                        }
                    }
                    _ => unimplemented!(),
                }
            }
            Table::MethodDef(mdt) => {
                let saved_program_counter = self.program_counter;
                self.program_counter = 0;

                let method = self.image.get_method(mdt.rva);
                let params = {
                    let method_sig = method.ty.as_fnptr().unwrap();
                    let has_this = if method_sig.has_this() { 1 } else { 0 };
                    self.stack_pop_last_elements(method_sig.params.len() as usize + has_this)
                };

                self.interpret(&method, &params);

                self.program_counter = saved_program_counter;
            }
            e => unimplemented!("call: unimplemented: {:?}", e),
        }
    }

    fn instr_callvirt(&mut self, table: usize, entry: usize) {
        // TODO: Refacotr
        let table = &self.image.metadata.metadata_stream.tables[table][entry - 1];
        match table {
            Table::MemberRef(mrt) => {
                let (table, entry) = mrt.class_table_and_entry();
                let class = &self.image.metadata.metadata_stream.tables[table][entry - 1];
                match class {
                    Table::TypeRef(trt) => {
                        let (asm_ref_name, ty_namespace, ty_name) =
                            self.image.get_info_from_type_ref_table(trt);
                        let name = self.image.get_string(mrt.name);

                        dprintln!("{}-{}-{}-{}", asm_ref_name, ty_namespace, ty_name, name);

                        if asm_ref_name == "mscorlib"
                            && ty_namespace == "System"
                            && ty_name == "String"
                        {
                            match name.as_str() {
                                "get_Length" => {
                                    let val = self.stack_pop();
                                    self.stack_push(Value::Int32(
                                        self.image.get_user_string(val.as_string().unwrap()).len()
                                            as i32,
                                    ));
                                }
                                "get_Chars" => {
                                    let idx = self.stack_pop().as_int32().unwrap() as usize;
                                    let val = self.stack_pop();
                                    self.stack_push(Value::Int32(
                                        self.image.get_user_string(val.as_string().unwrap())[idx]
                                            as i32,
                                    ));
                                }
                                _ => unimplemented!(),
                            }
                        }
                    }
                    _ => unimplemented!(),
                }
            } // TODO
            Table::MethodDef(mdt) => {
                let saved_program_counter = self.program_counter;
                self.program_counter = 0;

                let method = self.image.get_method(mdt.rva);
                let params = {
                    let method_sig = method.ty.as_fnptr().unwrap();
                    let has_this = if method_sig.has_this() { 1 } else { 0 };
                    self.stack_pop_last_elements(method_sig.params.len() as usize + has_this)
                };

                self.interpret(&method, &params);

                self.program_counter = saved_program_counter;
            }
            e => unimplemented!("call: unimplemented: {:?}", e),
        }
    }

    fn instr_newobj(&mut self, table: usize, entry: usize) {
        // TODO: Refactor
        let table = &self.image.metadata.metadata_stream.tables[table][entry - 1];
        match table {
            Table::MemberRef(_mrt) => {} // TODO
            Table::MethodDef(mdt) => {
                let saved_program_counter = self.program_counter;
                self.program_counter = 0;

                let method = self.image.get_method(mdt.rva);
                let new_obj = Value::Object(Rc::new(RefCell::new({
                    let class = &method.class.borrow();
                    let mut fields = FxHashMap::default();
                    for ClassField { name, .. } in &class.fields {
                        fields.insert(name.clone(), Value::Int32(0));
                    }
                    ObjectValue { fields }
                })));
                let mut params = {
                    let method_sig = method.ty.as_fnptr().unwrap();
                    self.stack_pop_last_elements(method_sig.params.len() as usize)
                };
                let mut actual_params = vec![new_obj.clone()];
                actual_params.append(&mut params);

                self.interpret(&method, &actual_params);
                self.stack_push(new_obj);

                self.program_counter = saved_program_counter;
            }
            e => unimplemented!("call: unimplemented: {:?}", e),
        }
    }

    fn instr_newarr(&mut self, table: usize, entry: usize) {
        // TODO: Refactor
        let val = self.stack_pop();
        let table = &self.image.metadata.metadata_stream.tables[table][entry - 1];
        match table {
            Table::TypeRef(trt) => {
                let (asm_ref_name, ty_namespace, ty_name) =
                    self.image.get_info_from_type_ref_table(trt);
                dprintln!("ty: [{}]{}.{}", asm_ref_name, ty_namespace, ty_name);
                match (
                    asm_ref_name.as_str(),
                    ty_namespace.as_str(),
                    ty_name.as_str(),
                ) {
                    ("mscorlib", "System", "Int32") => {
                        self.stack_push(Value::Array(Rc::new(RefCell::new(
                            repeat_with(|| Value::Int32(0))
                                .take(val.as_int32().unwrap() as usize)
                                .collect(),
                        ))))
                    }
                    ("mscorlib", "System", "Boolean") => {
                        self.stack_push(Value::Array(Rc::new(RefCell::new(
                            repeat_with(|| Value::Int32(0))
                                .take(val.as_int32().unwrap() as usize)
                                .collect(),
                        ))))
                    }
                    _ => unimplemented!(),
                }
            }
            e => unimplemented!("newarr: unimplemented: {:?}", e),
        }
    }

    fn instr_bge(&mut self, target: usize) {
        let val2 = self.stack_pop();
        let val1 = self.stack_pop();
        if val1.ge(val2) {
            self.program_counter = target /* interpret() everytime increments pc */- 1
        }
    }

    fn instr_bgt(&mut self, target: usize) {
        let val2 = self.stack_pop();
        let val1 = self.stack_pop();
        if val1.gt(val2) {
            self.program_counter = target /* interpret() everytime increments pc */- 1
        }
    }

    fn instr_ble(&mut self, target: usize) {
        let val2 = self.stack_pop();
        let val1 = self.stack_pop();
        if val1.le(val2) {
            self.program_counter = target /* interpret() everytime increments pc */- 1
        }
    }

    fn instr_blt(&mut self, target: usize) {
        let val2 = self.stack_pop();
        let val1 = self.stack_pop();
        if val1.lt(val2) {
            self.program_counter = target /* interpret() everytime increments pc */- 1
        }
    }

    fn instr_beq(&mut self, target: usize) {
        let val2 = self.stack_pop();
        let val1 = self.stack_pop();
        if val1.eq(val2) {
            self.program_counter = target /* interpret() everytime increments pc */- 1
        }
    }

    fn instr_bne_un(&mut self, target: usize) {
        let val2 = self.stack_pop();
        let val1 = self.stack_pop();
        if val1.ne(val2) {
            self.program_counter = target /* interpret() everytime increments pc */- 1
        }
    }

    fn instr_brtrue(&mut self, target: usize) {
        let val1 = self.stack_pop();
        if val1.is_true() {
            self.program_counter = target /* interpret() everytime increments pc */- 1
        }
    }

    fn instr_brfalse(&mut self, target: usize) {
        let val1 = self.stack_pop();
        if val1.is_false() {
            self.program_counter = target /* interpret() everytime increments pc */- 1
        }
    }

    fn instr_clt(&mut self) {
        let val2 = self.stack_pop();
        let val1 = self.stack_pop();
        self.stack_push(Value::Int32(if val1.lt(val2) { 1 } else { 0 }))
    }

    fn instr_ceq(&mut self) {
        let val2 = self.stack_pop();
        let val1 = self.stack_pop();
        self.stack_push(Value::Int32(if val1.eq(val2) { 1 } else { 0 }))
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

    pub fn as_object(&self) -> Option<Rc<RefCell<ObjectValue>>> {
        match self {
            Value::Object(obj) => Some(obj.clone()),
            _ => None,
        }
    }

    pub fn as_array(&self) -> Option<Rc<RefCell<Vec<Value>>>> {
        match self {
            Value::Array(obj) => Some(obj.clone()),
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

    pub fn eq(self, y: Value) -> bool {
        match (self, y) {
            (Value::Int32(x), Value::Int32(y)) => x == y,
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

impl BuiltinFunctions {
    #[rustfmt::skip]
    pub fn new() -> Self {
        Self {
            map: {
                let write_line = vec![
                    (Type::simple_method_ty(Type::void_ty(), &[Type::string_ty()]),
                     write_line_string as fn(&mut Interpreter)),
                    (Type::simple_method_ty(Type::void_ty(), &[Type::i4_ty()]), write_line_i4),
                    (Type::simple_method_ty(Type::void_ty(), &[Type::char_ty()]), write_line_char)
                ] .into_iter() .map(|(ty, function)| Function { ty, function }).collect();
                let write = vec![
                    (Type::simple_method_ty(Type::void_ty(), &[Type::string_ty()]),
                     write_string as fn(&mut Interpreter)),
                    (Type::simple_method_ty(Type::void_ty(), &[Type::i4_ty()]), write_i4),
                    (Type::simple_method_ty(Type::void_ty(), &[Type::char_ty()]), write_char),
                ] .into_iter() .map(|(ty, function)| Function { ty, function }).collect();

                macro_rules! hashmap {
                    ($e:expr) => {{
                        $e.into_iter()
                            .map(|(x, y)| (x.to_string(), y))
                            .collect::<FxHashMap<_, _>>()
                    }};
                }

                let function_map: FunctionMap =
                    hashmap!(vec![("WriteLine", write_line), ("Write", write)]);
                let type_name_map: TypeNameMap = hashmap!(vec![("Console", function_map)]);
                let type_namespace_map: TypeNamespaceMap =
                    hashmap!(vec![("System", type_name_map)]);
                let assembly_map: AssemblyMap = hashmap!(vec![("mscorlib", type_namespace_map)]);
                assembly_map
            },
        }
    }

    pub fn get_function(
        &mut self,
        aname: &str,
        tsname: &str,
        tname: &str,
        name: &str,
        ty: &Type,
    ) -> Option<&Function> {
        let funcs = self.map.get(aname)?.get(tsname)?.get(tname)?.get(name)?;
        funcs.iter().find(|f| &f.ty == ty)
    }
}

fn write_line_string(interp: &mut Interpreter) {
    let val = interp.stack_pop();
    println!(
        "{}",
        String::from_utf16_lossy(interp.image.get_user_string(val.as_string().unwrap()))
    );
}

fn write_line_i4(interp: &mut Interpreter) {
    println!("{}", interp.stack_pop().as_int32().unwrap());
}

fn write_line_char(interp: &mut Interpreter) {
    println!(
        "{}",
        String::from_utf16_lossy(&[interp.stack_pop().as_int32().unwrap() as u16])
    );
}
fn write_string(interp: &mut Interpreter) {
    let val = interp.stack_pop();
    print!(
        "{}",
        String::from_utf16_lossy(interp.image.get_user_string(val.as_string().unwrap()))
    );
}

fn write_i4(interp: &mut Interpreter) {
    print!("{}", interp.stack_pop().as_int32().unwrap());
}

fn write_char(interp: &mut Interpreter) {
    print!(
        "{}",
        String::from_utf16_lossy(&[interp.stack_pop().as_int32().unwrap() as u16])
    );
}
