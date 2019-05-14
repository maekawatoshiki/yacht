use crate::{
    metadata::signature::*,
    util::{holder::MethodHolder, name_path::*},
};
use llvm::{core::*, prelude::*};
use rustc_hash::FxHashMap;
use std::ffi::{c_void, CString};

#[derive(Clone)]
pub struct Function {
    pub ty: Type,
    pub function: *mut c_void,
    pub llvm_function: LLVMValueRef,
}

#[derive(Clone)]
pub struct BuiltinFunctions {
    map: MethodHolder<Function>,
    helper_map: FxHashMap<String, Function>,
}

impl BuiltinFunctions {
    #[rustfmt::skip]
    pub unsafe fn new(ctx: LLVMContextRef, module: LLVMModuleRef) -> Self {
        Self {
            helper_map: {
                let mut map = FxHashMap::default();
                map.insert(
                    "memory_alloc".to_string(),
                    Function {
                        ty: Type::void_ty(),
                        function: memory_alloc as *mut c_void,
                        llvm_function: LLVMAddFunction(
                            module,
                            CString::new("memory_alloc").unwrap().as_ptr(),
                            LLVMFunctionType(
                                LLVMPointerType(LLVMInt8TypeInContext(ctx), 0),
                                vec![LLVMInt32TypeInContext(ctx)].as_mut_ptr(),
                                1, 0))
                    }
                );
                map.insert(
                    "new_szarray".to_string(),
                    Function {
                        ty: Type::void_ty(),
                        function: new_szarray as *mut c_void,
                        llvm_function: LLVMAddFunction(
                            module,
                            CString::new("new_szarray").unwrap().as_ptr(),
                            LLVMFunctionType(
                                LLVMPointerType(LLVMInt8TypeInContext(ctx), 0),
                                vec![LLVMInt32TypeInContext(ctx),
                                    LLVMInt32TypeInContext(ctx)
                                    ].as_mut_ptr(),
                                2, 0))
                    }
                );
                map
            },
            map: {
                macro_rules! parse_llvm_ty {
                    (void) => { LLVMVoidTypeInContext(ctx) };
                    (i4  ) => { LLVMInt32TypeInContext(ctx) };
                    (char) => { LLVMInt32TypeInContext(ctx) };
                    // (dbl)     => { VariableType::Double. to_llvmty(context) };
                    (str ) => { LLVMPointerType(LLVMInt8TypeInContext(ctx), 0) };
                    (obj ) => { LLVMPointerType(LLVMInt8TypeInContext(ctx), 0) };
                    (ptr ) => { LLVMPointerType(LLVMInt8TypeInContext(ctx), 0) };
                }

                macro_rules! parse_ty {
                    (void) => { Type::void_ty() };
                    (i4  ) => { Type::i4_ty() };
                    (char) => { Type::char_ty() };
                    // (dbl)     => { VariableType::Double. to_llvmty(context) };
                    (obj ) => { Type::object_ty() };
                    (str ) => { Type::string_ty() };
                }

                macro_rules! def_func {
                    ($ret_ty:ident, [ $($param_ty:ident),* ], $f:expr, $name:expr) => {{
                        def_func!([0], $ret_ty, [$($param_ty),*], $f, $name)
                    }};

                    ([$flags:expr], $ret_ty:ident, [ $($param_ty:ident),* ], $f:expr, $name:expr) => {{
                        let mut llvm_params_ty = if $flags & 0x20 > 0 { vec![parse_llvm_ty!(ptr)] } else { vec![] };
                        llvm_params_ty.append(&mut vec![$(parse_llvm_ty!($param_ty)),*]);
                        let params_ty = vec![$(parse_ty!($param_ty)),*];
                        let func_ty = LLVMFunctionType(
                            parse_llvm_ty!($ret_ty),
                            llvm_params_ty.as_mut_ptr(),
                            llvm_params_ty.len() as u32, 0);
                        (Type::full_method_ty($flags, parse_ty!($ret_ty), &params_ty),
                         $f as *mut c_void,
                         LLVMAddFunction(module, CString::new($name).unwrap().as_ptr(), func_ty))
                    }}
                }

                let write_line = vec![
                    def_func!(        void, [str ],     write_line_string,     "[mscorlib]System::Console.WriteLine(String)"),
                    def_func!(        void, [i4  ],     write_line_i4,         "[mscorlib]System::Console.WriteLine(int32)"),
                    def_func!(        void, [char],     write_line_char,       "[mscorlib]System::Console.WriteLine(char)"),
                    def_func!(        void, [str, obj], write_line_string_obj, "[mscorlib]System::Console.WriteLine(String, Object)"),
                ].into_iter().map(|(ty, function, llvm_function)| Function { ty, function, llvm_function }).collect();
                let write = vec![
                    def_func!(        void, [str ],     write_string,          "[mscorlib]System::Console.Write(String)"),
                    def_func!(        void, [i4  ],     write_i4,              "[mscorlib]System::Console.Write(int32)"),
                    def_func!(        void, [char],     write_char,            "[mscorlib]System::Console.Write(char)"),
                ].into_iter().map(|(ty, function, llvm_function)| Function { ty, function, llvm_function }).collect();
                let get_length = vec![
                    def_func!([0x20], i4  ,  [],        get_length,            "[mscorlib]System::String.get_Length()")
                ].into_iter().map(|(ty, function, llvm_function)| Function { ty, function, llvm_function}).collect();
                let get_chars = vec![
                    def_func!([0x20], char, [i4],       get_chars_i4,          "[mscorlib]System::String.get_Chars(int32)")
                ].into_iter().map(|(ty, function, llvm_function)| Function { ty, function, llvm_function }).collect();
                let int32_to_string = vec![
                    def_func!([0x20], str,  [],         int_to_string,         "[mscorlib]System::Int32.ToString()")
                ].into_iter().map(|(ty, function, llvm_function)| Function { ty, function, llvm_function }).collect();
                let obj_to_string = vec![
                    def_func!([0x20], str,  [],         object_to_string,      "[mscorlib]System::Object.ToString()"),
                ].into_iter().map(|(ty, function, llvm_function)| Function { ty, function, llvm_function }).collect();

                let mut holder = MethodHolder::new();

                holder.add_list(MethodFullPath("mscorlib", "System", "Console", "WriteLine"),  write_line);
                holder.add_list(MethodFullPath("mscorlib", "System", "Console", "Write"),      write);
                holder.add_list(MethodFullPath("mscorlib", "System", "Object",  "ToString"),   obj_to_string);
                holder.add_list(MethodFullPath("mscorlib", "System", "Int32",   "ToString"),   int32_to_string);
                holder.add_list(MethodFullPath("mscorlib", "System", "String",  "get_Chars"),  get_chars);
                holder.add_list(MethodFullPath("mscorlib", "System", "String",  "get_Length"), get_length);

                holder
            },
        }
    }

    pub fn get_method<'a, T: Into<MethodFullPath<'a>>>(
        &self,
        path: T,
        ty: &Type,
    ) -> Option<&Function> {
        let methods = self.map.get_list(path.into())?;
        methods.iter().find(|f| &f.ty == ty)
    }

    pub fn get_helper_function(&self, name: &str) -> Option<&Function> {
        self.helper_map.get(name)
    }

    pub fn list_all_function(&self) -> Vec<Function> {
        let mut functions = self.map.collect();
        self.helper_map.iter().for_each(|(_, f)| {
            functions.push(f.clone());
        });
        functions
    }
}

#[no_mangle]
pub fn write_line_i4(n: i32) {
    println!("{}", n);
}

#[no_mangle]
pub fn write_line_char(c: i32) {
    println!("{}", c as u8 as char);
}

#[no_mangle]
pub fn write_line_string(s: *mut String) {
    println!("{}", unsafe { &*s });
}

#[no_mangle]
pub unsafe fn write_line_string_obj(s: *mut String, o: *mut u8) {
    let string = { &*s };
    let class_int32 = o as *mut u64;
    let method_table = *class_int32.offset(0) as *mut u64;
    let to_string =
        ::std::mem::transmute::<u64, fn(*mut u8) -> *mut String>(*method_table.offset(0));
    let string2 = &*to_string(class_int32 as *mut u8);
    for (i, s) in string.split("{0}").enumerate() {
        if i > 0 {
            print!("{}", string2)
        }
        print!("{}", s);
    }
    println!();
}

#[no_mangle]
pub fn write_i4(n: i32) {
    print!("{}", n);
}

#[no_mangle]
pub fn write_char(c: i32) {
    print!("{}", c as u8 as char);
}

#[no_mangle]
pub fn write_string(s: *mut String) {
    print!("{}", unsafe { &*s });
}

#[no_mangle]
pub fn get_length(s: *mut String) -> i32 {
    unsafe { &*s }.len() as i32
}

#[no_mangle]
pub fn get_chars_i4(s: *mut String, i: i32) -> i32 {
    unsafe { &*s }.as_str().as_bytes()[i as usize] as i32
}

#[no_mangle]
pub fn object_to_string(_obj: *mut u8) -> *mut String {
    unimplemented!()
}

#[no_mangle]
pub fn int_to_string(int32: *mut u8) -> *mut String {
    unsafe {
        let class_int32 = int32 as *mut u64;
        let value = *class_int32.offset(1) as i32;
        Box::into_raw(Box::new(format!("{}", value)))
    }
}

// TODO: Currently use boehm-gc. Replace with better way in the future.
#[link(name = "gc")]
extern "C" {
    fn GC_malloc(len: u32) -> *mut u8;
// fn GC_register_finalizer(obj: *mut u8, f: *mut u8, cd: *mut u8, ofn: *mut u8, ocd: *mut u8);
}

#[no_mangle]
pub fn memory_alloc(len: u32) -> *mut u8 {
    unsafe { GC_malloc(len) }
}

#[no_mangle]
pub fn new_szarray(elem_sz: u32, len: u32) -> *mut u8 {
    let ptr = unsafe { GC_malloc(elem_sz * len + 4) };
    unsafe { *(ptr as *mut u32).add(0) = len };
    ptr
}
