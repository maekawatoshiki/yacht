use crate::metadata::signature::*;
use llvm::{core::*, prelude::*};
use rustc_hash::FxHashMap;
use std::ffi::{c_void, CString};

type AssemblyMap = FxHashMap<String, TypeNamespaceMap>;
type TypeNamespaceMap = FxHashMap<String, TypeNameMap>;
type TypeNameMap = FxHashMap<String, FunctionMap>;
type FunctionMap = FxHashMap<String, Vec<Function>>;

#[derive(Clone)]
pub struct Function {
    pub ty: Type,
    pub function: *mut c_void,
    pub llvm_function: LLVMValueRef,
}

#[derive(Clone)]
pub struct BuiltinFunctions {
    map: AssemblyMap,
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
                    (ptr ) => { LLVMPointerType(LLVMInt8TypeInContext(ctx), 0) };
                }

                macro_rules! parse_ty {
                    (void) => { Type::void_ty() };
                    (i4  ) => { Type::i4_ty() };
                    (char) => { Type::char_ty() };
                    // (dbl)     => { VariableType::Double. to_llvmty(context) };
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
                    def_func!(        void, [str ],    write_line_string, "[mscorlib]System::Console.WriteLine(String)"),
                    def_func!(        void, [i4  ],    write_line_i4,     "[mscorlib]System::Console.WriteLine(int32)"),
                    def_func!(        void, [char],    write_line_char,   "[mscorlib]System::Console.WriteLine(char)"),
                ].into_iter().map(|(ty, function, llvm_function)| Function { ty, function, llvm_function }).collect();
                let write = vec![
                    def_func!(        void, [str ],    write_string,      "[mscorlib]System::Console.Write(String)"),
                    def_func!(        void, [i4  ],    write_i4,          "[mscorlib]System::Console.Write(int32)"),
                    def_func!(        void, [char],    write_char,        "[mscorlib]System::Console.Write(char)"),
                ].into_iter().map(|(ty, function, llvm_function)| Function { ty, function, llvm_function }).collect();
                let get_length = vec![
                    def_func!([0x20], i4  ,  [],       get_length,        "[mscorlib]System::String.get_Length()")
                ].into_iter().map(|(ty, function, llvm_function)| Function { ty, function, llvm_function}).collect();
                let get_chars = vec![
                    def_func!([0x20], char, [i4],      get_chars_i4,      "[mscorlib]System::String.get_Chars(int32)")
                ].into_iter().map(|(ty, function, llvm_function)| Function { ty, function, llvm_function }).collect();

                macro_rules! hashmap {
                    ($e:expr) => {{
                        $e.into_iter()
                            .map(|(x, y)| (x.to_string(), y))
                            .collect::<FxHashMap<_, _>>()
                    }};
                }

                let console_function_map: FunctionMap =
                    hashmap!(vec![("WriteLine", write_line), ("Write", write)]);
                let string_function_map: FunctionMap =
                    hashmap!(vec![("get_Length", get_length), ("get_Chars", get_chars)]);
                let type_name_map: TypeNameMap = hashmap!(vec![("Console", console_function_map),
                                                               ("String",  string_function_map)]);
                let type_namespace_map: TypeNamespaceMap =
                    hashmap!(vec![("System", type_name_map)]);
                let assembly_map: AssemblyMap = hashmap!(vec![("mscorlib", type_namespace_map)]);

                assembly_map
            },
        }
    }

    pub fn get_function(
        &self,
        aname: &str,
        tsname: &str,
        tname: &str,
        name: &str,
        ty: &Type,
    ) -> Option<&Function> {
        let funcs = self.map.get(aname)?.get(tsname)?.get(tname)?.get(name)?;
        funcs.iter().find(|f| &f.ty == ty)
    }

    pub fn get_helper_function(&self, name: &str) -> Option<&Function> {
        self.helper_map.get(name)
    }

    pub fn list_all_function(&self) -> Vec<Function> {
        let mut funcs = vec![];

        for (_, type_namespace_map) in &self.map {
            for (_, type_name_map) in type_namespace_map {
                for (_, function_map) in type_name_map {
                    for (_, func_list) in function_map {
                        for func in func_list {
                            funcs.push(func.clone());
                        }
                    }
                }
            }
        }

        for (_, func) in &self.helper_map {
            funcs.push(func.clone());
        }

        funcs
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
pub fn memory_alloc(len: u32) -> *mut u8 {
    Box::into_raw(vec![0u8; len as usize].into_boxed_slice()) as *mut u8
}

#[no_mangle]
pub fn new_szarray(elem_sz: u32, len: u32) -> *mut u8 {
    let ptr =
        Box::into_raw(vec![0u8; elem_sz as usize * len as usize + 4].into_boxed_slice()) as *mut u8;
    unsafe { *(ptr as *mut u32).add(0) = len };
    ptr
}
