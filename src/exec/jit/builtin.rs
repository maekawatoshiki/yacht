// TODO: GC support for String(Vec<u16>)

use crate::{
    exec::jit::jit::*,
    metadata::signature::*,
    util::{name_path::*, resolver::*},
};
use llvm::{core::*, prelude::*};
use rustc_hash::FxHashMap;
use std::{
    char::decode_utf16,
    ffi::{c_void, CString},
    mem, ptr,
};

#[derive(Clone, Debug)]
pub struct Function {
    pub ty: Type,
    pub function: *mut c_void,
    pub llvm_function: LLVMValueRef,
}

#[derive(Clone)]
pub struct BuiltinFunctions {
    pub map: NameResolver<Vec<Function>>,
    pub helper_map: FxHashMap<String, Function>,
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
                    (r8  ) => { LLVMDoubleTypeInContext(ctx) };
                    (char) => { LLVMInt32TypeInContext(ctx) };
                    (str ) => { LLVMPointerType(LLVMInt8TypeInContext(ctx), 0) };
                    (obj ) => { LLVMPointerType(LLVMInt8TypeInContext(ctx), 0) };
                    (obja) => { LLVMPointerType(LLVMInt8TypeInContext(ctx), 0) };
                    (ptr ) => { LLVMPointerType(LLVMInt8TypeInContext(ctx), 0) };
                }

                macro_rules! parse_ty {
                    (void)  => { Type::void_ty() };
                    (i4  )  => { Type::i4_ty() };
                    (r8  )  => { Type::r8_ty() };
                    (char)  => { Type::char_ty() };
                    (obj )  => { Type::object_ty() };
                    (obja)  => { Type::object_szarr_ty() };
                    (str )  => { Type::string_ty() };
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

                let sqrt = vec![
                    def_func!(        r8,   [r8 ],      0,                     "llvm.sqrt.f64")
                ].into_iter().map(|(ty, function, llvm_function)| Function { ty, function, llvm_function }).collect();
                let abs = vec![
                    def_func!(        r8,   [r8 ],      0,                     "llvm.fabs.f64")
                ].into_iter().map(|(ty, function, llvm_function)| Function { ty, function, llvm_function }).collect();
                let cos = vec![
                    def_func!(        r8,   [r8 ],      0,                     "llvm.cos.f64")
                ].into_iter().map(|(ty, function, llvm_function)| Function { ty, function, llvm_function }).collect();
                let sin = vec![
                    def_func!(        r8,   [r8 ],      0,                     "llvm.sin.f64")
                ].into_iter().map(|(ty, function, llvm_function)| Function { ty, function, llvm_function }).collect();
                let pow = vec![
                    def_func!(        r8,   [r8, r8],   0,                     "llvm.pow.f64")
                ].into_iter().map(|(ty, function, llvm_function)| Function { ty, function, llvm_function }).collect();
                let write_line = vec![
                    def_func!(        void, [str ],     write_line_string,     "[mscorlib]System::Console.WriteLine(String)"),
                    def_func!(        void, [i4  ],     write_line_i4,         "[mscorlib]System::Console.WriteLine(int32)"),
                    def_func!(        void, [r8  ],     write_line_r8,         "[mscorlib]System::Console.WriteLine(float64)"),
                    def_func!(        void, [char],     write_line_char,       "[mscorlib]System::Console.WriteLine(char)"),
                    def_func!(        void, [str, obj], write_line_string_obj, "[mscorlib]System::Console.WriteLine(String, Object)"),
                ].into_iter().map(|(ty, function, llvm_function)| Function { ty, function, llvm_function }).collect();
                let write = vec![
                    def_func!(        void, [str ],     write_string,          "[mscorlib]System::Console.Write(String)"),
                    def_func!(        void, [i4  ],     write_i4,              "[mscorlib]System::Console.Write(int32)"),
                    def_func!(        void, [r8  ],     write_r8,              "[mscorlib]System::Console.Write(float64)"),
                    def_func!(        void, [char],     write_char,            "[mscorlib]System::Console.Write(char)"),
                ].into_iter().map(|(ty, function, llvm_function)| Function { ty, function, llvm_function }).collect();
                let get_length = vec![
                    def_func!([0x20], i4  ,  [],        get_length,            "[mscorlib]System::String.get_Length()")
                ].into_iter().map(|(ty, function, llvm_function)| Function { ty, function, llvm_function}).collect();
                let get_chars = vec![
                    def_func!([0x20], char, [i4],       get_chars_i4,          "[mscorlib]System::String.get_Chars(int32)")
                ].into_iter().map(|(ty, function, llvm_function)| Function { ty, function, llvm_function }).collect();
                let concat = vec![
                    def_func!(        str,  [obja],     concat_obj_arr,        "[mscorlib]System::String.Concat(Object[])"),
                    def_func!(        str,  [obj, obj], concat_obj_obj,        "[mscorlib]System::String.Concat(Object, Object)"),
                    def_func!(        str,  [str, str], concat_str_str,        "[mscorlib]System::String.Concat(String, String)"),
                    def_func!(        str,  [obj, obj, obj], concat_obj_obj_obj, "[mscorlib]System::String.Concat(Object, Object, Object)")
                ].into_iter().map(|(ty, function, llvm_function)| Function { ty, function, llvm_function }).collect();
                let int32_to_string = vec![
                    def_func!([0x20], str,  [],         int_to_string,         "[mscorlib]System::Int32.ToString()")
                ].into_iter().map(|(ty, function, llvm_function)| Function { ty, function, llvm_function }).collect();
                let obj_to_string = vec![
                    def_func!([0x20], str,  [],         object_to_string,      "[mscorlib]System::Object.ToString()"),
                ].into_iter().map(|(ty, function, llvm_function)| Function { ty, function, llvm_function }).collect();
                let string_to_string = vec![
                    def_func!([0x20], str,  [],         string_to_string,      "[mscorlib]System::String.ToString()"),
                ].into_iter().map(|(ty, function, llvm_function)| Function { ty, function, llvm_function }).collect();

                let mut resolver = NameResolver::new();

                resolver.add(MethodPath(vec!["mscorlib", "System", "Console", "WriteLine" ]), write_line      );
                resolver.add(MethodPath(vec!["mscorlib", "System", "Console", "Write"     ]), write           );
                resolver.add(MethodPath(vec!["mscorlib", "System", "Object",  "ToString"  ]), obj_to_string   );
                resolver.add(MethodPath(vec!["mscorlib", "System", "Int32",   "ToString"  ]), int32_to_string );
                resolver.add(MethodPath(vec!["mscorlib", "System", "String",  "get_Chars" ]), get_chars       );
                resolver.add(MethodPath(vec!["mscorlib", "System", "String",  "get_Length"]), get_length      );
                resolver.add(MethodPath(vec!["mscorlib", "System", "String",  "ToString"  ]), string_to_string);
                resolver.add(MethodPath(vec!["mscorlib", "System", "String",  "Concat"    ]), concat          );
                resolver.add(MethodPath(vec!["mscorlib", "System", "Math",    "Sqrt"      ]), sqrt            );
                resolver.add(MethodPath(vec!["mscorlib", "System", "Math",    "Sin"       ]), sin             );
                resolver.add(MethodPath(vec!["mscorlib", "System", "Math",    "Cos"       ]), cos             );
                resolver.add(MethodPath(vec!["mscorlib", "System", "Math",    "Abs"       ]), abs             );
                resolver.add(MethodPath(vec!["mscorlib", "System", "Math",    "Pow"       ]), pow             );

                resolver
            },
        }
    }

    pub fn get_method<'a, T: Into<MethodPath<'a>>>(&self, path: T, ty: &Type) -> Option<&Function> {
        let methods = self.map.get(path.into())?;
        methods.iter().find(|f| &f.ty == ty)
    }

    pub fn get_helper_function(&self, name: &str) -> Option<&Function> {
        self.helper_map.get(name)
    }

    pub fn list_all_function(&self) -> Vec<Function> {
        let mut functions: Vec<Function> =
            self.map.collect_values().into_iter().flatten().collect();
        functions.append(&mut self.helper_map.iter().map(|(_, f)| f.clone()).collect());
        functions
    }
}

#[no_mangle]
pub fn write_line_i4(n: i32) {
    println!("{}", n);
}

#[no_mangle]
pub fn write_line_r8(n: f64) {
    println!("{}", n);
}

#[no_mangle]
pub fn write_line_char(c: u16) {
    println!(
        "{}",
        decode_utf16([c].iter().cloned()).next().unwrap().unwrap()
    );
}

#[no_mangle]
pub fn write_line_string(system_string: *mut u64) {
    let utf16_string_ptr = unsafe { retrieve_utf16_string_from_system_string(system_string) };
    println!(
        "{}",
        String::from_utf16_lossy(unsafe { &*utf16_string_ptr })
    );
}

unsafe fn retrieve_utf16_string_from_system_string(system_string: *mut u64) -> *mut Vec<u16> {
    *system_string.offset(1) as *mut Vec<u16>
}

unsafe fn new_system_string(s: String) -> *mut u64 {
    let system_string = memory_alloc(16) as *mut u64;
    *(system_string.offset(0) as *mut MethodTablePtrTy) =
        STRING_METHOD_TABLE_PTR.with(|smp| smp.borrow().unwrap());
    *(system_string.offset(1) as *mut *mut Vec<u16>) = new_utf16_string(s);
    system_string
}

unsafe fn get_virtual_to_string_method(obj: *mut u64) -> fn(*mut u64) -> *mut u64 {
    let method_table = *obj.offset(0) as *mut u64;
    mem::transmute::<u64, fn(*mut u64) -> *mut u64>(*method_table.offset(0))
}

unsafe fn convert_object_to_string(obj: *mut u64) -> String {
    String::from_utf16_lossy(&*retrieve_utf16_string_from_system_string(
        (get_virtual_to_string_method(obj))(obj),
    ))
}

#[no_mangle]
unsafe fn concat_str_str(s1: *mut u64, s2: *mut u64) -> *mut u64 {
    let mut s1 = String::from_utf16_lossy(&*retrieve_utf16_string_from_system_string(s1));
    let s2 = String::from_utf16_lossy(&*retrieve_utf16_string_from_system_string(s2));
    s1.push_str(s2.as_str());
    new_system_string(s1)
}

#[no_mangle]
unsafe fn concat_obj_arr(objs: *mut u64) -> *mut u64 {
    let len = *objs;
    let objs = objs.add(1) as *mut u64;
    let mut res = "".to_string();
    for i in 0..len {
        let system_object = *objs.add(i as usize) as *mut u64;
        let s = convert_object_to_string(system_object);
        res.push_str(s.as_str());
    }
    new_system_string(res)
}

#[no_mangle]
unsafe fn concat_obj_obj(obj1: *mut u64, obj2: *mut u64) -> *mut u64 {
    let mut s1 = convert_object_to_string(obj1);
    let s2 = convert_object_to_string(obj2);
    s1.push_str(s2.as_str());
    new_system_string(s1)
}

#[no_mangle]
unsafe fn concat_obj_obj_obj(obj1: *mut u64, obj2: *mut u64, obj3: *mut u64) -> *mut u64 {
    let mut s1 = convert_object_to_string(obj1);
    let s2 = convert_object_to_string(obj2);
    let s3 = convert_object_to_string(obj3);
    s1.push_str(s2.as_str());
    s1.push_str(s3.as_str());
    new_system_string(s1)
}

#[no_mangle]
pub unsafe fn write_line_string_obj(system_string: *mut u64, class_int32: *mut u64) {
    let string =
        String::from_utf16_lossy({ &*retrieve_utf16_string_from_system_string(system_string) });
    let to_string = get_virtual_to_string_method(class_int32);
    let string2 = String::from_utf16_lossy(&*retrieve_utf16_string_from_system_string(to_string(
        class_int32,
    )));
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
pub fn write_r8(n: f64) {
    print!("{}", n);
}

#[no_mangle]
pub fn write_char(c: u16) {
    print!(
        "{}",
        decode_utf16([c].iter().cloned()).next().unwrap().unwrap()
    );
}

#[no_mangle]
pub fn write_string(system_string: *mut u64) {
    let utf16_string_ptr = unsafe { retrieve_utf16_string_from_system_string(system_string) };
    print!(
        "{}",
        String::from_utf16_lossy(unsafe { &*utf16_string_ptr })
    );
}

#[no_mangle]
pub fn get_length(system_string: *mut u64) -> i32 {
    let string_ptr = unsafe { retrieve_utf16_string_from_system_string(system_string) };
    unsafe { &*string_ptr }.len() as i32
}

#[no_mangle]
pub fn get_chars_i4(system_string: *mut u64, i: i32) -> i32 {
    let string_ptr = unsafe { retrieve_utf16_string_from_system_string(system_string) };
    (unsafe { &*string_ptr })[i as usize] as i32
}

#[no_mangle]
pub unsafe fn object_to_string(_obj: *mut u8) -> *mut u64 {
    new_system_string("X".to_string())
}

#[no_mangle]
pub fn int_to_string(class_int32: *mut u64) -> *mut u64 {
    unsafe {
        let value = *class_int32.offset(1) as i32;
        new_system_string(format!("{}", value))
    }
}

#[no_mangle]
pub fn string_to_string(string: *mut u8) -> *mut u8 {
    string
}

// TODO: Currently use boehm-gc. Replace with better way in the future.
#[link(name = "gc")]
extern "C" {
    fn GC_malloc(len: u32) -> *mut u8;
    fn GC_register_finalizer(obj: *mut u8, f: *mut u8, cd: *mut u8, ofn: *mut u8, ocd: *mut u8);
}

unsafe fn new_utf16_string(s: String) -> *mut Vec<u16> {
    let utf16 = s.encode_utf16().collect::<Vec<u16>>();
    let ptr = GC_malloc(mem::size_of::<Vec<u16>>() as u32);
    ptr::copy_nonoverlapping(&utf16 as *const Vec<u16>, ptr as *mut Vec<u16>, 1);
    mem::forget(utf16);
    GC_register_finalizer(
        ptr,
        finalizer_string as *mut u8,
        0 as *mut u8,
        0 as *mut u8,
        0 as *mut u8,
    );
    ptr as *mut Vec<u16>
}

pub unsafe fn new_utf16_string_from_vec_u16(utf16: Vec<u16>) -> *mut Vec<u16> {
    let ptr = GC_malloc(mem::size_of::<Vec<u16>>() as u32);
    ptr::copy_nonoverlapping(&utf16 as *const Vec<u16>, ptr as *mut Vec<u16>, 1);
    mem::forget(utf16);
    GC_register_finalizer(
        ptr,
        finalizer_string as *mut u8,
        0 as *mut u8,
        0 as *mut u8,
        0 as *mut u8,
    );
    ptr as *mut Vec<u16>
}

#[no_mangle]
fn finalizer_string(obj: *mut Vec<u16>, _cd: *mut u8) {
    unsafe { ptr::drop_in_place(obj) }
}

#[no_mangle]
pub fn memory_alloc(len: u32) -> *mut u8 {
    unsafe { GC_malloc(len) }
}

#[no_mangle]
pub fn new_szarray(elem_sz: u32, len: u32) -> *mut u8 {
    let ptr = unsafe { GC_malloc(elem_sz * len + 8) };
    unsafe { *(ptr as *mut u64) = len as u64 };
    ptr
}
