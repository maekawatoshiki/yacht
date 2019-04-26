use crate::{
    exec::{cfg::*, instruction::*},
    metadata::{class::*, metadata::*, method::*, signature::*},
};
use llvm;
use llvm::{core::*, prelude::*};
use rustc_hash::FxHashMap;
use std::collections::VecDeque;
use std::ffi::c_void;
use std::ffi::CString;
use std::ptr;

macro_rules! cstr0 {
    () => {
        CString::new("").unwrap().as_ptr()
    };
}

pub type CResult<T> = Result<T, Error>;

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    CouldntCompile,
    General,
}

#[derive(Debug, Clone)]
pub enum BasicBlockInfo {
    Positioned(LLVMBasicBlockRef),
    Unpositioned(LLVMBasicBlockRef),
}

#[derive(Debug, Clone)]
pub struct TypedValue {
    pub ty: Type,
    pub val: LLVMValueRef,
}

#[derive(Debug, Clone)]
pub struct PhiStack {
    src_bb: LLVMBasicBlockRef,
    stack: Vec<TypedValue>,
}

#[derive(Debug)]
pub struct JITCompiler<'a> {
    image: &'a mut Image,
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    pass_mgr: LLVMPassManagerRef,
    generating: Option<LLVMValueRef>,
    generated: FxHashMap<u32, LLVMValueRef>, // RVA, Value
    basic_blocks: FxHashMap<usize, BasicBlockInfo>,
    phi_stack: FxHashMap<usize, Vec<PhiStack>>, // destination,
    env: Environment,
    compile_queue: VecDeque<(LLVMValueRef, MethodBody)>,
    builtin_functions: FxHashMap<String, LLVMValueRef>,
    strings: Vec<*mut String>,
    class_types: ClassTypesHolder,
}

#[derive(Debug, Clone)]
pub struct Environment {
    pub arguments: FxHashMap<usize, TypedValue>,
    pub locals: FxHashMap<usize, TypedValue>,
}

#[derive(Debug, Clone)]
pub struct ClassTypesHolder {
    pub map: FxHashMap<String, FxHashMap<String, LLVMTypeRef>>,
}

impl<'a> JITCompiler<'a> {
    pub unsafe fn new(image: &'a mut Image) -> Self {
        llvm::target::LLVM_InitializeNativeTarget();
        llvm::target::LLVM_InitializeNativeAsmPrinter();
        llvm::target::LLVM_InitializeNativeAsmParser();
        llvm::target::LLVM_InitializeAllTargetMCs();
        llvm::execution_engine::LLVMLinkInMCJIT();

        let context = LLVMContextCreate();
        let module =
            LLVMModuleCreateWithNameInContext(CString::new("yacht").unwrap().as_ptr(), context);
        let builder = LLVMCreateBuilderInContext(context);
        let pass_mgr = LLVMCreatePassManager();

        llvm::transforms::scalar::LLVMAddReassociatePass(pass_mgr);
        llvm::transforms::scalar::LLVMAddGVNPass(pass_mgr);
        llvm::transforms::scalar::LLVMAddInstructionCombiningPass(pass_mgr);
        llvm::transforms::scalar::LLVMAddPromoteMemoryToRegisterPass(pass_mgr);
        llvm::transforms::scalar::LLVMAddTailCallEliminationPass(pass_mgr);
        llvm::transforms::scalar::LLVMAddJumpThreadingPass(pass_mgr);

        Self {
            image,
            context,
            module,
            builder,
            pass_mgr,
            generating: None,
            generated: FxHashMap::default(),
            basic_blocks: FxHashMap::default(),
            phi_stack: FxHashMap::default(),
            env: Environment::new(),
            compile_queue: VecDeque::new(),
            strings: vec![],
            class_types: ClassTypesHolder::new(),
            builtin_functions: {
                let mut fs = FxHashMap::default();
                fs.insert(
                    "WriteLine(int)".to_string(),
                    LLVMAddFunction(
                        module,
                        CString::new("WriteLine(int)").unwrap().as_ptr(),
                        LLVMFunctionType(
                            LLVMVoidTypeInContext(context),
                            vec![LLVMInt32TypeInContext(context)].as_mut_ptr(),
                            1,
                            0,
                        ),
                    ),
                );
                fs.insert(
                    "WriteLine(string)".to_string(),
                    LLVMAddFunction(
                        module,
                        CString::new("WriteLine(string)").unwrap().as_ptr(),
                        LLVMFunctionType(
                            LLVMVoidTypeInContext(context),
                            vec![LLVMPointerType(LLVMInt8TypeInContext(context), 0)].as_mut_ptr(),
                            1,
                            0,
                        ),
                    ),
                );
                fs.insert(
                    "WriteLine(char)".to_string(),
                    LLVMAddFunction(
                        module,
                        CString::new("WriteLine(char)").unwrap().as_ptr(),
                        LLVMFunctionType(
                            LLVMVoidTypeInContext(context),
                            vec![LLVMPointerType(LLVMInt8TypeInContext(context), 0)].as_mut_ptr(),
                            1,
                            0,
                        ),
                    ),
                );
                fs.insert(
                    "Write(int)".to_string(),
                    LLVMAddFunction(
                        module,
                        CString::new("Write(int)").unwrap().as_ptr(),
                        LLVMFunctionType(
                            LLVMVoidTypeInContext(context),
                            vec![LLVMInt32TypeInContext(context)].as_mut_ptr(),
                            1,
                            0,
                        ),
                    ),
                );
                fs.insert(
                    "Write(string)".to_string(),
                    LLVMAddFunction(
                        module,
                        CString::new("Write(string)").unwrap().as_ptr(),
                        LLVMFunctionType(
                            LLVMVoidTypeInContext(context),
                            vec![LLVMPointerType(LLVMInt8TypeInContext(context), 0)].as_mut_ptr(),
                            1,
                            0,
                        ),
                    ),
                );
                fs.insert(
                    "Write(char)".to_string(),
                    LLVMAddFunction(
                        module,
                        CString::new("Write(char)").unwrap().as_ptr(),
                        LLVMFunctionType(
                            LLVMVoidTypeInContext(context),
                            vec![LLVMPointerType(LLVMInt8TypeInContext(context), 0)].as_mut_ptr(),
                            1,
                            0,
                        ),
                    ),
                );
                fs.insert(
                    "memory_alloc".to_string(),
                    LLVMAddFunction(
                        module,
                        CString::new("memory_alloc").unwrap().as_ptr(),
                        LLVMFunctionType(
                            LLVMPointerType(LLVMInt8TypeInContext(context), 0),
                            vec![LLVMInt32TypeInContext(context)].as_mut_ptr(),
                            1,
                            0,
                        ),
                    ),
                );
                fs.insert(
                    "get_Length()".to_string(),
                    LLVMAddFunction(
                        module,
                        CString::new("get_Length()").unwrap().as_ptr(),
                        LLVMFunctionType(
                            LLVMInt32TypeInContext(context),
                            vec![LLVMPointerType(LLVMInt8TypeInContext(context), 0)].as_mut_ptr(),
                            1,
                            0,
                        ),
                    ),
                );
                fs.insert(
                    "get_Chars(int32)".to_string(),
                    LLVMAddFunction(
                        module,
                        CString::new("get_Chars(int32)").unwrap().as_ptr(),
                        LLVMFunctionType(
                            LLVMInt32TypeInContext(context),
                            vec![
                                LLVMPointerType(LLVMInt8TypeInContext(context), 0),
                                LLVMInt32TypeInContext(context),
                            ]
                            .as_mut_ptr(),
                            2,
                            0,
                        ),
                    ),
                );
                fs
            },
        }
    }

    pub unsafe fn run_main(&mut self, main: LLVMValueRef) {
        let mut ee = 0 as llvm::execution_engine::LLVMExecutionEngineRef;
        let mut error = 0 as *mut i8;
        if llvm::execution_engine::LLVMCreateExecutionEngineForModule(
            &mut ee,
            self.module,
            &mut error,
        ) != 0
        {
            panic!("llvm error: failed to initialize execute engine")
        }

        for (name, f) in [
            ("WriteLine(int)", write_line_int as *mut c_void),
            ("WriteLine(string)", write_line_string as *mut c_void),
            ("WriteLine(char)", write_line_char as *mut c_void),
            ("Write(int)", write_int as *mut c_void),
            ("Write(string)", write_string as *mut c_void),
            ("Write(char)", write_char as *mut c_void),
            ("get_Length()", get_length as *mut c_void),
            ("get_Chars(int32)", get_chars as *mut c_void),
            ("memory_alloc", memory_alloc as *mut c_void),
        ]
        .iter()
        {
            let llvm_f = *self.builtin_functions.get(*name).unwrap();
            llvm::execution_engine::LLVMAddGlobalMapping(ee, llvm_f, *f);
        }

        llvm::execution_engine::LLVMRunFunction(ee, main, 0, vec![].as_mut_ptr());
    }

    pub unsafe fn generate_main(&mut self, method: &MethodBody) -> LLVMValueRef {
        self.basic_blocks.clear();
        self.phi_stack.clear();
        self.env = Environment::new();

        let mut basic_blocks = CFGMaker::new().make_basic_blocks(&method.body);
        let (ret_ty, mut params_ty): (LLVMTypeRef, Vec<LLVMTypeRef>) =
            { (Type::new(ElementType::Void).to_llvmty(self), vec![]) };
        let func_ty = LLVMFunctionType(ret_ty, params_ty.as_mut_ptr(), params_ty.len() as u32, 0);
        let func_name = format!("yacht-Main");
        let func = LLVMAddFunction(
            self.module,
            CString::new(func_name.as_str()).unwrap().as_ptr(),
            func_ty,
        );

        self.generating = Some(func);

        let bb_entry = LLVMAppendBasicBlockInContext(
            self.context,
            func,
            CString::new("entry").unwrap().as_ptr(),
        );

        LLVMPositionBuilderAtEnd(self.builder, bb_entry);

        // Declare locals
        for (i, ty) in method.locals_ty.iter().enumerate() {
            self.get_local(i, Some(&ty));
        }

        self.basic_blocks
            .insert(0, BasicBlockInfo::Positioned(bb_entry));

        for block in &basic_blocks {
            if block.start > 0 {
                self.basic_blocks.insert(
                    block.start,
                    BasicBlockInfo::Unpositioned(LLVMAppendBasicBlock(func, cstr0!())),
                );
            }
        }

        for i in 0..basic_blocks.len() {
            self.compile_block(&mut basic_blocks, i, vec![]).unwrap();
        }

        let last_block = basic_blocks.last().unwrap();
        let bb_last = (*self.basic_blocks.get(&last_block.start).unwrap()).retrieve();
        LLVMPositionBuilderAtEnd(self.builder, bb_last);
        if cur_bb_has_no_terminator(self.builder) {
            LLVMBuildRetVoid(self.builder);
        }

        let mut iter_bb = LLVMGetFirstBasicBlock(func);
        while iter_bb != ptr::null_mut() {
            if LLVMIsATerminatorInst(LLVMGetLastInstruction(iter_bb)) == ptr::null_mut() {
                let terminator_builder = LLVMCreateBuilderInContext(self.context);
                LLVMPositionBuilderAtEnd(terminator_builder, iter_bb);
                LLVMBuildRetVoid(self.builder);
            }
            iter_bb = LLVMGetNextBasicBlock(iter_bb);
        }

        while let Some((func, method)) = self.compile_queue.pop_front() {
            self.generate_func(func, &method);
        }

        when_debug!(LLVMDumpModule(self.module));

        llvm::analysis::LLVMVerifyFunction(
            func,
            llvm::analysis::LLVMVerifierFailureAction::LLVMAbortProcessAction,
        );

        LLVMRunPassManager(self.pass_mgr, self.module);

        func
    }

    unsafe fn generate_func(&mut self, func: LLVMValueRef, method: &MethodBody) {
        self.generating = Some(func);
        self.env = Environment::new();

        let method_ty = method.ty.as_fnptr().unwrap();
        let mut basic_blocks = CFGMaker::new().make_basic_blocks(&method.body);
        let ret_ty = LLVMGetElementType(LLVMGetReturnType(LLVMTypeOf(func)));
        let bb_entry = LLVMAppendBasicBlockInContext(
            self.context,
            func,
            CString::new("entry").unwrap().as_ptr(),
        );

        LLVMPositionBuilderAtEnd(self.builder, bb_entry);

        let shift = if method_ty.has_this() {
            LLVMBuildStore(
                self.builder,
                LLVMGetParam(func, 0),
                self.get_argument(
                    0,
                    Some(&Type::new(ElementType::Class(method.class.clone()))),
                ),
            );
            1
        } else {
            0
        };
        for (i, ty) in method_ty.params.iter().enumerate() {
            LLVMBuildStore(
                self.builder,
                LLVMGetParam(func, (i + shift) as u32),
                self.get_argument(i + shift, Some(&ty)),
            );
        }

        // Declare locals
        for (i, ty) in method.locals_ty.iter().enumerate() {
            self.get_local(i, Some(&ty));
        }

        self.basic_blocks
            .insert(0, BasicBlockInfo::Positioned(bb_entry));

        for block in &basic_blocks {
            if block.start > 0 {
                self.basic_blocks.insert(
                    block.start,
                    BasicBlockInfo::Unpositioned(LLVMAppendBasicBlock(func, cstr0!())),
                );
            }
        }

        for i in 0..basic_blocks.len() {
            self.compile_block(&mut basic_blocks, i, vec![]).unwrap();
        }

        let last_block = basic_blocks.last().unwrap();
        let bb_last = (*self.basic_blocks.get(&last_block.start).unwrap()).retrieve();
        LLVMPositionBuilderAtEnd(self.builder, bb_last);
        if cur_bb_has_no_terminator(self.builder) {
            if LLVMGetTypeKind(ret_ty) == llvm::LLVMTypeKind::LLVMVoidTypeKind {
                LLVMBuildRetVoid(self.builder);
            } else {
                LLVMBuildRet(self.builder, LLVMConstNull(ret_ty));
            }
        }

        let mut iter_bb = LLVMGetFirstBasicBlock(func);
        while iter_bb != ptr::null_mut() {
            if LLVMIsATerminatorInst(LLVMGetLastInstruction(iter_bb)) == ptr::null_mut() {
                let terminator_builder = LLVMCreateBuilderInContext(self.context);
                LLVMPositionBuilderAtEnd(terminator_builder, iter_bb);
                if LLVMGetTypeKind(ret_ty) == llvm::LLVMTypeKind::LLVMVoidTypeKind {
                    LLVMBuildRetVoid(self.builder);
                } else {
                    LLVMBuildRet(terminator_builder, LLVMConstNull(ret_ty));
                }
            }
            iter_bb = LLVMGetNextBasicBlock(iter_bb);
        }

        self.basic_blocks.clear();
        self.phi_stack.clear();
    }

    unsafe fn get_local_ty(&mut self, id: usize) -> Type {
        self.env.locals.get(&id).unwrap().ty.clone()
    }

    unsafe fn get_argument_ty(&mut self, id: usize) -> Type {
        self.env.arguments.get(&id).unwrap().ty.clone()
    }

    unsafe fn get_local(&mut self, id: usize, ty: Option<&Type>) -> LLVMValueRef {
        if let Some(v) = self.env.locals.get(&id) {
            return v.val;
        }

        let func = self.generating.unwrap();
        let builder = LLVMCreateBuilderInContext(self.context);
        let entry_bb = LLVMGetEntryBasicBlock(func);
        let first_inst = LLVMGetFirstInstruction(entry_bb);
        // A variable must be declared at the first point of entry block
        if first_inst == ptr::null_mut() {
            LLVMPositionBuilderAtEnd(builder, entry_bb);
        } else {
            LLVMPositionBuilderBefore(builder, first_inst);
        }

        let var = LLVMBuildAlloca(builder, ty.unwrap().to_llvmty(self), cstr0!());

        self.env
            .locals
            .insert(id, TypedValue::new(ty.unwrap().clone(), var));
        var
    }

    unsafe fn get_argument(&mut self, id: usize, ty: Option<&Type>) -> LLVMValueRef {
        if let Some(v) = self.env.arguments.get(&id) {
            return v.val;
        }

        let func = self.generating.unwrap();
        let builder = LLVMCreateBuilderInContext(self.context);
        let entry_bb = LLVMGetEntryBasicBlock(func);
        let first_inst = LLVMGetFirstInstruction(entry_bb);
        // A variable is always declared at the first point of entry block
        if first_inst == ptr::null_mut() {
            LLVMPositionBuilderAtEnd(builder, entry_bb);
        } else {
            LLVMPositionBuilderBefore(builder, first_inst);
        }

        let var = LLVMBuildAlloca(builder, ty.unwrap().to_llvmty(self), cstr0!());

        self.env
            .arguments
            .insert(id, TypedValue::new(ty.unwrap().clone(), var));
        var
    }

    // Returns destination
    unsafe fn compile_block(
        &mut self,
        blocks: &mut [BasicBlock],
        idx: usize,
        init_stack: Vec<TypedValue>,
    ) -> CResult<usize> {
        #[rustfmt::skip]
        macro_rules! cur_block { () => {{ &blocks[idx] }}; };
        #[rustfmt::skip]
        macro_rules! cur_block_mut { () => {{ &mut blocks[idx] }}; };

        fn find_block(start: usize, blocks: &[BasicBlock]) -> Option<usize> {
            blocks.iter().enumerate().find_map(
                |(i, block)| {
                    if block.start == start {
                        Some(i)
                    } else {
                        None
                    }
                },
            )
        }

        if cur_block!().generated {
            return Ok(0);
        }

        cur_block_mut!().generated = true;

        let bb = self.basic_blocks.get_mut(&cur_block!().start).unwrap();
        LLVMPositionBuilderAtEnd(self.builder, bb.set_positioned().retrieve());

        let phi_stack = self.build_phi_stack(cur_block!().start, init_stack);
        let stack = self.compile_bytecode(cur_block!(), phi_stack)?;

        match &cur_block!().kind {
            BrKind::ConditionalJmp { destinations } => {
                let mut d = 0;
                for dst in destinations.clone() {
                    if let Some(i) = find_block(dst, blocks) {
                        d = self.compile_block(blocks, i, stack.clone())?;
                    } else {
                        continue;
                    };
                    // TODO: All ``d`` must be the same
                }
                Ok(d)
            }
            BrKind::UnconditionalJmp { destination } => {
                let src_bb = self.get_basic_block(cur_block!().start).retrieve();
                self.phi_stack
                    .entry(*destination)
                    .or_insert(vec![])
                    .push(PhiStack { src_bb, stack });
                Ok(*destination)
            }
            BrKind::JmpRequired { destination } => {
                let src_bb = self.get_basic_block(cur_block!().start).retrieve();
                if cur_bb_has_no_terminator(self.builder) {
                    let bb = self
                        .get_basic_block(*destination)
                        .set_positioned()
                        .retrieve();
                    LLVMBuildBr(self.builder, bb);
                }
                self.phi_stack
                    .entry(*destination)
                    .or_insert(vec![])
                    .push(PhiStack { src_bb, stack });
                Ok(*destination)
            }
            _ => Ok(0),
        }
    }

    unsafe fn build_phi_stack(
        &mut self,
        start: usize,
        mut stack: Vec<TypedValue>,
    ) -> Vec<TypedValue> {
        if let Some(phi_stacks) = self.phi_stack.get(&start) {
            let init_stack_size = stack.len();

            // Firstly, build llvm phi which needs a type of all conceivable values.
            let src_bb = phi_stacks[0].src_bb;
            for TypedValue { val, ty } in &phi_stacks[0].stack {
                let phi = LLVMBuildPhi(self.builder, LLVMTypeOf(*val), cstr0!());
                LLVMAddIncoming(phi, vec![*val].as_mut_ptr(), vec![src_bb].as_mut_ptr(), 1);
                stack.push(TypedValue::new(ty.clone(), phi));
            }

            for phi_stack in &phi_stacks[1..] {
                let src_bb = phi_stack.src_bb;
                for (i, TypedValue { val, .. }) in (&phi_stack.stack).iter().enumerate() {
                    let phi = stack[init_stack_size + i].val;
                    LLVMAddIncoming(phi, vec![*val].as_mut_ptr(), vec![src_bb].as_mut_ptr(), 1);
                }
            }
        }

        stack
    }

    unsafe fn compile_bytecode(
        &mut self,
        block: &BasicBlock,
        mut stack: Vec<TypedValue>,
    ) -> CResult<Vec<TypedValue>> {
        #[rustfmt::skip]
        macro_rules! binop { ($op:ident) => {{
            let val2 = stack.pop().unwrap();
            let val1 = stack.pop().unwrap();
            stack.push(TypedValue::new(
                val1.ty,
                concat_idents!(LLVMBuild, $op)(self.builder, val1.val, val2.val, cstr0!()),
            ));
        }}}
        #[rustfmt::skip]
        macro_rules! push_i4 { ($n:expr) => {
             stack.push(TypedValue::new(
                Type::i4_ty(), llvm_const_int32(self.context, $n as u64),
            ))
        }}
        #[rustfmt::skip]
        macro_rules! ldloc { ($n:expr) => {
            stack.push(TypedValue::new(
                self.get_local_ty($n),
                LLVMBuildLoad(self.builder, self.get_local($n, None), cstr0!()),
            ))
        }}
        #[rustfmt::skip]
        macro_rules! stloc { ($n:expr) => {{
            LLVMBuildStore(
                self.builder, stack.pop().unwrap().val, self.get_local($n, None));
        }}; }
        #[rustfmt::skip]
        macro_rules! ldarg { ($n:expr) => {
            stack.push(TypedValue::new(
                self.get_argument_ty($n),
                LLVMBuildLoad(self.builder, self.get_argument($n, None), cstr0!()),
            ))
        }}

        let code = &block.code;

        for instr in code {
            match instr {
                Instruction::Ldstr { us_offset } => stack.push(TypedValue::new(
                    Type::string_ty(),
                    llvm_const_ptr(self.context, {
                        let s = String::from_utf16_lossy(
                            self.image.metadata.user_strings.get(&us_offset).unwrap(),
                        );
                        self.strings.push(Box::into_raw(Box::new(s)));
                        *self.strings.last().unwrap() as *mut u64
                    }),
                )),
                Instruction::Ldc_I4_0 => push_i4!(0),
                Instruction::Ldc_I4_1 => push_i4!(1),
                Instruction::Ldc_I4_2 => push_i4!(2),
                Instruction::Ldc_I4_3 => push_i4!(3),
                Instruction::Ldc_I4_S { n } => push_i4!(*n),
                Instruction::Ldc_I4 { n } => push_i4!(*n),
                Instruction::Pop => {
                    stack.pop();
                }
                Instruction::Dup => {
                    stack.push(stack.last().unwrap().clone());
                }
                Instruction::Call { table, entry } => {
                    self.gen_instr_call(&mut stack, *table, *entry)
                }
                Instruction::CallVirt { table, entry } => {
                    self.gen_instr_callvirt(&mut stack, *table, *entry)
                }
                Instruction::Newobj { table, entry } => {
                    self.gen_instr_newobj(&mut stack, *table, *entry)
                }
                Instruction::Ldloc_0 => ldloc!(0),
                Instruction::Ldloc_1 => ldloc!(1),
                Instruction::Ldloc_2 => ldloc!(2),
                Instruction::Ldfld { table, entry } => {
                    self.gen_instr_ldfld(&mut stack, *table, *entry)
                }
                Instruction::Stloc_0 => stloc!(0),
                Instruction::Stloc_1 => stloc!(1),
                Instruction::Stloc_2 => stloc!(2),
                Instruction::Stfld { table, entry } => {
                    self.gen_instr_stfld(&mut stack, *table, *entry)
                }
                Instruction::Ldarg_0 => ldarg!(0),
                Instruction::Ldarg_1 => ldarg!(1),
                Instruction::Ldarg_2 => ldarg!(2),
                Instruction::Add => binop!(Add),
                Instruction::Sub => binop!(Sub),
                Instruction::Mul => binop!(Mul),
                Instruction::Rem => binop!(SRem),
                Instruction::Ret => {
                    let ret_ty =
                        LLVMGetElementType(LLVMGetReturnType(LLVMTypeOf(self.generating.unwrap())));
                    if LLVMGetTypeKind(ret_ty) == llvm::LLVMTypeKind::LLVMVoidTypeKind {
                        LLVMBuildRetVoid(self.builder);
                    } else {
                        let val = stack.pop().unwrap().val;
                        LLVMBuildRet(self.builder, self.typecast(val, ret_ty));
                    }
                }
                Instruction::Brfalse { .. } | Instruction::Brtrue { .. } => {
                    let val1 = stack.pop().unwrap();
                    let cond_val = LLVMBuildICmp(
                        self.builder,
                        match instr {
                            Instruction::Brfalse { .. } => llvm::LLVMIntPredicate::LLVMIntEQ,
                            Instruction::Brtrue { .. } => llvm::LLVMIntPredicate::LLVMIntNE,
                            _ => unreachable!(),
                        },
                        val1.val,
                        LLVMConstNull(LLVMTypeOf(val1.val)),
                        cstr0!(),
                    );
                    let destinations = block.kind.get_conditional_jump_destinations();
                    let bb_then = self.get_basic_block(destinations[0]).retrieve();
                    let bb_else = self.get_basic_block(destinations[1]).retrieve();
                    LLVMBuildCondBr(self.builder, cond_val, bb_then, bb_else);
                }
                Instruction::Bge { .. }
                | Instruction::Blt { .. }
                | Instruction::Ble { .. }
                | Instruction::Beq { .. }
                | Instruction::Bne_un { .. }
                | Instruction::Bgt { .. } => {
                    let val2 = stack.pop().unwrap();
                    let val1 = stack.pop().unwrap();
                    let cond_val = LLVMBuildICmp(
                        self.builder,
                        match instr {
                            Instruction::Bge { .. } => llvm::LLVMIntPredicate::LLVMIntSGE,
                            Instruction::Blt { .. } => llvm::LLVMIntPredicate::LLVMIntSLT,
                            Instruction::Ble { .. } => llvm::LLVMIntPredicate::LLVMIntSLE,
                            Instruction::Bgt { .. } => llvm::LLVMIntPredicate::LLVMIntSGT,
                            Instruction::Beq { .. } => llvm::LLVMIntPredicate::LLVMIntEQ,
                            Instruction::Bne_un { .. } => llvm::LLVMIntPredicate::LLVMIntNE,
                            _ => unreachable!(),
                        },
                        val1.val,
                        val2.val,
                        cstr0!(),
                    );
                    let destinations = block.kind.get_conditional_jump_destinations();
                    let bb_then = self.get_basic_block(destinations[0]).retrieve();
                    let bb_else = self.get_basic_block(destinations[1]).retrieve();
                    LLVMBuildCondBr(self.builder, cond_val, bb_then, bb_else);
                }
                Instruction::Br { .. } => {
                    let destination = block.kind.get_unconditional_jump_destination();
                    let bb_br = self.get_basic_block(destination).retrieve();
                    if cur_bb_has_no_terminator(self.builder) {
                        LLVMBuildBr(self.builder, bb_br);
                    }
                }
                Instruction::Clt | Instruction::Ceq => {
                    let val2 = stack.pop().unwrap();
                    let val1 = stack.pop().unwrap();
                    let cond_val = self.typecast(
                        LLVMBuildICmp(
                            self.builder,
                            match instr {
                                Instruction::Clt { .. } => llvm::LLVMIntPredicate::LLVMIntSLT,
                                Instruction::Ceq { .. } => llvm::LLVMIntPredicate::LLVMIntEQ,
                                _ => unreachable!(),
                            },
                            val1.val,
                            val2.val,
                            cstr0!(),
                        ),
                        LLVMInt32TypeInContext(self.context),
                    );
                    stack.push(TypedValue::new(Type::i4_ty(), cond_val));
                }
            }
        }

        Ok(stack)
    }

    unsafe fn gen_instr_call(&mut self, stack: &mut Vec<TypedValue>, table: usize, entry: usize) {
        let table = self.image.metadata.metadata_stream.tables[table][entry - 1];
        match table {
            Table::MemberRef(mrt) => {
                let (table, entry) = mrt.class_table_and_entry();
                let class = &self.image.metadata.metadata_stream.tables[table][entry - 1];
                match class {
                    Table::TypeRef(trt) => {
                        let (table, entry) = trt.resolution_scope_table_and_entry();
                        let art = match self.image.metadata.metadata_stream.tables[table][entry - 1]
                        {
                            Table::AssemblyRef(art) => art,
                            _ => unimplemented!(),
                        };
                        let ar_name = self.image.get_string(art.name);
                        let ty_namespace = self.image.get_string(trt.type_namespace);
                        let ty_name = self.image.get_string(trt.type_name);
                        let name = self.image.get_string(mrt.name);
                        let sig = self
                            .image
                            .metadata
                            .blob
                            .get(&(mrt.signature as u32))
                            .unwrap();
                        let ty = SignatureParser::new(sig)
                            .parse_method_ref_sig(self.image)
                            .unwrap();

                        if ar_name == "mscorlib" && ty_namespace == "System" && ty_name == "Console"
                        {
                            if name == "WriteLine" {
                                let val = stack.pop().unwrap();
                                if ty.equal_method(ElementType::Void, &[ElementType::String]) {
                                    self.call_function(
                                        *self.builtin_functions.get("WriteLine(string)").unwrap(),
                                        vec![val.val],
                                    );
                                } else if ty.equal_method(ElementType::Void, &[ElementType::I4]) {
                                    self.call_function(
                                        *self.builtin_functions.get("WriteLine(int)").unwrap(),
                                        vec![val.val],
                                    );
                                } else if ty.equal_method(ElementType::Void, &[ElementType::Char]) {
                                    self.call_function(
                                        *self.builtin_functions.get("WriteLine(char)").unwrap(),
                                        vec![val.val],
                                    );
                                }
                            } else if name == "Write" {
                                let val = stack.pop().unwrap();
                                if ty.equal_method(ElementType::Void, &[ElementType::String]) {
                                    self.call_function(
                                        *self.builtin_functions.get("Write(string)").unwrap(),
                                        vec![val.val],
                                    );
                                } else if ty.equal_method(ElementType::Void, &[ElementType::I4]) {
                                    self.call_function(
                                        *self.builtin_functions.get("Write(int)").unwrap(),
                                        vec![val.val],
                                    );
                                } else if ty.equal_method(ElementType::Void, &[ElementType::Char]) {
                                    self.call_function(
                                        *self.builtin_functions.get("Write(char)").unwrap(),
                                        vec![val.val],
                                    );
                                }
                            }
                        }
                    }
                    _ => unimplemented!(),
                }
            }
            Table::MethodDef(mdt) => {
                let method = self.image.get_method(mdt.rva);
                let method_sig = method.ty.as_fnptr().unwrap();
                let has_this = if method_sig.has_this() { 1 } else { 0 };
                let params = stack
                    .drain(stack.len() - method_sig.params.len() - has_this..)
                    .map(|tv| tv.val)
                    .collect();
                let func = if let Some(llvm_func) = self.generated.get(&mdt.rva) {
                    *llvm_func
                } else {
                    let method = self.image.get_method(mdt.rva);
                    let ret_ty = method_sig.ret.to_llvmty(self);
                    let mut params_ty = if method_sig.has_this() {
                        vec![self.get_class_type(&method.class.borrow())]
                    } else {
                        vec![]
                    };
                    params_ty.append(
                        &mut method_sig
                            .params
                            .iter()
                            .map(|ty| ty.to_llvmty(self))
                            .collect::<Vec<LLVMTypeRef>>(),
                    );
                    let func_ty =
                        LLVMFunctionType(ret_ty, params_ty.as_mut_ptr(), params_ty.len() as u32, 0);
                    let func = LLVMAddFunction(
                        self.module,
                        CString::new(method.name.as_str()).unwrap().as_ptr(),
                        func_ty,
                    );
                    self.generated.insert(mdt.rva, func);
                    self.compile_queue.push_back((func, method.clone()));
                    func
                };
                let ret = self.call_function(func, params);
                if !method_sig.ret.is_void() {
                    stack.push(TypedValue::new(method_sig.ret.clone(), ret))
                }
            }
            e => unimplemented!("call: unimplemented: {:?}", e),
        }
    }

    unsafe fn gen_instr_callvirt(
        &mut self,
        stack: &mut Vec<TypedValue>,
        table: usize,
        entry: usize,
    ) {
        let table = self.image.metadata.metadata_stream.tables[table][entry - 1];
        match table {
            Table::MemberRef(mrt) => {
                let (table, entry) = mrt.class_table_and_entry();
                let class = &self.image.metadata.metadata_stream.tables[table][entry - 1];
                match class {
                    Table::TypeRef(trt) => {
                        let (table, entry) = trt.resolution_scope_table_and_entry();
                        let art = match self.image.metadata.metadata_stream.tables[table][entry - 1]
                        {
                            Table::AssemblyRef(art) => art,
                            _ => unimplemented!(),
                        };
                        let ar_name = self.image.get_string(art.name);
                        let ty_namespace = self.image.get_string(trt.type_namespace);
                        let ty_name = self.image.get_string(trt.type_name);
                        let name = self.image.get_string(mrt.name);

                        if ar_name == "mscorlib" && ty_namespace == "System" && ty_name == "String"
                        {
                            match name.as_str() {
                                "get_Length" => {
                                    let val = stack.pop().unwrap();
                                    stack.push(TypedValue::new(
                                        Type::i4_ty(),
                                        self.call_function(
                                            *self.builtin_functions.get("get_Length()").unwrap(),
                                            vec![val.val],
                                        ),
                                    ));
                                }
                                "get_Chars" => {
                                    let idx = stack.pop().unwrap();
                                    let val = stack.pop().unwrap();
                                    stack.push(TypedValue::new(
                                        Type::i4_ty(),
                                        self.call_function(
                                            *self
                                                .builtin_functions
                                                .get("get_Chars(int32)")
                                                .unwrap(),
                                            vec![val.val, idx.val],
                                        ),
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
                let method = self.image.get_method(mdt.rva);
                let method_sig = method.ty.as_fnptr().unwrap();
                assert!(method_sig.has_this());
                let params = stack
                    .drain(stack.len() - method_sig.params.len() - /*this(obj)=*/1..)
                    .map(|tv| tv.val)
                    .collect();
                let func = if let Some(llvm_func) = self.generated.get(&mdt.rva) {
                    *llvm_func
                } else {
                    let method = self.image.get_method(mdt.rva);
                    let ret_ty = method_sig.ret.to_llvmty(self);
                    let mut params_ty = vec![self.get_class_type(&method.class.borrow())];
                    params_ty.append(
                        &mut method_sig
                            .params
                            .iter()
                            .map(|ty| ty.to_llvmty(self))
                            .collect::<Vec<LLVMTypeRef>>(),
                    );
                    let func_ty =
                        LLVMFunctionType(ret_ty, params_ty.as_mut_ptr(), params_ty.len() as u32, 0);
                    let func = LLVMAddFunction(
                        self.module,
                        CString::new(method.name.as_str()).unwrap().as_ptr(),
                        func_ty,
                    );
                    self.generated.insert(mdt.rva, func);
                    self.compile_queue.push_back((func, method.clone()));
                    func
                };
                let ret = self.call_function(func, params);
                if !method_sig.ret.is_void() {
                    stack.push(TypedValue::new(method_sig.ret.clone(), ret))
                }
            }
            e => unimplemented!("call: unimplemented: {:?}", e),
        }
    }

    unsafe fn gen_instr_stfld(&mut self, stack: &mut Vec<TypedValue>, table: usize, entry: usize) {
        let val = stack.pop().unwrap();
        let obj = stack.pop().unwrap();
        let table = &self.image.metadata.metadata_stream.tables[table][entry - 1];
        match table {
            Table::Field(f) => {
                let name = self.image.get_string(f.name);
                let class = &obj.ty.as_class().unwrap().borrow();
                let idx = class.fields.iter().position(|f| &f.name == name).unwrap();
                let gep = LLVMBuildGEP(
                    self.builder,
                    obj.val,
                    vec![
                        llvm_const_int32(self.context, 0),
                        llvm_const_int32(self.context, idx as u64),
                    ]
                    .as_mut_ptr(),
                    2,
                    cstr0!(),
                );
                LLVMBuildStore(self.builder, val.val, gep);
            }
            e => unimplemented!("{:?}", e),
        }
    }

    unsafe fn gen_instr_ldfld(&mut self, stack: &mut Vec<TypedValue>, table: usize, entry: usize) {
        let obj = stack.pop().unwrap();
        let table = &self.image.metadata.metadata_stream.tables[table][entry - 1];
        match table {
            Table::Field(f) => {
                let name = self.image.get_string(f.name);
                let class = &obj.ty.as_class().unwrap().borrow();
                let (idx, ty) = class
                    .fields
                    .iter()
                    .enumerate()
                    .find(|(_, f)| &f.name == name)
                    .map(|(i, f)| (i, f.ty.clone()))
                    .unwrap();
                stack.push(TypedValue::new(ty, {
                    let gep = LLVMBuildGEP(
                        self.builder,
                        obj.val,
                        vec![
                            llvm_const_int32(self.context, 0),
                            llvm_const_int32(self.context, idx as u64),
                        ]
                        .as_mut_ptr(),
                        2,
                        cstr0!(),
                    );
                    LLVMBuildLoad(self.builder, gep, cstr0!())
                }));
            }
            e => unimplemented!("{:?}", e),
        }
    }

    unsafe fn gen_instr_newobj(&mut self, stack: &mut Vec<TypedValue>, table: usize, entry: usize) {
        let table = self.image.metadata.metadata_stream.tables[table][entry - 1];
        match table {
            Table::MemberRef(_mrt) => {} // TODO
            Table::MethodDef(mdt) => {
                let method = self.image.get_method(mdt.rva);
                let method_sig = method.ty.as_fnptr().unwrap();
                assert!(method_sig.has_this());
                let llvm_class_ty = self.get_class_type(&method.class.borrow());
                let mut params_ty = vec![llvm_class_ty];
                let new_obj = LLVMBuildTruncOrBitCast(
                    self.builder,
                    self.call_function(
                        *self.builtin_functions.get("memory_alloc").unwrap(),
                        vec![self.get_size_of_llvm_class_type(llvm_class_ty)],
                    ),
                    llvm_class_ty,
                    cstr0!(),
                );
                let mut params = vec![new_obj];
                params.append(
                    &mut stack
                        .drain(stack.len() - method_sig.params.len()..)
                        .map(|tv| tv.val)
                        .collect(),
                );
                let func = if let Some(llvm_func) = self.generated.get(&mdt.rva) {
                    *llvm_func
                } else {
                    let method = self.image.get_method(mdt.rva);
                    let ret_ty = method_sig.ret.to_llvmty(self);
                    params_ty.append(
                        &mut method_sig
                            .params
                            .iter()
                            .map(|ty| ty.to_llvmty(self))
                            .collect::<Vec<LLVMTypeRef>>(),
                    );
                    let func_ty =
                        LLVMFunctionType(ret_ty, params_ty.as_mut_ptr(), params_ty.len() as u32, 0);
                    let func = LLVMAddFunction(
                        self.module,
                        CString::new(method.name.as_str()).unwrap().as_ptr(),
                        func_ty,
                    );
                    self.generated.insert(mdt.rva, func);
                    self.compile_queue.push_back((func, method.clone()));
                    func
                };
                self.call_function(func, params);
                stack.push(TypedValue::new(
                    Type::new(ElementType::Class(method.class)),
                    new_obj,
                ))
            }
            e => unimplemented!("call: unimplemented: {:?}", e),
        }
    }

    unsafe fn call_function(
        &self,
        callee: LLVMValueRef,
        mut args: Vec<LLVMValueRef>,
    ) -> LLVMValueRef {
        LLVMBuildCall(
            self.builder,
            callee,
            args.as_mut_ptr(),
            args.len() as u32,
            cstr0!(),
        )
    }

    unsafe fn get_basic_block(&mut self, pc: usize) -> &mut BasicBlockInfo {
        let func = self.generating.unwrap();
        self.basic_blocks
            .entry(pc)
            .or_insert_with(|| BasicBlockInfo::Unpositioned(LLVMAppendBasicBlock(func, cstr0!())))
    }

    unsafe fn get_class_type(&mut self, class: &ClassInfo) -> LLVMTypeRef {
        if let Some(ty) = self.class_types.get(class) {
            return ty;
        }
        let class_ty = LLVMStructCreateNamed(
            self.context,
            CString::new(class.name.as_str()).unwrap().as_ptr(),
        );
        let class_ptr_ty = LLVMPointerType(class_ty, 0);
        self.class_types.add(class, class_ptr_ty);
        let mut fields_ty = class
            .fields
            .iter()
            .map(|ClassField { ty, .. }| ty.to_llvmty(self))
            .collect::<Vec<LLVMTypeRef>>();
        LLVMStructSetBody(class_ty, fields_ty.as_mut_ptr(), fields_ty.len() as u32, 0);
        class_ptr_ty
    }

    unsafe fn get_size_of_llvm_class_type(&self, class: LLVMTypeRef) -> LLVMValueRef {
        LLVMConstPtrToInt(
            LLVMConstGEP(
                LLVMConstNull(class),
                vec![llvm_const_int32(self.context, 1)].as_mut_ptr(),
                1,
            ),
            LLVMInt32TypeInContext(self.context),
        )
    }

    unsafe fn typecast(&self, val: LLVMValueRef, to: LLVMTypeRef) -> LLVMValueRef {
        let v_ty = LLVMTypeOf(val);

        if matches!(LLVMGetTypeKind(to), llvm::LLVMTypeKind::LLVMVoidTypeKind) {
            return val;
        }

        match LLVMGetTypeKind(v_ty) {
            llvm::LLVMTypeKind::LLVMIntegerTypeKind => match LLVMGetTypeKind(to) {
                llvm::LLVMTypeKind::LLVMIntegerTypeKind => {
                    let val_bw = LLVMGetIntTypeWidth(v_ty);
                    let to_bw = LLVMGetIntTypeWidth(to);
                    if val_bw < to_bw {
                        return LLVMBuildZExtOrBitCast(self.builder, val, to, cstr0!());
                    }
                }
                llvm::LLVMTypeKind::LLVMDoubleTypeKind => {
                    return LLVMBuildSIToFP(self.builder, val, to, cstr0!());
                }
                _ => {}
            },
            llvm::LLVMTypeKind::LLVMDoubleTypeKind | llvm::LLVMTypeKind::LLVMFloatTypeKind => {
                return LLVMBuildFPToSI(self.builder, val, to, cstr0!());
            }
            llvm::LLVMTypeKind::LLVMVoidTypeKind => return val,
            llvm::LLVMTypeKind::LLVMPointerTypeKind => match LLVMGetTypeKind(to) {
                llvm::LLVMTypeKind::LLVMIntegerTypeKind => {
                    return LLVMBuildPtrToInt(self.builder, val, to, cstr0!());
                }
                _ => {}
            },
            _ => {}
        }
        LLVMBuildTruncOrBitCast(self.builder, val, to, cstr0!())
    }
}

unsafe fn cur_bb_has_no_terminator(builder: LLVMBuilderRef) -> bool {
    LLVMIsATerminatorInst(LLVMGetLastInstruction(LLVMGetInsertBlock(builder))) == ptr::null_mut()
}

pub trait CastIntoLLVMType {
    unsafe fn to_llvmty<'a>(&self, compiler: &mut JITCompiler<'a>) -> LLVMTypeRef;
}

impl CastIntoLLVMType for Type {
    unsafe fn to_llvmty<'a>(&self, compiler: &mut JITCompiler<'a>) -> LLVMTypeRef {
        let ctx = compiler.context;
        match self.base {
            ElementType::Void => LLVMVoidTypeInContext(ctx),
            ElementType::Boolean => LLVMInt8TypeInContext(ctx),
            ElementType::Char => LLVMInt32TypeInContext(ctx),
            ElementType::I4 => LLVMInt32TypeInContext(ctx),
            ElementType::String => LLVMPointerType(LLVMInt8TypeInContext(ctx), 0),
            ElementType::Class(ref class) => {
                let class = &class.borrow();
                compiler.get_class_type(class)
            }
            _ => unimplemented!(),
            // &VariableType::Int => LLVMInt32TypeInContext(ctx),
            // &VariableType::Double => LLVMDoubleTypeInContext(ctx),
            // &VariableType::Void => LLVMVoidTypeInContext(ctx),
            // &VariableType::Pointer => LLVMPointerType(LLVMInt8TypeInContext(ctx), 0),
            // &VariableType::Long => unimplemented!(),
        }
    }
}

impl BasicBlockInfo {
    pub fn retrieve(&self) -> LLVMBasicBlockRef {
        match self {
            BasicBlockInfo::Positioned(bb) | BasicBlockInfo::Unpositioned(bb) => *bb,
        }
    }

    pub fn set_positioned(&mut self) -> &Self {
        match self {
            BasicBlockInfo::Unpositioned(bb) => *self = BasicBlockInfo::Positioned(*bb),
            _ => {}
        };
        self
    }

    pub fn is_positioned(&self) -> bool {
        match self {
            BasicBlockInfo::Positioned(_) => true,
            _ => false,
        }
    }
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            arguments: FxHashMap::default(),
            locals: FxHashMap::default(),
        }
    }
}

impl TypedValue {
    pub fn new(ty: Type, val: LLVMValueRef) -> Self {
        Self { ty, val }
    }
}

impl ClassTypesHolder {
    pub fn new() -> Self {
        Self {
            map: FxHashMap::default(),
        }
    }

    pub fn get(&mut self, class: &ClassInfo) -> Option<LLVMTypeRef> {
        if let Some(names) = self.map.get(&class.namespace) {
            if let Some(ty) = names.get(&class.name) {
                return Some(*ty);
            }
        }
        None
    }

    pub fn add(&mut self, class: &ClassInfo, ty: LLVMTypeRef) {
        self.map
            .entry(class.namespace.clone())
            .or_insert(FxHashMap::default())
            .insert(class.name.clone(), ty);
    }
}

unsafe fn llvm_const_int32(ctx: LLVMContextRef, n: u64) -> LLVMValueRef {
    LLVMConstInt(LLVMInt32TypeInContext(ctx), n, 1)
}

unsafe fn llvm_const_ptr(ctx: LLVMContextRef, p: *mut u64) -> LLVMValueRef {
    let ptr_as_int = LLVMConstInt(LLVMInt64TypeInContext(ctx), p as u64, 0);
    let const_ptr = LLVMConstIntToPtr(ptr_as_int, LLVMPointerType(LLVMInt8TypeInContext(ctx), 0));
    const_ptr
}

// Builtins

#[no_mangle]
fn write_line_int(n: i32) {
    println!("{}", n);
}

#[no_mangle]
fn write_line_char(c: i32) {
    println!("{}", c as u8 as char);
}

#[no_mangle]
fn write_line_string(s: *mut String) {
    println!("{}", unsafe { &*s });
}

#[no_mangle]
fn write_int(n: i32) {
    print!("{}", n);
}

#[no_mangle]
fn write_char(c: i32) {
    print!("{}", c as u8 as char);
}

#[no_mangle]
fn write_string(s: *mut String) {
    print!("{}", unsafe { &*s });
}

#[no_mangle]
fn get_length(s: *mut String) -> i32 {
    unsafe { &*s }.len() as i32
}

#[no_mangle]
fn get_chars(s: *mut String, i: i32) -> i32 {
    unsafe { &*s }.as_str().as_bytes()[i as usize] as i32
}

#[no_mangle]
fn memory_alloc(len: u32) -> *mut u8 {
    Box::into_raw(vec![0u8; len as usize].into_boxed_slice()) as *mut u8
}
