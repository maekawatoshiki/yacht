use crate::{
    exec::{cfg::*, instruction::*},
    metadata::{metadata::*, method::*, signature::*},
};
use llvm;
use llvm::{core::*, prelude::*};
use rustc_hash::FxHashMap;
use std::collections::VecDeque;
use std::ffi::CString;
use std::ptr;

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
pub struct PhiStack {
    src_bb: LLVMBasicBlockRef,
    stack: Vec<LLVMValueRef>,
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
    compile_queue: VecDeque<(MethodSignature, LLVMValueRef, MethodBodyRef)>,
    builtin_functions: FxHashMap<String, LLVMValueRef>,
}

#[derive(Debug, Clone)]
pub struct Environment {
    pub arguments: FxHashMap<usize, LLVMValueRef>,
    pub locals: FxHashMap<usize, LLVMValueRef>,
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

        let f = *self.builtin_functions.get_mut("WriteLine(int)").unwrap();
        llvm::execution_engine::LLVMAddGlobalMapping(
            ee,
            f,
            write_line_int as *mut ::std::ffi::c_void,
        );

        llvm::execution_engine::LLVMRunFunction(ee, main, 0, vec![].as_mut_ptr());
    }

    pub unsafe fn generate_main(&mut self, method_ref: MethodBodyRef) -> LLVMValueRef {
        let method = method_ref.borrow();
        let mut basic_blocks = CFGMaker::new().make_basic_blocks(&method.body);
        let (ret_ty, mut params_ty): (LLVMTypeRef, Vec<LLVMTypeRef>) =
            { (Type::new(ElementType::Void).to_llvmty(self.context), vec![]) };
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
                    BasicBlockInfo::Unpositioned(LLVMAppendBasicBlock(
                        func,
                        CString::new("").unwrap().as_ptr(),
                    )),
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
            // if ret_ty == VariableType::Void {
            LLVMBuildRetVoid(self.builder);
            // } else {
            //     LLVMBuildRet(self.builder, LLVMConstNull(func_ret_ty));
            // }
        }

        let mut iter_bb = LLVMGetFirstBasicBlock(func);
        while iter_bb != ptr::null_mut() {
            if LLVMIsATerminatorInst(LLVMGetLastInstruction(iter_bb)) == ptr::null_mut() {
                let terminator_builder = LLVMCreateBuilderInContext(self.context);
                LLVMPositionBuilderAtEnd(terminator_builder, iter_bb);
                // if ret_ty == VariableType::Void {
                LLVMBuildRetVoid(self.builder);
                // } else {
                //     LLVMBuildRet(terminator_builder, LLVMConstNull(func_ret_ty));
                // }
            }
            iter_bb = LLVMGetNextBasicBlock(iter_bb);
        }

        self.basic_blocks.clear();
        self.phi_stack.clear();

        while let Some((sig, func, method)) = self.compile_queue.pop_front() {
            self.generate_func(sig, func, method);
        }

        when_debug!(LLVMDumpModule(self.module));

        llvm::analysis::LLVMVerifyFunction(
            func,
            llvm::analysis::LLVMVerifierFailureAction::LLVMAbortProcessAction,
        );

        LLVMRunPassManager(self.pass_mgr, self.module);

        func
    }

    unsafe fn generate_func(
        &mut self,
        sig: MethodSignature,
        func: LLVMValueRef,
        method_ref: MethodBodyRef,
    ) {
        self.generating = Some(func);
        self.env = Environment::new();

        let method = method_ref.borrow();
        let mut basic_blocks = CFGMaker::new().make_basic_blocks(&method.body);
        let ret_ty = LLVMGetElementType(LLVMGetReturnType(LLVMTypeOf(func)));

        let bb_entry = LLVMAppendBasicBlockInContext(
            self.context,
            func,
            CString::new("entry").unwrap().as_ptr(),
        );

        LLVMPositionBuilderAtEnd(self.builder, bb_entry);

        for (i, ty) in sig.params.iter().enumerate() {
            LLVMBuildStore(
                self.builder,
                LLVMGetParam(func, i as u32),
                self.get_argument(i, Some(&ty)),
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
                    BasicBlockInfo::Unpositioned(LLVMAppendBasicBlock(
                        func,
                        CString::new("").unwrap().as_ptr(),
                    )),
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

    unsafe fn get_local(&mut self, id: usize, ty: Option<&Type>) -> LLVMValueRef {
        if let Some(v) = self.env.locals.get(&id) {
            return *v;
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

        let var = LLVMBuildAlloca(
            builder,
            ty.unwrap().to_llvmty(self.context),
            CString::new("").unwrap().as_ptr(),
        );

        self.env.locals.insert(id, var);
        var
    }

    unsafe fn get_argument(&mut self, id: usize, ty: Option<&Type>) -> LLVMValueRef {
        if let Some(v) = self.env.arguments.get(&id) {
            return *v;
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

        let var = LLVMBuildAlloca(
            builder,
            ty.unwrap().to_llvmty(self.context),
            CString::new("").unwrap().as_ptr(),
        );

        self.env.arguments.insert(id, var);
        var
    }

    unsafe fn compile_block(
        &mut self,
        blocks: &mut [BasicBlock],
        idx: usize,
        init_stack: Vec<LLVMValueRef>,
    ) -> CResult<usize> {
        #[rustfmt::skip]
        macro_rules! block { () => {{ &mut blocks[idx] }}; };

        if block!().generated {
            return Ok(0);
        }

        block!().generated = true;

        let bb = self.basic_blocks.get_mut(&block!().start).unwrap();
        LLVMPositionBuilderAtEnd(self.builder, bb.set_positioned().retrieve());

        let phi_stack = self.build_phi_stack(block!().start, init_stack);
        let stack = self.compile_bytecode(block!(), phi_stack)?;

        fn find(pc: usize, blocks: &[BasicBlock]) -> Option<usize> {
            for (i, block) in blocks.iter().enumerate() {
                if block.start == pc {
                    return Some(i);
                }
            }
            None
        }

        match block!().kind.clone() {
            BrKind::ConditionalJmp { destinations } => {
                let mut d = 0;
                for dst in destinations {
                    if let Some(i) = find(dst, blocks) {
                        d = self.compile_block(blocks, i, stack.clone())?;
                    } else {
                        continue;
                    };
                    // TODO: All ``d`` must be the same
                }
                Ok(d)
            }
            BrKind::UnconditionalJmp { destination } => {
                let src_bb = self.get_basic_block(block!().start).retrieve();
                self.phi_stack
                    .entry(destination)
                    .or_insert(vec![])
                    .push(PhiStack { src_bb, stack });
                Ok(destination)
            }
            BrKind::JmpRequired { destination } => {
                let src_bb = self.get_basic_block(block!().start).retrieve();
                if cur_bb_has_no_terminator(self.builder) {
                    let bb = self
                        .get_basic_block(destination)
                        .set_positioned()
                        .retrieve();
                    LLVMBuildBr(self.builder, bb);
                }
                self.phi_stack
                    .entry(destination)
                    .or_insert(vec![])
                    .push(PhiStack { src_bb, stack });
                Ok(destination)
            }
            _ => Ok(0),
        }
    }

    unsafe fn build_phi_stack(
        &mut self,
        start: usize,
        mut stack: Vec<LLVMValueRef>,
    ) -> Vec<LLVMValueRef> {
        let init_size = stack.len();

        if let Some(phi_stacks) = self.phi_stack.get(&start) {
            // Firstly, build llvm's phi which needs a type of all conceivable values.
            let src_bb = phi_stacks[0].src_bb;
            for val in &phi_stacks[0].stack {
                let phi = LLVMBuildPhi(
                    self.builder,
                    LLVMTypeOf(*val),
                    CString::new("").unwrap().as_ptr(),
                );
                LLVMAddIncoming(phi, vec![*val].as_mut_ptr(), vec![src_bb].as_mut_ptr(), 1);
                stack.push(phi);
            }

            for phi_stack in &phi_stacks[1..] {
                let src_bb = phi_stack.src_bb;
                for (i, val) in (&phi_stack.stack).iter().enumerate() {
                    let phi = stack[init_size + i];
                    LLVMAddIncoming(phi, vec![*val].as_mut_ptr(), vec![src_bb].as_mut_ptr(), 1);
                }
            }
        }

        stack
    }

    unsafe fn compile_bytecode(
        &mut self,
        block: &BasicBlock,
        mut stack: Vec<LLVMValueRef>,
    ) -> CResult<Vec<LLVMValueRef>> {
        let code = &block.code;

        for instr in code {
            match instr {
                Instruction::Ldstr { .. } => {}
                Instruction::Ldc_I4_0 => stack.push(llvm_const_int32(self.context, 0)),
                Instruction::Ldc_I4_1 => stack.push(llvm_const_int32(self.context, 1)),
                Instruction::Ldc_I4_2 => stack.push(llvm_const_int32(self.context, 2)),
                Instruction::Ldc_I4_S { n } => {
                    stack.push(llvm_const_int32(self.context, *n as u64))
                }
                Instruction::Ldc_I4 { n } => stack.push(llvm_const_int32(self.context, *n as u64)),
                Instruction::Pop => {
                    stack.pop();
                }
                Instruction::Call { table, entry } => {
                    self.gen_instr_call(&mut stack, *table, *entry)
                }
                Instruction::Ldloc_0 => stack.push(LLVMBuildLoad(
                    self.builder,
                    self.get_local(0, None),
                    CString::new("").unwrap().as_ptr(),
                )),
                Instruction::Stloc_0 => {
                    LLVMBuildStore(self.builder, stack.pop().unwrap(), self.get_local(0, None));
                }
                Instruction::Ldarg_0 => stack.push(LLVMBuildLoad(
                    self.builder,
                    self.get_argument(0, None),
                    CString::new("").unwrap().as_ptr(),
                )),
                Instruction::Ldarg_1 => stack.push(LLVMBuildLoad(
                    self.builder,
                    self.get_argument(1, None),
                    CString::new("").unwrap().as_ptr(),
                )),
                Instruction::Add => {
                    let val2 = stack.pop().unwrap();
                    let val1 = stack.pop().unwrap();
                    stack.push(LLVMBuildAdd(
                        self.builder,
                        val1,
                        val2,
                        CString::new("add").unwrap().as_ptr(),
                    ));
                }
                Instruction::Sub => {
                    let val2 = stack.pop().unwrap();
                    let val1 = stack.pop().unwrap();
                    stack.push(LLVMBuildSub(
                        self.builder,
                        val1,
                        val2,
                        CString::new("sub").unwrap().as_ptr(),
                    ));
                }
                Instruction::Ret => {
                    if LLVMGetTypeKind(LLVMGetElementType(LLVMGetReturnType(LLVMTypeOf(
                        self.generating.unwrap(),
                    )))) == llvm::LLVMTypeKind::LLVMVoidTypeKind
                    {
                        LLVMBuildRetVoid(self.builder);
                    } else {
                        let val = stack.pop().unwrap();
                        LLVMBuildRet(self.builder, val);
                    }
                }
                Instruction::Bge { .. } => {
                    let val2 = stack.pop().unwrap();
                    let val1 = stack.pop().unwrap();
                    let cond_val = LLVMBuildICmp(
                        self.builder,
                        llvm::LLVMIntPredicate::LLVMIntSGE,
                        val1,
                        val2,
                        CString::new("bge").unwrap().as_ptr(),
                    );
                    let destinations = block.kind.get_conditional_jump_destinations();
                    let bb_then = self.get_basic_block(destinations[0]).retrieve();
                    let bb_else = self.get_basic_block(destinations[1]).retrieve();
                    LLVMBuildCondBr(self.builder, cond_val, bb_then, bb_else);
                }
                Instruction::Blt { .. } => {
                    let val2 = stack.pop().unwrap();
                    let val1 = stack.pop().unwrap();
                    let cond_val = LLVMBuildICmp(
                        self.builder,
                        llvm::LLVMIntPredicate::LLVMIntSLT,
                        val1,
                        val2,
                        CString::new("bge").unwrap().as_ptr(),
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
            }
        }

        // Instruction::Bge { target } => self.instr_bge(image, *target),
        Ok(stack)
    }

    unsafe fn gen_instr_call(&mut self, stack: &mut Vec<LLVMValueRef>, table: usize, entry: usize) {
        let table = &self.image.metadata.metadata_stream.tables[table][entry - 1];
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
                        let ty = SignatureParser::new(sig).parse_method_ref_sig().unwrap();

                        if ar_name == "mscorlib"
                            && ty_namespace == "System"
                            && ty_name == "Console"
                            && name == "WriteLine"
                        {
                            let val = stack.pop().unwrap();
                            if ty.equal_method(ElementType::Void, &[ElementType::String]) {
                                // println!(
                                //     "{}",
                                //     String::from_utf16_lossy(
                                //         self.image
                                //             .metadata
                                //             .user_strings
                                //             .get(&val.as_string().unwrap())
                                //             .unwrap()
                                //     )
                                // );
                            } else if ty.equal_method(ElementType::Void, &[ElementType::I4]) {
                                self.call_function(
                                    *self.builtin_functions.get("WriteLine(int)").unwrap(),
                                    vec![val],
                                );
                                // println!("{}", val.as_int32().unwrap());
                            }
                        }
                    }
                    _ => unimplemented!(),
                }
            }
            Table::MethodDef(mdt) => {
                // let saved_program_counter = self.program_counter;
                // self.program_counter = 0;
                //
                let sig = self
                    .image
                    .metadata
                    .blob
                    .get(&(mdt.signature as u32))
                    .unwrap();
                let ty = SignatureParser::new(sig).parse_method_def_sig().unwrap();
                let method_sig = ty.as_fnptr().unwrap();
                let params = {
                    let mut params = vec![];
                    for _ in 0..method_sig.params.len() {
                        params.push(stack.pop().unwrap());
                    }
                    params.reverse();
                    params
                    // self.stack_pop_last_elements(method_sig.params.len() as usize)
                };
                let rva = mdt.rva;
                let func = if let Some(llvm_func) = self.generated.get(&rva) {
                    *llvm_func
                } else {
                    let method_ref = self.image.get_method(mdt.rva);
                    let ret_ty = method_sig.ret.to_llvmty(self.context);
                    let mut params_ty = method_sig
                        .params
                        .iter()
                        .map(|ty| ty.to_llvmty(self.context))
                        .collect::<Vec<LLVMTypeRef>>();
                    let func_ty =
                        LLVMFunctionType(ret_ty, params_ty.as_mut_ptr(), params_ty.len() as u32, 0);
                    let func =
                        LLVMAddFunction(self.module, CString::new("1").unwrap().as_ptr(), func_ty);
                    self.generated.insert(rva, func);
                    self.compile_queue
                        .push_back((method_sig.clone(), func, method_ref));
                    func
                };
                let ret = self.call_function(func, params);
                if !method_sig.ret.is_void() {
                    stack.push(ret)
                }
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
            CString::new("").unwrap().as_ptr(),
        )
    }

    unsafe fn get_basic_block(&mut self, pc: usize) -> &mut BasicBlockInfo {
        let func = self.generating.unwrap();
        self.basic_blocks.entry(pc).or_insert_with(|| {
            BasicBlockInfo::Unpositioned(LLVMAppendBasicBlock(
                func,
                CString::new("").unwrap().as_ptr(),
            ))
        })
    }
}

unsafe fn cur_bb_has_no_terminator(builder: LLVMBuilderRef) -> bool {
    LLVMIsATerminatorInst(LLVMGetLastInstruction(LLVMGetInsertBlock(builder))) == ptr::null_mut()
}

pub trait CastIntoLLVMType {
    unsafe fn to_llvmty(&self, ctx: LLVMContextRef) -> LLVMTypeRef;
}

impl CastIntoLLVMType for Type {
    unsafe fn to_llvmty(&self, ctx: LLVMContextRef) -> LLVMTypeRef {
        match self.base {
            ElementType::Void => LLVMVoidTypeInContext(ctx),
            ElementType::I4 => LLVMInt32TypeInContext(ctx),
            _ => unimplemented!()
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

unsafe fn llvm_const_int32(ctx: LLVMContextRef, n: u64) -> LLVMValueRef {
    LLVMConstInt(LLVMInt32TypeInContext(ctx), n, 1)
}

// Builtins

#[no_mangle]
fn write_line_int(n: i32) {
    println!("{}", n);
}
