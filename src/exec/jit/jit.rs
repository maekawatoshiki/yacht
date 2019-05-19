use crate::{
    exec::{
        instruction::*,
        jit::{builtin::*, cfg::*},
    },
    metadata::{class::*, image::*, metadata::*, method::*, signature::*, token::*},
    util::{holder::*, name_path::*},
};
use llvm;
use llvm::{core::*, prelude::*};
use rustc_hash::FxHashMap;
use std::collections::VecDeque;
use std::ffi::CString;
use std::ptr;

macro_rules! cstr0 {
    () => {
        CString::new("").unwrap().as_ptr()
    };
}

macro_rules! raw_memory {
    ($elem_ty:ty, $len:expr) => {{
        Box::into_raw(vec![0 as $elem_ty; $len].into_boxed_slice()) as *mut $elem_ty
    }};
}

pub type MethodTableElementTy = *mut ::std::ffi::c_void;
pub type MethodTablePtrTy = *mut MethodTableElementTy;

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

pub struct JITCompiler<'a> {
    pub image: &'a mut Image,
    pub context: LLVMContextRef,
    pub module: LLVMModuleRef,
    pub builder: LLVMBuilderRef,
    pub pass_mgr: LLVMPassManagerRef,
    pub generating: Option<LLVMValueRef>,
    pub generated: FxHashMap<RVA, LLVMValueRef>,
    pub basic_blocks: FxHashMap<usize, BasicBlockInfo>,
    pub phi_stack: FxHashMap<usize, Vec<PhiStack>>, // destination,
    pub env: Environment,
    pub compile_queue: VecDeque<(LLVMValueRef, MethodInfoRef)>,
    pub builtin_functions: BuiltinFunctions,
    pub class_types: ClassTypesHolder,
    pub method_table_map: FxHashMap<MethodTablePtrTy, (LLVMValueRef, Vec<LLVMValueRef>)>,
}

#[derive(Debug, Clone)]
pub struct Environment {
    pub arguments: FxHashMap<usize, TypedValue>,
    pub locals: FxHashMap<usize, TypedValue>,
}

#[derive(Debug, Clone)]
pub struct ClassTypesHolder {
    base: Holder<(LLVMTypeRef, MethodTablePtrTy)>,
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
            class_types: ClassTypesHolder::new(context),
            builtin_functions: BuiltinFunctions::new(context, module),
            method_table_map: FxHashMap::default(),
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

        for f in self.builtin_functions.list_all_function() {
            llvm::execution_engine::LLVMAddGlobalMapping(ee, f.llvm_function, f.function);
        }

        llvm::execution_engine::LLVMRunFunction(ee, main, 0, vec![].as_mut_ptr());
    }

    pub unsafe fn generate_main(&mut self, method_ref: &MethodInfoRef) -> LLVMValueRef {
        self.basic_blocks.clear();
        self.phi_stack.clear();
        self.env = Environment::new();

        let method_info = method_ref.borrow();
        let method = method_info.as_mdef();

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

        let bb_before_entry = LLVMAppendBasicBlockInContext(
            self.context,
            func,
            CString::new("initialize").unwrap().as_ptr(),
        );

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

        // Entry block is positioned
        self.basic_blocks
            .insert(0, BasicBlockInfo::Positioned(bb_entry));

        // Other blocks are not positioned
        for block in &basic_blocks {
            if block.start > 0 {
                self.basic_blocks.insert(
                    block.start,
                    BasicBlockInfo::Unpositioned(LLVMAppendBasicBlock(func, cstr0!())),
                );
            }
        }

        // Compile all the basic blocks

        for i in 0..basic_blocks.len() {
            self.compile_block(&mut basic_blocks, i, vec![]).unwrap();
        }

        // Compile the functions in the queue

        while let Some((func, method)) = self.compile_queue.pop_front() {
            self.generate_func(func, &method);
        }

        // Set all the class methods to the appropriate method_table

        LLVMPositionBuilderAtEnd(self.builder, bb_before_entry);

        for (_, (llvm_method_table, methods)) in &self.method_table_map {
            if methods.len() == 0 {
                continue;
            }

            let method_table = self.typecast(
                *llvm_method_table,
                LLVMPointerType(LLVMTypeOf(methods[0]), 0),
            );

            for (i, vmethod) in methods.iter().enumerate() {
                self.store2element(method_table, vec![self.llvm_int32(i as u64)], *vmethod);
            }
        }

        LLVMBuildBr(self.builder, bb_entry);

        // Append ``ret void`` to the incomplete basic blocks

        let mut iter_bb = LLVMGetFirstBasicBlock(func);

        while iter_bb != ptr::null_mut() {
            if LLVMIsATerminatorInst(LLVMGetLastInstruction(iter_bb)) == ptr::null_mut() {
                let terminator_builder = LLVMCreateBuilderInContext(self.context);
                LLVMPositionBuilderAtEnd(terminator_builder, iter_bb);
                LLVMBuildRetVoid(terminator_builder);
            }
            iter_bb = LLVMGetNextBasicBlock(iter_bb);
        }

        when_debug!(LLVMDumpModule(self.module));

        llvm::analysis::LLVMVerifyFunction(
            func,
            llvm::analysis::LLVMVerifierFailureAction::LLVMAbortProcessAction,
        );

        LLVMRunPassManager(self.pass_mgr, self.module);

        func
    }

    unsafe fn generate_func(&mut self, func: LLVMValueRef, method_ref: &MethodInfoRef) {
        self.generating = Some(func);
        self.env = Environment::new();

        let method_info = method_ref.borrow();
        let method = method_info.as_mdef();
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
                    LLVMBuildRetVoid(terminator_builder);
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
            blocks
                .iter()
                .enumerate()
                .find(|(_, block)| block.start == start)
                .map_or(None, |(i, _)| Some(i))
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
                    let i = find_block(dst, blocks).unwrap();
                    d = self.compile_block(blocks, i, stack.clone())?;
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
                    let bb = self.get_basic_block(*destination).retrieve();
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
                Type::i4_ty(), self.llvm_int32($n as u64),
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
            let val = self.get_local($n, None);
            LLVMBuildStore(self.builder,
                self.typecast(stack.pop().unwrap().val,
                    LLVMGetElementType(LLVMTypeOf(val))), val);
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
                Instruction::Ldnull => push_i4!(0),
                Instruction::Ldstr { us_offset } => stack.push(TypedValue::new(
                    Type::string_ty(),
                    self.llvm_ptr(Box::into_raw(Box::new(
                        self.image.get_user_string(*us_offset).clone(),
                    )) as *mut u8),
                )),
                Instruction::Ldc_I4_0 => push_i4!(0),
                Instruction::Ldc_I4_1 => push_i4!(1),
                Instruction::Ldc_I4_2 => push_i4!(2),
                Instruction::Ldc_I4_3 => push_i4!(3),
                Instruction::Ldc_I4_4 => push_i4!(4),
                Instruction::Ldc_I4_5 => push_i4!(5),
                Instruction::Ldc_I4_6 => push_i4!(6),
                Instruction::Ldc_I4_7 => push_i4!(7),
                Instruction::Ldc_I4_8 => push_i4!(8),
                Instruction::Ldc_I4_S { n } => push_i4!(*n),
                Instruction::Ldc_I4 { n } => push_i4!(*n),
                Instruction::Ldloc_0 => ldloc!(0),
                Instruction::Ldloc_1 => ldloc!(1),
                Instruction::Ldloc_2 => ldloc!(2),
                Instruction::Ldloc_3 => ldloc!(3),
                Instruction::Ldloc_S { n } => ldloc!(*n as usize),
                Instruction::Ldfld { token } => self.gen_instr_ldfld(&mut stack, *token),
                Instruction::Ldelem_U1 => self.gen_instr_ldelem_i1(&mut stack),
                Instruction::Ldelem_I1 => self.gen_instr_ldelem_i1(&mut stack),
                Instruction::Ldelem_I4 => self.gen_instr_ldelem_i4(&mut stack),
                Instruction::Stloc_0 => stloc!(0),
                Instruction::Stloc_1 => stloc!(1),
                Instruction::Stloc_2 => stloc!(2),
                Instruction::Stloc_3 => stloc!(3),
                Instruction::Stloc_S { n } => stloc!(*n as usize),
                Instruction::Stfld { token } => self.gen_instr_stfld(&mut stack, *token),
                Instruction::Stelem_I1 => self.gen_instr_stelem_i1(&mut stack),
                Instruction::Stelem_I4 => self.gen_instr_stelem_i4(&mut stack),
                Instruction::Ldarg_0 => ldarg!(0),
                Instruction::Ldarg_1 => ldarg!(1),
                Instruction::Ldarg_2 => ldarg!(2),
                Instruction::Ldarg_3 => ldarg!(3),
                Instruction::Ldlen => self.gen_instr_ldlen(&mut stack),
                Instruction::Conv_I4 => self.gen_instr_conv_i4(&mut stack),
                Instruction::Pop => {
                    stack.pop();
                }
                Instruction::Dup => {
                    stack.push(stack.last().unwrap().clone());
                }
                Instruction::Call { token } => self.gen_instr_call(&mut stack, *token),
                Instruction::CallVirt { token } => self.gen_instr_callvirt(&mut stack, *token),
                Instruction::Box { token } => self.gen_instr_box(&mut stack, *token),
                Instruction::Newobj { token } => self.gen_instr_newobj(&mut stack, *token),
                Instruction::Newarr { token } => self.gen_instr_newarr(&mut stack, *token),
                Instruction::Add => binop!(Add),
                Instruction::Sub => binop!(Sub),
                Instruction::Mul => binop!(Mul),
                Instruction::Div => binop!(SDiv),
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

    unsafe fn gen_instr_call(&mut self, stack: &mut Vec<TypedValue>, token: Token) {
        self.gen_instr_general_call(stack, token)
    }

    unsafe fn gen_instr_callvirt(&mut self, stack: &mut Vec<TypedValue>, token: Token) {
        self.gen_instr_general_call(stack, token)
    }

    unsafe fn get_function_by_rva(&mut self, rva: u32) -> LLVMValueRef {
        if let Some(f) = self.generated.get(&rva) {
            return *f;
        }

        let method_ref = self.image.get_method_by_rva(rva);
        let method_info = method_ref.borrow();
        let method = method_info.as_mdef();
        let method_sig = method.ty.as_fnptr().unwrap();

        let ret_ty = method_sig.ret.to_llvmty(self);
        let mut params_ty = method_sig
            .params
            .iter()
            .map(|ty| ty.to_llvmty(self))
            .collect::<Vec<LLVMTypeRef>>();

        if method_sig.has_this() {
            params_ty.insert(0, self.get_class_type(&method.class.borrow()))
        }

        let func_ty = LLVMFunctionType(ret_ty, params_ty.as_mut_ptr(), params_ty.len() as u32, 0);
        let func = LLVMAddFunction(
            self.module,
            CString::new(method.name.as_str()).unwrap().as_ptr(),
            func_ty,
        );

        self.generated.insert(rva, func);
        self.compile_queue.push_back((func, method_ref.clone()));

        func
    }

    unsafe fn gen_instr_general_call(&mut self, stack: &mut Vec<TypedValue>, token: Token) {
        unsafe fn call(
            compiler: &mut JITCompiler,
            stack: &mut Vec<TypedValue>,
            func: LLVMValueRef,
            method_sig: &MethodSignature,
        ) {
            let has_this = if method_sig.has_this() { 1 } else { 0 };
            let args = stack
                .drain(stack.len() - method_sig.params.len() - has_this..)
                .map(|tv| tv.val)
                .enumerate()
                .map(|(i, arg)| {
                    if method_sig.has_this() && i == 0 {
                        arg
                    } else {
                        let llvm_ty = method_sig.params[i - has_this].to_llvmty(compiler);
                        compiler.typecast(arg, llvm_ty)
                    }
                })
                .collect();
            let ret = compiler.call_function(func, args);
            if !method_sig.ret.is_void() {
                stack.push(TypedValue::new(method_sig.ret.clone(), ret));
            }
        };

        unsafe fn callvirt(
            compiler: &mut JITCompiler,
            stack: &mut Vec<TypedValue>,
            method: &MethodInfo,
            method_sig: &MethodSignature,
            method_ty: LLVMTypeRef,
        ) {
            let args: Vec<LLVMValueRef> = stack
                .drain(stack.len() - method_sig.params.len() -/*obj*/1..)
                .map(|tv| tv.val)
                .enumerate()
                .map(|(i, arg)| {
                    if i == 0 {
                        /* this */
                        arg
                    } else {
                        let llvm_ty = method_sig.params[i - 1].to_llvmty(compiler);
                        compiler.typecast(arg, llvm_ty)
                    }
                })
                .collect();
            let obj = args[0];

            let method_table =
                compiler.load_element(obj, vec![compiler.llvm_int32(0), compiler.llvm_int32(0)]);
            let idx = method
                .as_mdef()
                .class
                .borrow()
                .method_table
                .iter()
                .position(|m| m.borrow().get_name() == &method.as_mdef().name)
                .unwrap() as u64;
            let raw_vmethod = compiler.load_element(method_table, vec![compiler.llvm_int32(idx)]);
            let vmethod = compiler.typecast(raw_vmethod, method_ty);

            let ret = compiler.call_function(vmethod, args);
            if !method_sig.ret.is_void() {
                stack.push(TypedValue::new(method_sig.ret.clone(), ret));
            }
        };

        match self.image.get_table_entry(token) {
            Table::MemberRef(mrt) => {
                let token = mrt.class_table_and_entry();
                let class = &self.image.get_table_entry(token);
                match class {
                    Table::TypeRef(trt) => {
                        let type_path = self.image.get_path_from_type_ref_table(trt);
                        let name = self.image.get_string(mrt.name);
                        let ty = self.image.get_method_ref_type_from_signature(mrt.signature);
                        if let Some(f) = self
                            .builtin_functions
                            .get_method(type_path.with_method_name(name), &ty)
                        {
                            let method_sig = ty.as_fnptr().unwrap();
                            call(self, stack, f.llvm_function, method_sig);
                        }
                    }
                    _ => unimplemented!(),
                }
            }
            Table::MethodDef(mdt) => {
                let func = self.get_function_by_rva(mdt.rva);
                let method_ref = self.image.get_method_by_rva(mdt.rva);
                let method = method_ref.borrow();
                let method_sig = method.as_mdef().ty.as_fnptr().unwrap();
                if method.as_mdef().is_virtual() {
                    callvirt(self, stack, &method, method_sig, LLVMTypeOf(func));
                } else {
                    call(self, stack, func, method_sig);
                }
            }
            e => unimplemented!("call: unimplemented: {:?}", e),
        }
    }

    unsafe fn gen_instr_stfld(&mut self, stack: &mut Vec<TypedValue>, token: Token) {
        let val = stack.pop().unwrap();
        let obj = stack.pop().unwrap();
        match self.image.get_table_entry(token) {
            Table::Field(f) => {
                let name = self.image.get_string(f.name);
                let class = &obj.ty.as_class().unwrap().borrow();
                let idx =
                    class.fields.iter().position(|f| &f.name == name).unwrap() + /*method_table=*/1;
                self.store2element(
                    obj.val,
                    vec![self.llvm_int32(0), self.llvm_int32(idx as u64)],
                    val.val,
                )
            }
            e => unimplemented!("{:?}", e),
        }
    }

    unsafe fn gen_instr_ldfld(&mut self, stack: &mut Vec<TypedValue>, token: Token) {
        let obj = stack.pop().unwrap();
        match self.image.get_table_entry(token) {
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
                stack.push(TypedValue::new(
                    ty,
                    self.load_element(
                        obj.val,
                        vec![
                            self.llvm_int32(0),
                            self.llvm_int32(idx as u64 + /*method_table=*/1),
                        ],
                    ),
                ));
            }
            e => unimplemented!("{:?}", e),
        }
    }

    unsafe fn gen_instr_ldelem_i1(&mut self, stack: &mut Vec<TypedValue>) {
        let val = self.gen_instr_general_ldelem(stack);
        stack.push(TypedValue::new(Type::boolean_ty(), val))
    }

    unsafe fn gen_instr_ldelem_i4(&mut self, stack: &mut Vec<TypedValue>) {
        let val = self.gen_instr_general_ldelem(stack);
        stack.push(TypedValue::new(Type::i4_ty(), val))
    }

    unsafe fn gen_instr_general_ldelem(&mut self, stack: &mut Vec<TypedValue>) -> LLVMValueRef {
        let index = stack.pop().unwrap().val;
        let array = stack.pop().unwrap().val;
        self.load_element(
            array,
            vec![LLVMBuildAdd(
                self.builder,
                index,
                self.llvm_int32(4),
                cstr0!(),
            )],
        )
    }

    unsafe fn gen_instr_stelem_i1(&mut self, stack: &mut Vec<TypedValue>) {
        self.gen_instr_general_stelem(stack)
    }

    unsafe fn gen_instr_stelem_i4(&mut self, stack: &mut Vec<TypedValue>) {
        self.gen_instr_general_stelem(stack)
    }

    unsafe fn gen_instr_general_stelem(&mut self, stack: &mut Vec<TypedValue>) {
        let value = stack.pop().unwrap().val;
        let index = stack.pop().unwrap().val;
        let array = stack.pop().unwrap().val;
        self.store2element(
            array,
            vec![LLVMBuildAdd(
                self.builder,
                index,
                self.llvm_int32(4),
                cstr0!(),
            )],
            value,
        );
    }

    unsafe fn gen_instr_ldlen(&mut self, stack: &mut Vec<TypedValue>) {
        let array = self.typecast(
            stack.pop().unwrap().val,
            LLVMPointerType(LLVMInt32TypeInContext(self.context), 0),
        );
        let index = self.load_element(array, vec![self.llvm_int32(0)]);
        stack.push(TypedValue::new(Type::i4_ty(), index));
    }

    unsafe fn gen_instr_conv_i4(&mut self, stack: &mut Vec<TypedValue>) {
        let value = stack.pop().unwrap().val;
        stack.push(TypedValue::new(
            Type::i4_ty(),
            self.typecast(value, LLVMInt32TypeInContext(self.context)),
        ))
    }

    unsafe fn gen_instr_box(&mut self, stack: &mut Vec<TypedValue>, token: Token) {
        let val = stack.pop().unwrap().val;
        match self.image.get_table_entry(token) {
            Table::TypeRef(trt) => {
                // TODO: Refactor
                let TypeFullPath(path) = self.image.get_path_from_type_ref_table(&trt);
                match (path[0], path[1], path[2]) {
                    ("mscorlib", "System", "Int32") => {
                        let class_system_int32_ref =
                            self.image.class_cache.get(&token.into()).unwrap().clone();
                        let class_system_int32 = class_system_int32_ref.borrow();
                        let class_int32 = self
                            .class_types
                            .get(TypeFullPath(vec!["mscorlib", "System", "Int32"]))
                            .unwrap();
                        let new_obj = self.typecast(
                            self.call_function(
                                self.builtin_functions
                                    .get_helper_function("memory_alloc")
                                    .unwrap()
                                    .llvm_function,
                                vec![self.get_size_of_llvm_class_type(class_int32)],
                            ),
                            class_int32,
                        );
                        let method_table = self.ensure_all_class_methods_compiled(
                            self.class_types
                                .get_method_table_ptr(&*class_system_int32)
                                .unwrap(),
                            &class_system_int32.method_table,
                        );
                        self.store2element(
                            new_obj,
                            vec![self.llvm_int32(0), self.llvm_int32(0)],
                            method_table,
                        );
                        self.store2element(
                            new_obj,
                            vec![self.llvm_int32(0), self.llvm_int32(1)],
                            val,
                        );
                        stack.push(TypedValue::new(Type::object_ty(), new_obj));
                    }
                    _ => unimplemented!(),
                }
            }
            e => unimplemented!("newarr: unimplemented: {:?}", e),
        }
    }

    unsafe fn gen_instr_newarr(&mut self, stack: &mut Vec<TypedValue>, token: Token) {
        let len = stack.pop().unwrap().val;
        match self.image.get_table_entry(token) {
            Table::TypeRef(trt) => {
                // TODO: Refactor
                let TypeFullPath(path) = self.image.get_path_from_type_ref_table(&trt);
                match (path[0], path[1], path[2]) {
                    ("mscorlib", "System", ty) => {
                        let (szarr_ty, sz) = match ty {
                            "Int32" => (Type::i4_szarr_ty(), 4),
                            "Boolean" => (Type::boolean_szarr_ty(), 1),
                            _ => unimplemented!(),
                        };
                        let llvm_szarr_ty = szarr_ty.to_llvmty(self);
                        let new_arr = self.typecast(
                            self.call_function(
                                self.builtin_functions
                                    .get_helper_function("new_szarray")
                                    .unwrap()
                                    .llvm_function,
                                vec![self.llvm_int32(sz), len],
                            ),
                            llvm_szarr_ty,
                        );
                        stack.push(TypedValue::new(szarr_ty, new_arr));
                    }
                    _ => unimplemented!(),
                }
            }
            e => unimplemented!("newarr: unimplemented: {:?}", e),
        }
    }

    unsafe fn gen_instr_newobj(&mut self, stack: &mut Vec<TypedValue>, token: Token) {
        match self.image.get_table_entry(token) {
            Table::MemberRef(_mrt) => {} // TODO
            Table::MethodDef(mdt) => {
                let method_ref = self.image.get_method_by_rva(mdt.rva);
                let method_info = method_ref.borrow();
                let method = method_info.as_mdef();
                let method_sig = method.ty.as_fnptr().unwrap();
                let llvm_class_ty = self.get_class_type(&method.class.borrow());
                let new_obj = self.typecast(
                    self.call_function(
                        self.builtin_functions
                            .get_helper_function("memory_alloc")
                            .unwrap()
                            .llvm_function,
                        vec![self.get_size_of_llvm_class_type(llvm_class_ty)],
                    ),
                    llvm_class_ty,
                );
                let mut params: Vec<LLVMValueRef> = stack
                    .drain(stack.len() - method_sig.params.len()..)
                    .map(|tv| tv.val)
                    .collect();
                params.insert(0, new_obj);

                let func = self.get_function_by_rva(mdt.rva);
                self.call_function(func, params);

                let class = method.class.borrow();
                let method_table = self.ensure_all_class_methods_compiled(
                    self.class_types.get_method_table_ptr(&*class).unwrap(),
                    &class.method_table,
                );

                self.store2element(
                    new_obj,
                    vec![self.llvm_int32(0), self.llvm_int32(0)],
                    method_table,
                );

                stack.push(TypedValue::new(
                    Type::new(ElementType::Class(method.class.clone())),
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

        // method_table always occupies the first field
        fields_ty.insert(
            0,
            LLVMPointerType(LLVMPointerType(LLVMInt8TypeInContext(self.context), 0), 0),
        );

        LLVMStructSetBody(class_ty, fields_ty.as_mut_ptr(), fields_ty.len() as u32, 0);

        class_ptr_ty
    }

    unsafe fn get_size_of_llvm_class_type(&self, class: LLVMTypeRef) -> LLVMValueRef {
        LLVMConstPtrToInt(
            LLVMConstGEP(
                LLVMConstNull(class),
                vec![self.llvm_int32(1)].as_mut_ptr(),
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

    unsafe fn ensure_all_class_methods_compiled(
        &mut self,
        method_table_ptr: MethodTablePtrTy,
        method_table: &Vec<MethodInfoRef>,
    ) -> LLVMValueRef {
        if let Some((llvm_method_table, _)) = self.method_table_map.get(&method_table_ptr) {
            return *llvm_method_table;
        }

        let llvm_method_table = self.llvm_ptr(method_table_ptr as *mut u8);
        let mut vmethods = vec![];

        for vmethod in method_table {
            match &*vmethod.borrow() {
                MethodInfo::MDef(m) => vmethods.push(self.get_function_by_rva(m.rva)),
                // TODO
                MethodInfo::MRef(m) => {
                    let class = m.class.borrow();
                    vmethods.push(
                        self.builtin_functions
                            .get_method(
                                MethodFullPath(vec![
                                    "mscorlib",
                                    class.namespace.as_str(),
                                    class.name.as_str(),
                                    m.name.as_str(),
                                ]),
                                &m.ty,
                            )
                            .unwrap()
                            .llvm_function,
                    );
                }
            }
        }

        self.method_table_map
            .insert(method_table_ptr, (llvm_method_table, vmethods));

        llvm_method_table
    }

    unsafe fn load_element(&self, obj: LLVMValueRef, mut idx: Vec<LLVMValueRef>) -> LLVMValueRef {
        let gep = LLVMBuildGEP(
            self.builder,
            obj,
            idx.as_mut_ptr(),
            idx.len() as u32,
            cstr0!(),
        );
        LLVMBuildLoad(self.builder, gep, cstr0!())
    }

    unsafe fn store2element(
        &self,
        obj: LLVMValueRef,
        mut idx: Vec<LLVMValueRef>,
        val: LLVMValueRef,
    ) {
        let gep = LLVMBuildGEP(
            self.builder,
            obj,
            idx.as_mut_ptr(),
            idx.len() as u32,
            cstr0!(),
        );
        LLVMBuildStore(
            self.builder,
            self.typecast(val, LLVMGetElementType(LLVMTypeOf(gep))),
            gep,
        );
    }

    unsafe fn llvm_int32(&self, n: u64) -> LLVMValueRef {
        llvm_const_int32(self.context, n)
    }

    unsafe fn llvm_ptr(&self, ptr: *mut u8) -> LLVMValueRef {
        llvm_const_ptr(self.context, ptr)
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
            ElementType::SzArray(ref szarr) => {
                LLVMPointerType(szarr.elem_ty.to_llvmty(compiler), 0)
            }
            ElementType::Class(ref class) => {
                let class = &class.borrow();
                compiler.get_class_type(class)
            }
            ElementType::Object => LLVMPointerType(LLVMInt8TypeInContext(ctx), 0),
            _ => unimplemented!(),
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
    pub unsafe fn new(ctx: LLVMContextRef) -> Self {
        // TODO: Refactor

        let system_object =
            LLVMStructCreateNamed(ctx, CString::new("System::Object").unwrap().as_ptr());
        let mut fields_ty = vec![LLVMPointerType(
            LLVMPointerType(LLVMInt8TypeInContext(ctx), 0),
            0,
        )];
        LLVMStructSetBody(
            system_object,
            fields_ty.as_mut_ptr(),
            fields_ty.len() as u32,
            0,
        );

        let system_int32 =
            LLVMStructCreateNamed(ctx, CString::new("System::Int32").unwrap().as_ptr());
        let mut fields_ty = vec![
            LLVMPointerType(LLVMPointerType(LLVMInt8TypeInContext(ctx), 0), 0),
            LLVMInt32TypeInContext(ctx),
        ];
        LLVMStructSetBody(
            system_int32,
            fields_ty.as_mut_ptr(),
            fields_ty.len() as u32,
            0,
        );

        let method_table_len = 1;

        Self {
            base: {
                let mut holder = Holder::new();
                holder.add(
                    TypeFullPath(vec!["mscorlib", "System", "Object"]),
                    (
                        LLVMPointerType(system_object, 0),
                        raw_memory!(MethodTableElementTy, method_table_len),
                    ),
                );
                holder.add(
                    TypeFullPath(vec!["mscorlib", "System", "Int32"]),
                    (
                        LLVMPointerType(system_int32, 0),
                        raw_memory!(MethodTableElementTy, method_table_len),
                    ),
                );
                holder
            },
        }
    }

    pub fn get<'a, T: Into<TypeFullPath<'a>>>(&self, path: T) -> Option<LLVMTypeRef> {
        Some((*self.base.get(path.into())?).0)
    }

    pub fn get_method_table_ptr<'a, T: Into<TypeFullPath<'a>>>(
        &self,
        path: T,
    ) -> Option<MethodTablePtrTy> {
        Some((*self.base.get(path.into())?).1)
    }

    pub fn add(&mut self, class: &ClassInfo, ty: LLVMTypeRef) {
        self.base.add(
            class.into(): TypeFullPath,
            (
                ty,
                raw_memory!(MethodTableElementTy, class.method_table.len()),
            ),
        );
    }
}

unsafe fn llvm_const_int32(ctx: LLVMContextRef, n: u64) -> LLVMValueRef {
    LLVMConstInt(LLVMInt32TypeInContext(ctx), n, 1)
}

unsafe fn llvm_const_ptr(ctx: LLVMContextRef, p: *mut u8) -> LLVMValueRef {
    let ptr_as_int = LLVMConstInt(LLVMInt64TypeInContext(ctx), p as u64, 0);
    let const_ptr = LLVMConstIntToPtr(ptr_as_int, LLVMPointerType(LLVMInt8TypeInContext(ctx), 0));
    const_ptr
}
