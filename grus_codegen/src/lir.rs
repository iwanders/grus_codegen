#![allow(dead_code)]
#![allow(unused_imports)]
use cranelift_codegen::ir::Function as CraneliftIrFunction;
use cranelift_codegen::ir::Inst as IrInst;
use cranelift_codegen::ir::InstructionData as IrInstructionData;
use cranelift_codegen::ir::{self, Value};

use crate::codegen as cg;

/**
    A low(er) level intermediate representation.
*/
/*

    It would be nice if we can go from cranelift IR to this lower level representation...
    Identifying things like 'Magic stuff [EAX] and [--][--], page 36', which uses a register as
    base address and another as offset.

    Want to be able to put the calling convention here.

    Handle offsets and jumps for branches and loops.

    Ideall, we can handle;
        Some instructions still IrInst, with values.
            - Only know registers after we've performed the register allocation.
        Some instructions have (partial) limitations on registers
            - Some instructions have hardware constraints.
            - Calling convention.
            - If a single Ir Instruction becomes a bunch of instructions.

    Should we instead use cranelift IR opcodes to express?
    Basically going from cranelift IR to cranelift IR?

    That doesn't work though for sections that MUST have specific registers... like with the
    calling convention, since the cranelift IR is all on Values. It would be nice to be able to
    reuse that though.


    What if we do:
        LirSection:
            - Vec<HwInstruction>
            - Vec<IrInstruction>

        That way we can gradually convert ir instructions into hw instructions?
        And we can do something whereh hw instructions aren't fully specified yet, potentially
        with partial register specifications and populate the remainder?

        A block can then be made up of multiple sections.
        The trivial translation is lirsections where the ir instructions is one long.
        This is necessary for example where an ir instruction lowers into multiple hardware
        instructions.

        And we can track which ir instructions resulted in which hw instructions, while still being
        able to create sections that contain just hw instructions, like with the calling convention?

    Stages:
        - Function::new(), just copies the cranelift function.
        - lirify; creates lir blocks, copies each Cranelift Instruction into its own section.
        - lower_first; Creates hw instructions for most section, without register allocations.
        - register allocation:
            - For already lowered instructions, virtual operands are substituted with registers.
            - For non lowered ir, registers are stored for second lower pass.
        - lower second; now that registers are available for all instructions remaining ir is lowered.
        - Can create bytecode for sections now.
        - Something with jumps and offsets?
*/

/*
Need to give blocks clear 'these are the input registers' to the block, currently we don't have that.
*/
use cranelift_codegen::isa::CallConv;
use log::*;

/*
use regalloc2::Block as RegBlock;
use regalloc2::Function as RegFunction;
use regalloc2::Inst as RegInst;
use regalloc2::Operand as RegOperand;
use regalloc2::{InstRange, PRegSet, RegClass, VReg};
*/

#[derive(Copy, Clone, Debug)]
struct BlockId(usize);
impl BlockId {
    pub fn from(v: usize) -> BlockId {
        BlockId(v)
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Inst(usize);
use Inst as LirInst;

#[derive(Copy, Clone, Debug)]
pub enum LirOperand {
    Virtual(Value),
    Machine(cg::Operand),
}
impl From<Value> for LirOperand {
    fn from(v: Value) -> LirOperand {
        LirOperand::Virtual(v)
    }
}
impl From<cg::Operand> for LirOperand {
    fn from(v: cg::Operand) -> LirOperand {
        LirOperand::Machine(v)
    }
}

#[derive(Clone, Debug)]
pub struct InstructionData {
    pub operation: cg::Op,
    // Should we have def_operand and use_operand?
    pub def_operands: Vec<LirOperand>,
    pub use_operands: Vec<LirOperand>,
}
impl InstructionData {
    pub fn new(operation: cg::Op) -> Self {
        Self {
            operation,
            def_operands: vec![],
            use_operands: vec![],
        }
    }
    pub fn with_use<T: Into<LirOperand> + Copy>(self, ops: &[T]) -> Self {
        Self {
            operation: self.operation,
            def_operands: self.def_operands,
            use_operands: ops.into_iter().map(|z| (*z).into()).collect(),
        }
    }
    pub fn with_def<T: Into<LirOperand> + Copy>(self, ops: &[T]) -> Self {
        Self {
            operation: self.operation,
            def_operands: ops.into_iter().map(|z| (*z).into()).collect(),
            use_operands: self.use_operands,
        }
    }
    pub fn has_virtuals(&self) -> bool {
        self.use_operands
            .iter()
            .any(|v| matches!(v, LirOperand::Virtual(_)))
            || self
                .def_operands
                .iter()
                .any(|v| matches!(v, LirOperand::Virtual(_)))
    }
    pub fn assemble(&self) -> Result<Vec<u8>, cg::CodegenError> {
        let mut opv = cg::OperandVec::new();
        for cont in [&self.def_operands, &self.use_operands] {
            for v in cont.iter() {
                match v {
                    LirOperand::Machine(r) => opv.push(*r),
                    LirOperand::Virtual(_) => {
                        todo!()
                    }
                }
            }
        }
        let inst = cg::Instruction {
            op: self.operation,
            operands: opv,
        };
        inst.serialize()
    }
}

#[derive(Clone, Debug)]
struct Section {
    lir_inst: Vec<LirInst>,
    ir_inst: Vec<IrInst>,
    ir_regs: Vec<Vec<cg::Reg>>,
    is_lowered: bool,
}
impl Section {
    fn from_ir(v: IrInst) -> Self {
        Self {
            lir_inst: vec![],
            ir_inst: vec![v],
            ir_regs: vec![],
            is_lowered: false,
        }
    }
    pub fn is_lowered(&self) -> bool {
        self.is_lowered
    }
}

#[derive(Clone, Debug)]
struct Block {
    sections: Vec<Section>,
    id: BlockId,

    // block_param:
    block_preds: Vec<BlockId>,
    block_succs: Vec<BlockId>,

    block_params: Vec<Value>,
}
impl Block {
    fn new(id: BlockId) -> Self {
        Self {
            sections: vec![],
            block_succs: vec![],
            block_preds: vec![],
            block_params: vec![],
            id,
        }
    }
    pub fn push_section(&mut self, section: Section) {
        self.sections.push(section)
    }
}

#[derive(Debug)]
pub struct Function {
    blocks: Vec<Block>,
    entry_block: Option<BlockId>,

    instdata: Vec<InstructionData>,

    call_convention: CallConv,
    fun: CraneliftIrFunction,
    fun_args: Vec<Value>,
}

impl Function {
    pub fn from_ir(fun: &CraneliftIrFunction) -> Self {
        Self {
            blocks: vec![],
            instdata: vec![],
            fun_args: vec![],
            entry_block: None,
            call_convention: CallConv::SystemV,
            fun: fun.clone(),
        }
    }

    pub fn lirify(&mut self) {
        let dfg = &self.fun.stencil.dfg;
        let layout = &self.fun.stencil.layout;

        let cfg = cranelift_codegen::flowgraph::ControlFlowGraph::with_function(&self.fun);

        let ir_entry_block = self
            .fun
            .layout
            .entry_block()
            .expect("should have entry block");
        for v in dfg.block_params(ir_entry_block).iter() {
            self.fun_args.push(*v);
        }

        for b in layout.blocks() {
            let lirblockid = BlockId::from(b.as_u32() as usize);
            let mut lirblock = Block::new(lirblockid);

            debug!("b: {b:?}");
            let block_data = &dfg.blocks[b];
            debug!(
                "block_data params: {:?}",
                block_data.params(&dfg.value_lists)
            );

            lirblock.block_succs = cfg
                .succ_iter(b)
                .map(|v| BlockId::from(v.as_u32() as usize))
                .collect();
            lirblock.block_preds = cfg
                .pred_iter(b)
                .map(|v| BlockId::from(v.block.as_u32() as usize))
                .collect();

            lirblock.block_params = dfg.block_params(b).iter().copied().collect();

            for inst in layout.block_insts(b) {
                let s = Section::from_ir(inst);
                lirblock.push_section(s);

                debug!("inst: {inst:?}");
                let instdata = dfg.insts[inst];
                debug!("  instruction_data: {instdata:?}");
                let arguments = instdata.arguments(&dfg.value_lists);
                let type_of = |v: &ir::Value| dfg.value_type(*v);
                let types_of =
                    |v: &[ir::Value]| v.iter().map(|z| dfg.value_type(*z)).collect::<Vec<_>>();
                debug!("  args: {:?} types: {:?}", arguments, types_of(arguments));

                // let allocs_args = regs.inst_allocs(RegInst::new(inst.as_u32() as usize));
                // debug!("  allocs: {allocs_args:?}");
                // let use_allocs = &allocs_args[0..arguments.len()];
                // debug!("  use_allocs: {use_allocs:?}");
                // let def_allocs = &allocs_args[arguments.len().min(arguments.len() + 1)..];
                // debug!("  def_allocs: {def_allocs:?}");

                debug!(
                    "  results? {:?} -> {:?}  (types: {:?}) ",
                    dfg.has_results(inst),
                    dfg.inst_results(inst),
                    types_of(dfg.inst_results(inst))
                );
                let typevar_operand = instdata.typevar_operand(&dfg.value_lists);
                debug!(
                    "  typevar_operand: {:?}, type: {:?}",
                    typevar_operand,
                    typevar_operand.as_ref().map(type_of)
                );
                debug!("  opcode: {:?}", instdata.opcode());
            }
            self.blocks.push(lirblock);
        }

        let irentry = layout.entry_block().expect("should have entry block");
        let entryblockid = BlockId::from(irentry.as_u32() as usize);
        self.entry_block = Some(entryblockid);
    }

    /// Lower IR instructions to partial machine instructions
    pub fn lower_first(&mut self) {
        let new_op = InstructionData::new;
        use cg::{Op, Operand, Width};
        let dfg = &self.fun.dfg;
        let type_of = |v: &ir::Value| dfg.value_type(*v);
        let _types_of = |v: &[ir::Value]| v.iter().map(|z| dfg.value_type(*z)).collect::<Vec<_>>();

        for b in self.blocks.iter_mut() {
            for s in b.sections.iter_mut() {
                // let lirs = &mut s.lir_inst;
                let mut lirs = vec![];
                for inst in s.ir_inst.iter() {
                    let instdata = self.fun.dfg.insts[*inst];
                    let def_ir = self.fun.dfg.inst_results(*inst);
                    let use_ir = instdata.arguments(&self.fun.dfg.value_lists);
                    let typevar_operand = instdata.typevar_operand(&dfg.value_lists);
                    match instdata {
                        IrInstructionData::UnaryImm { opcode, imm } => match opcode {
                            ir::Opcode::Iconst => {
                                lirs.push(
                                    new_op(Op::Mov(Width::W64))
                                        .with_use(&[Operand::Immediate(imm.bits())])
                                        .with_def(&[def_ir[0]]),
                                );
                            }
                            _ => todo!(
                                "unimplemented opcode: {:?} in {:?}, of {:?}",
                                opcode,
                                inst,
                                self.fun.name
                            ),
                        },
                        IrInstructionData::MultiAry { opcode, args } => match opcode {
                            ir::Opcode::Return => {
                                if args.len(&self.fun.dfg.value_lists) != 1 {
                                    todo!()
                                }
                                // Lowered in second pass.
                            }
                            _ => todo!(
                                "unimplemented opcode: {:?} in {:?}, of {:?}",
                                opcode,
                                inst,
                                self.fun.name
                            ),
                        },

                        IrInstructionData::Binary { opcode, args: _ } => match opcode {
                            ir::Opcode::Iadd => {
                                let width: Width = typevar_operand
                                    .as_ref()
                                    .map(type_of)
                                    .map(|z| z.into())
                                    .unwrap();

                                // x86 add overwrites the first operand, so for now:
                                //    Move operand one into destination.
                                //    Add operand two to destination.
                                // lirs.push(
                                // new_op(Op::Mov(width))
                                // .with_use(&[use_ir[0]])
                                // .with_def(&[def_ir[0]]),
                                // );
                                // That violated ssa!
                                lirs.push(
                                    new_op(Op::IAdd(width))
                                        .with_use(&[use_ir[0], use_ir[1]])
                                        .with_def(&[def_ir[0]]),
                                );
                            }
                            ir::Opcode::Isub => {
                                let width: Width = typevar_operand
                                    .as_ref()
                                    .map(type_of)
                                    .map(|z| z.into())
                                    .unwrap();

                                // x86 add overwrites the first operand, so for now:
                                //    Move operand one into destination.
                                //    Add operand two to destination.
                                lirs.push(
                                    new_op(Op::ISub(width))
                                        .with_use(&[use_ir[0], use_ir[1]])
                                        .with_def(&[def_ir[0]]),
                                );
                            }

                            ir::Opcode::Imul => {
                                let width: Width = typevar_operand
                                    .as_ref()
                                    .map(type_of)
                                    .map(|z| z.into())
                                    .unwrap();

                                // x86 integer mul supports three forms, we use the two-operand
                                // version here, this multiplies into the destination.

                                lirs.push(
                                    new_op(Op::IMul(width))
                                        .with_use(&[use_ir[0], use_ir[1]])
                                        .with_def(&[def_ir[0]]),
                                );
                            }
                            _ => todo!(
                                "unimplemented opcode: {:?} in {:?}, of {:?}",
                                opcode,
                                inst,
                                self.fun.name
                            ),
                        },
                        IrInstructionData::Brif {
                            opcode: _,
                            arg,
                            blocks,
                        } => {
                            // Conditional branch when cond (arg) is non-zero
                            // take 'then' (first block) when c != 0 and the 'else' branch otherwise.
                            // In the ideal case, we'd have the register allocator shuffle the values in the two
                            // blocks we need to handle the if statement.
                            debug!("Brif : {arg:#?}  {blocks:#?}");
                            // Model the branch as an instruction that reads one value... ignoring the fact that it
                            // writes values used by the branch for now.
                            lirs.push(new_op(Op::Test).with_use::<LirOperand>(&[
                                use_ir[0].into(),
                                LirOperand::Machine(Operand::Immediate(0)).into(),
                            ]));
                            // Collect the arguments that we'll end up using...
                            let mut use_args: Vec<LirOperand> = vec![];
                            for b in blocks.iter() {
                                let args = b.args_slice(&dfg.value_lists);
                                for v in args {
                                    use_args.push((*v).into());
                                }
                            }
                            // The second lower will split this into the two blocks, so here it is a single block.
                            lirs.push(
                                new_op(Op::Jcc(cg::JumpCondition::IsZero)).with_use(&use_args),
                            );
                        }

                        _ => todo!(
                            "unimplemented structure: {:?} in {:?}, of {:?}",
                            instdata,
                            inst,
                            self.fun.name
                        ),
                    }
                }

                // Use the lirs, actually put instruction indices.
                for l in lirs {
                    let new_id = Inst(self.instdata.len());
                    self.instdata.push(l);
                    s.lir_inst.push(new_id);
                    s.is_lowered = true;
                }
            }
        }
    }

    pub fn prune(&mut self) {
        todo!("remove unused instructions, blocks")
    }

    pub fn reg_wrapper(&self) -> RegWrapper {
        RegWrapper::new(&self)
    }

    pub fn inst_data(&self, inst: Inst) -> Option<&InstructionData> {
        self.instdata.get(inst.0)
    }

    pub fn apply_regalloc(&mut self, wrapper: &RegWrapper, regs: &RegOutput) {
        use cg::Reg;

        // Call convention is kinda hardcoded on the regalloc side...
        // reg0 is arg 0; EDI, Diane's
        // reg1 is arg 1; ESI, Silk
        // reg2 is arg 2; EDX, Dress
        // reg3 is arg 3; ECX, Costs
        // reg4 is arg 4; r8?, 8
        // reg5 is arg 5; r9?, 9$

        // And regalloc uses 'n' registers from 0..n.
        // So we need to map those to something.

        let regmap = [
            Reg::EDI, // PReg(0),
            Reg::ESI, // PReg(1),
            Reg::EDX, // PReg(2),
            Reg::ECX, // PReg(3),
                      // Reg::R8,  // PReg(4),
                      // Reg::R9,  // PReg(5),
                      // Reg::R10, Reg::R11, Reg::R12, Reg::R13, Reg::R14, Reg::R15
        ];
        let rg2x = |p: regalloc2::PReg| regmap[p.index()];

        // Loop must match the order in the wrapper creator.
        for (reginst, info) in wrapper.inst_info.iter() {
            println!("regs: {regs:#?}");
            match info.inst {
                LirOrIrInst::Lir(inst) => {
                    let inst_data = &mut self.instdata[inst.0];
                    if !inst_data.has_virtuals() {
                        continue;
                    }
                    println!("@ {reginst:?} inst_data: {inst_data:#?}");
                    let allocs_args = regs.inst_allocs(*reginst);
                    let use_allocs = &allocs_args[0..];
                    println!("use_allocs.len: {}", use_allocs.len());
                    let mut index = 0;
                    // let use_allocs = &allocs_args[0..inst_data.use_operands.len()];
                    for cont in [&mut inst_data.use_operands, &mut inst_data.def_operands] {
                        for z in cont.iter_mut() {
                            match z {
                                LirOperand::Virtual(_v) => {
                                    *z = LirOperand::Machine(cg::Operand::Reg(rg2x(
                                        use_allocs[index].as_reg().unwrap(),
                                    )));
                                    index += 1;
                                }
                                _ => {}
                            }
                        }
                    }
                    // println!("after: {inst_data:#?}, index: {index} ops: {}", info.operands.len());
                    if index != use_allocs.len() {
                        panic!();
                    }
                }
                LirOrIrInst::Ir(inst) => {
                    println!("reginst: {reginst:?}");
                    let allocs_args = regs.inst_allocs(*reginst);
                    let allocs = &allocs_args[0..];

                    // Find the section that has this instruction.
                    let mut block_si_and_ii = None;
                    for (bi, b) in self.blocks.iter().enumerate() {
                        for (si, s) in b.sections.iter().enumerate() {
                            for (ii, i) in s.ir_inst.iter().enumerate() {
                                if *i == inst {
                                    block_si_and_ii = Some((bi, si, ii));
                                }
                            }
                        }
                    }
                    let (bi, si, ii) =
                        block_si_and_ii.expect("could not find expected instruction");
                    self.blocks[bi].sections[si].ir_regs.resize(ii + 1, vec![]);
                    self.blocks[bi].sections[si].ir_regs[ii] =
                        allocs.iter().map(|v| rg2x(v.as_reg().unwrap())).collect();
                }
            }
        }
    }

    pub fn lower_second(&mut self) {
        let new_op = InstructionData::new;
        use cg::{Op, Operand, Width};

        for b in self.blocks.iter_mut() {
            for s in b.sections.iter_mut() {
                for (ii, linst) in s.lir_inst.iter().enumerate() {
                    let instdata = &mut self.instdata[linst.0];
                    match instdata.operation {
                        // Only things to do for jne, we need to split that into the actual blocks.
                        cg::Op::Jcc(jump_condition) => {
                            // Need to know what blocks this relates to, and what registers we got assigned there.
                            error!("instdata: {:#?}", instdata);
                            // The test is already done, so we only need to make jump and movs to the block destinations.
                            // Obtain the IR brif.
                            let mut branch_true_args;
                            let mut branch_true_block;
                            let mut branch_false_args;
                            let mut branch_false_block;
                            if let ir::InstructionData::Brif {
                                opcode,
                                arg,
                                blocks,
                            } = self.fun.dfg.insts[s.ir_inst[0]]
                            {
                                branch_true_args =
                                    blocks[0].args_slice(&self.fun.dfg.value_lists).len();
                                branch_true_block = blocks[0].block(&self.fun.dfg.value_lists);
                                branch_false_args =
                                    blocks[1].args_slice(&self.fun.dfg.value_lists).len();
                                branch_false_block = blocks[0].block(&self.fun.dfg.value_lists);
                            } else {
                                panic!("could not find brif for jcc");
                            }
                            // Now... we need to handle the setup of the block we jump to.
                            // Two sections, jump over the first if false.
                            //   Section A:
                            //     Setup to jump into the true block
                            //     <At end here, jump to true block>
                            //   Section B:
                            //     Setup to jump into the false block
                            //     <At end here, jump to false block>
                            todo!("the block doesn't specify into which registers block args go? How do we reconcile this?")
                        }
                        _ => {}
                    }
                }

                if s.is_lowered() {
                    continue;
                }
                let mut lirs = vec![];
                for (ii, inst) in s.ir_inst.iter().enumerate() {
                    let instdata = self.fun.dfg.insts[*inst];
                    match instdata {
                        IrInstructionData::MultiAry { opcode, args } => match opcode {
                            ir::Opcode::Return => {
                                if args.len(&self.fun.dfg.value_lists) != 1 {
                                    todo!()
                                }

                                lirs.push(
                                    new_op(Op::Mov(Width::W64))
                                        .with_use(&[Operand::Reg(s.ir_regs[ii][0])])
                                        .with_def(&[Operand::Reg(cg::Reg::EAX)]),
                                );
                                lirs.push(new_op(Op::Return));
                            }
                            _ => todo!(
                                "unimplemented opcode: {:?} in {:?}, of {:?}",
                                opcode,
                                inst,
                                self.fun.name
                            ),
                        },
                        IrInstructionData::Brif {
                            opcode,
                            arg,
                            blocks,
                        } => {
                            error!("ir_regs: {:#?}", s.ir_regs);

                            todo!();
                        }
                        _ => todo!(
                            "unimplemented structure: {:?} in {:?}, of {:?}",
                            instdata,
                            inst,
                            self.fun.name
                        ),
                    }
                }
                // Use the lirs, actually put instruction indices.
                for l in lirs {
                    let new_id = Inst(self.instdata.len());
                    self.instdata.push(l);
                    s.lir_inst.push(new_id);
                    s.is_lowered = true;
                }
            }
        }
    }

    pub fn patch_operations(&mut self) {
        let new_op = InstructionData::new;
        use cg::{Op, Operand, Width};

        for b in self.blocks.iter_mut() {
            for s in b.sections.iter_mut() {
                let mut new_inst = vec![];
                for (si, sint) in s.lir_inst.iter().enumerate() {
                    let instdata = &mut self.instdata[sint.0];
                    match instdata.operation {
                        cg::Op::IAdd(_) | cg::Op::ISub(_) | cg::Op::IMul(_) => {
                            let dest = instdata.def_operands[0];
                            let src0 = instdata.use_operands[0];
                            // let src1 = instdata.use_operands[1];

                            // Insert a new move that moves src0 into dest.
                            instdata.use_operands.remove(0);
                            new_inst.push((
                                si,
                                new_op(Op::Mov(Width::W64))
                                    .with_use(&[src0])
                                    .with_def(&[dest]),
                            ));
                        }
                        _ => {}
                    }
                }
                new_inst.reverse();
                for (si, d) in new_inst {
                    let new_id = Inst(self.instdata.len());
                    s.lir_inst.insert(si, new_id);
                    self.instdata.push(d);
                }
            }
        }
    }

    pub fn assemble(&self) -> Vec<u8> {
        let mut v = vec![];

        for b in self.blocks.iter() {
            for s in b.sections.iter() {
                for i in s.lir_inst.iter() {
                    debug!("assembling: {:?}", self.instdata[i.0]);
                    let mut z = self.instdata[i.0].assemble().unwrap();
                    v.append(&mut z);
                }
            }
        }
        v
    }
}

// use cranelift_codegen::ir::Inst as IrInst;
use regalloc2::Block as RegBlock;
use regalloc2::Function as RegFunction;
use regalloc2::Inst as RegInst;
use regalloc2::Operand as RegOperand;
use regalloc2::Output as RegOutput;
use regalloc2::{InstRange, PRegSet, RegClass, VReg};

use std::collections::HashMap;

#[derive(Debug)]
enum LirOrIrInst {
    Lir(Inst),
    Ir(IrInst),
}
impl From<Inst> for LirOrIrInst {
    fn from(v: Inst) -> LirOrIrInst {
        LirOrIrInst::Lir(v)
    }
}
impl From<IrInst> for LirOrIrInst {
    fn from(v: IrInst) -> LirOrIrInst {
        LirOrIrInst::Ir(v)
    }
}

#[derive(Debug)]
struct InstInfo {
    is_ret: bool,
    is_branch: bool,
    operands: Vec<RegOperand>,
    inst: LirOrIrInst,
}

#[derive(Debug)]
pub struct RegWrapper {
    num_insts: usize,
    num_blocks: usize,
    num_values: usize,
    entry_block: RegBlock,
    block_insn: HashMap<RegBlock, InstRange>,
    block_params: HashMap<RegBlock, Vec<VReg>>,
    block_succs: HashMap<RegBlock, Vec<RegBlock>>,
    block_preds: HashMap<RegBlock, Vec<RegBlock>>,
    inst_info: HashMap<RegInst, InstInfo>,
    value_info: HashMap<Value, VReg>,
}

impl RegWrapper {
    pub fn new(lirfun: &Function) -> Self {
        let mut block_insn: HashMap<RegBlock, InstRange> = Default::default();
        let mut block_params: HashMap<RegBlock, Vec<VReg>> = Default::default();
        let mut block_succs: HashMap<RegBlock, Vec<RegBlock>> = Default::default();
        let mut block_preds: HashMap<RegBlock, Vec<RegBlock>> = Default::default();
        let mut inst_info: HashMap<RegInst, InstInfo> = Default::default();
        let mut value_info: HashMap<Value, VReg> = Default::default();

        let fun = &lirfun.fun;
        let entry_block = RegBlock::new(
            lirfun
                .entry_block
                .expect("regalloc function must have entry block")
                .0,
        );

        let mut first_inst_in_fun = None;

        let last_block = lirfun.blocks.last().unwrap().id.0;
        for b in lirfun.blocks.iter() {
            let regblock = RegBlock::new(b.id.0);
            // let is_entry_block = regblock == entry_block;
            //let is_last_block = last_block == b.id.0;

            let mut first_inst = None;
            let mut last_inst = None;

            for s in b.sections.iter() {
                if s.is_lowered() {
                    // if it is lowered, we operate on the LIR.
                    for inst in s.lir_inst.iter() {
                        let data = lirfun.inst_data(*inst).expect("ill formed function");
                        let mut operands = vec![];
                        for z in data.use_operands.iter() {
                            match z {
                                LirOperand::Virtual(v) => {
                                    // Should do something here with retrieving the constraints of the opcode
                                    let regtype = RegClass::Int;
                                    let vreg = value_info
                                        .entry(*v)
                                        .or_insert_with(|| VReg::new(v.as_u32() as usize, regtype));
                                    let operand = RegOperand::new(
                                        *vreg,
                                        regalloc2::OperandConstraint::Any,
                                        regalloc2::OperandKind::Use,
                                        regalloc2::OperandPos::Early,
                                    );
                                    operands.push(operand);
                                }
                                LirOperand::Machine(r) => {
                                    match r {
                                        cg::Operand::Immediate(_) => {
                                            // Not actually an operand for register allocation.
                                        }
                                        cg::Operand::Reg(r) => {
                                            todo!("{r:?}");
                                        }
                                    }
                                }
                            }
                        }
                        for z in data.def_operands.iter() {
                            match z {
                                LirOperand::Virtual(v) => {
                                    // Should do something here with retrieving the constraints of the opcode
                                    let regtype = RegClass::Int;
                                    let vreg = value_info
                                        .entry(*v)
                                        .or_insert_with(|| VReg::new(v.as_u32() as usize, regtype));
                                    let operand = RegOperand::new(
                                        *vreg,
                                        regalloc2::OperandConstraint::Any,
                                        regalloc2::OperandKind::Def,
                                        regalloc2::OperandPos::Late,
                                    );
                                    operands.push(operand);
                                }
                                LirOperand::Machine(r) => {
                                    match r {
                                        cg::Operand::Immediate(_) => {
                                            // Not actually an operand for register allocation.
                                        }
                                        cg::Operand::Reg(r) => {
                                            todo!("{r:?} for {data:?}");
                                        }
                                    }
                                }
                            }
                        }

                        let reg_inst = RegInst::new(inst_info.len());
                        if first_inst.is_none() {
                            first_inst = Some(reg_inst);
                        }
                        if first_inst_in_fun.is_none() {
                            first_inst_in_fun = Some(reg_inst);
                        }
                        last_inst = Some(reg_inst);
                        println!("assigning {last_inst:?}");

                        let is_ret = data.operation.is_return();
                        let is_branch = data.operation.is_branch();
                        let info = InstInfo {
                            is_ret,
                            is_branch,
                            operands,
                            inst: (*inst).into(),
                        };
                        inst_info.insert(reg_inst, info);
                    }
                } else {
                    // if it is not lowered, we operate on the IR.
                    for irinst in s.ir_inst.iter() {
                        let instdata = fun.dfg.insts[*irinst];

                        let mut operands = vec![];

                        // Input arguments to instruction.
                        let arguments = instdata.arguments(&fun.dfg.value_lists);
                        // println!("Adding arguments to ir inst: {arguments:?} {irinst:?}");
                        for v in arguments {
                            let valuetype = fun.dfg.value_type(*v);
                            let regtype = if valuetype.is_int() {
                                RegClass::Int
                            } else {
                                RegClass::Float
                            };
                            let operand = RegOperand::new(
                                VReg::new(v.as_u32() as usize, regtype),
                                regalloc2::OperandConstraint::Any,
                                regalloc2::OperandKind::Use,
                                regalloc2::OperandPos::Early,
                            );
                            operands.push(operand);
                        }

                        // Results of instruction.
                        for r in fun.dfg.inst_results(*irinst) {
                            let valuetype = fun.dfg.value_type(*r);
                            let regtype = if valuetype.is_int() {
                                RegClass::Int
                            } else {
                                RegClass::Float
                            };
                            let operand = RegOperand::new(
                                VReg::new(r.as_u32() as usize, regtype),
                                regalloc2::OperandConstraint::Any,
                                regalloc2::OperandKind::Def,
                                regalloc2::OperandPos::Late,
                            );
                            operands.push(operand);
                        }

                        let is_ret = instdata.opcode().is_return();
                        let is_branch = instdata.opcode().is_branch();
                        let info = InstInfo {
                            is_ret,
                            is_branch,
                            operands,
                            inst: (*irinst).into(),
                        };
                        println!(" irinst {irinst:?} with {info:?}");
                        let reg_inst = RegInst::new(inst_info.len());
                        if first_inst.is_none() {
                            first_inst = Some(reg_inst);
                        }
                        if first_inst_in_fun.is_none() {
                            first_inst_in_fun = Some(reg_inst);
                        }
                        last_inst = Some(reg_inst);

                        inst_info.insert(reg_inst, info);
                    }
                }
            }

            // WHY!?
            error!("seriously wonky +1 here to ensure register allocation works");
            // Does it relate to
            // https://github.com/bytecodealliance/regalloc2/blob/925df1b4674435a9322e21912926a68749517861/src/lib.rs#L1514-L1522
            //let last_plus_one = last_inst.map(|v| RegInst::new(v.0 as usize + if !is_last_block {1}else{0}));

            let last_plus_one = last_inst.map(|v| RegInst::new(v.0 as usize + 1));

            let range = InstRange::new(
                first_inst.expect("block should have instruction"),
                last_plus_one.expect("block should have instruction"),
            );
            println!("Assigning {regblock:?} with {range:?}");
            block_insn.insert(regblock, range);

            block_succs.insert(
                regblock,
                b.block_succs.iter().map(|v| RegBlock::new(v.0)).collect(),
            );

            block_preds.insert(
                regblock,
                b.block_preds.iter().map(|v| RegBlock::new(v.0)).collect(),
            );

            let these_block_params = block_params.entry(regblock).or_default();
            for v in b.block_params.iter() {
                let regtype = RegClass::Int;
                let vreg = value_info
                    .entry(*v)
                    .or_insert_with(|| VReg::new(v.as_u32() as usize, regtype));
                these_block_params.push(*vreg);
            }
        }

        // Find the first instruction in the entry block, into that instruction we'll inject the
        // function arguments.
        if let Some(reg_inst) = first_inst_in_fun {
            let first_info = inst_info
                .get_mut(&reg_inst)
                .expect("no info for first instruction");
            for (i, v) in lirfun.fun_args.iter().enumerate() {
                let regtype = RegClass::Int;

                let vreg = value_info
                    .entry(*v)
                    .or_insert_with(|| VReg::new(v.as_u32() as usize, regtype));
                let preg = regalloc2::PReg::new(i, regtype);

                println!("Note to self, hardcoded call convention; {vreg:?} -> {preg:?}");
                let operand = RegOperand::new(
                    *vreg,
                    regalloc2::OperandConstraint::FixedReg(preg),
                    regalloc2::OperandKind::Def,
                    regalloc2::OperandPos::Early,
                );

                first_info.operands.push(operand);
            }
        } else {
            panic!()
        }

        let num_insts = inst_info.len();
        let num_blocks = block_insn.len();
        let num_values = value_info.len();
        println!("block_insn: {block_insn:#?}");

        Self {
            num_insts,
            num_blocks,
            num_values,
            entry_block,
            block_insn,
            block_params,
            block_succs,
            block_preds,
            inst_info,
            value_info,
        }
    }
}

impl RegFunction for RegWrapper {
    fn num_insts(&self) -> usize {
        self.num_insts
    }
    fn num_blocks(&self) -> usize {
        self.num_blocks
    }
    fn entry_block(&self) -> RegBlock {
        self.entry_block
    }
    fn block_insns(&self, block: regalloc2::Block) -> InstRange {
        self.block_insn[&block]
    }
    fn block_succs(&self, block: regalloc2::Block) -> &[regalloc2::Block] {
        &self.block_succs[&block]
    }
    fn block_preds(&self, block: regalloc2::Block) -> &[regalloc2::Block] {
        &self.block_preds[&block]
    }
    fn block_params(&self, block: regalloc2::Block) -> &[VReg] {
        &self.block_params[&block]
    }
    fn is_ret(&self, reginst: regalloc2::Inst) -> bool {
        self.inst_info[&reginst].is_ret
    }
    fn is_branch(&self, reginst: regalloc2::Inst) -> bool {
        self.inst_info[&reginst].is_branch
    }
    fn branch_blockparams(&self, _: regalloc2::Block, _: regalloc2::Inst, _: usize) -> &[VReg] {
        todo!()
    }
    fn inst_operands(&self, reginst: regalloc2::Inst) -> &[regalloc2::Operand] {
        &self.inst_info[&reginst].operands
    }
    fn inst_clobbers(&self, _inst: regalloc2::Inst) -> PRegSet {
        // PRegSet::default()
        todo!()
    }
    fn num_vregs(&self) -> usize {
        self.num_values
    }
    fn spillslot_size(&self, _: RegClass) -> usize {
        todo!()
    }
}
