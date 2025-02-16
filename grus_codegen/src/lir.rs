#![allow(dead_code)]
#![allow(unused_imports)]
use cranelift_codegen::ir::Function as CraneliftIrFunction;
use cranelift_codegen::ir::Inst as IrInst;
use cranelift_codegen::ir::InstructionData as IrInstructionData;
use cranelift_codegen::ir::{self, Value};
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
        - Lower; Creates hw instructions for each section, without register allocations.
        - (register allocation?)
        - Can create bytecode for sections now.
        - Something with jumps and offsets.
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

use crate::codegen as cg;

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
    Machine(crate::codegen::Operand),
}
impl From<Value> for LirOperand {
    fn from(v: Value) -> LirOperand {
        LirOperand::Virtual(v)
    }
}
impl From<crate::codegen::Operand> for LirOperand {
    fn from(v: crate::codegen::Operand) -> LirOperand {
        LirOperand::Machine(v)
    }
}

#[derive(Clone, Debug)]
pub struct InstructionData {
    pub operation: crate::codegen::Op,
    // Should we have def_operand and use_operand?
    pub def_operands: Vec<LirOperand>,
    pub use_operands: Vec<LirOperand>,
}
impl InstructionData {
    pub fn new(operation: crate::codegen::Op) -> Self {
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
}

#[derive(Clone, Debug)]
struct Section {
    lir_inst: Vec<LirInst>,
    ir_inst: Vec<IrInst>,
}
impl Section {
    fn from_ir(v: IrInst) -> Self {
        Self {
            lir_inst: vec![],
            ir_inst: vec![v],
        }
    }
}

#[derive(Clone, Debug)]
struct Block {
    sections: Vec<Section>,
    id: BlockId,
}
impl Block {
    fn new(id: BlockId) -> Self {
        Self {
            sections: vec![],
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
}

impl Function {
    pub fn from_ir(fun: &CraneliftIrFunction) -> Self {
        Self {
            blocks: vec![],
            instdata: vec![],
            entry_block: None,
            call_convention: CallConv::SystemV,
            fun: fun.clone(),
        }
    }

    pub fn lirify(&mut self) {
        let dfg = &self.fun.stencil.dfg;
        let layout = &self.fun.stencil.layout;

        for b in layout.blocks() {
            let lirblockid = BlockId::from(b.as_u32() as usize);
            let mut lirblock = Block::new(lirblockid);

            debug!("b: {b:?}");
            let block_data = &dfg.blocks[b];
            debug!(
                "block_data params: {:?}",
                block_data.params(&dfg.value_lists)
            );

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
    pub fn lower(&mut self) {
        let new_op = InstructionData::new;
        use crate::codegen::{Op, Operand, Width};
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
                                lirs.push(
                                    new_op(Op::Mov(Width::W64))
                                        .with_use(&[use_ir[0]])
                                        .with_def(&[Operand::Reg(crate::codegen::Reg::EAX)]),
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

                        IrInstructionData::Binary { opcode, args: _ } => match opcode {
                            ir::Opcode::Iadd => {
                                let width: Width = typevar_operand
                                    .as_ref()
                                    .map(type_of)
                                    .map(|z| z.into())
                                    .unwrap_or(Width::W64);

                                // x86 add overwrites the first operand, so for now:
                                //    Move operand one into destination.
                                //    Add operand two to destination.
                                lirs.push(
                                    new_op(Op::Mov(width))
                                        .with_use(&[use_ir[0]])
                                        .with_def(&[def_ir[0]]),
                                );
                                lirs.push(
                                    new_op(Op::IAdd(width))
                                        .with_use(&[def_ir[0], use_ir[1]])
                                        .with_def(&[def_ir[0]]),
                                );
                            }
                            /*
                            ir::Opcode::Isub => {
                                let width: Width = typevar_operand
                                    .as_ref()
                                    .map(type_of)
                                    .with_context(|| "could not determine width")?
                                    .into();

                                // x86 add overwrites the first operand, so for now:
                                //    Move operand one into destination.
                                //    Add operand two to destination.
                                let dest = Operand::Reg(rg2x(def_allocs[0].as_reg().unwrap()));
                                let xinst = cg::Instruction::op(
                                    Op::Mov(width),
                                    &[dest, Operand::Reg(rg2x(use_allocs[0].as_reg().unwrap()))],
                                );
                                buffer.append(&mut xinst.serialize()?);
                                let xinst = cg::Instruction::op(
                                    Op::ISub(width),
                                    &[dest, Operand::Reg(rg2x(use_allocs[1].as_reg().unwrap()))],
                                );
                                buffer.append(&mut xinst.serialize()?);
                            }
                            ir::Opcode::Imul => {
                                let width: Width = typevar_operand
                                    .as_ref()
                                    .map(type_of)
                                    .with_context(|| "could not determine width")?
                                    .into();

                                // x86 integer mul supports three forms, we use the two-operand
                                // version here, this multiplies into the destination.
                                let dest = Operand::Reg(rg2x(def_allocs[0].as_reg().unwrap()));
                                let xinst = cg::Instruction::op(
                                    Op::Mov(width),
                                    &[dest, Operand::Reg(rg2x(use_allocs[0].as_reg().unwrap()))],
                                );
                                buffer.append(&mut xinst.serialize()?);
                                let xinst = cg::Instruction::op(
                                    Op::IMul(width),
                                    &[dest, Operand::Reg(rg2x(use_allocs[1].as_reg().unwrap()))],
                                );
                                buffer.append(&mut xinst.serialize()?);
                            }*/
                            _ => todo!(
                                "unimplemented opcode: {:?} in {:?}, of {:?}",
                                opcode,
                                inst,
                                self.fun.name
                            ),
                        },
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
}

// use cranelift_codegen::ir::Inst as IrInst;
use regalloc2::Block as RegBlock;
use regalloc2::Function as RegFunction;
use regalloc2::Inst as RegInst;
use regalloc2::Operand as RegOperand;
use regalloc2::{InstRange, PRegSet, RegClass, VReg};

use std::collections::HashMap;

#[derive(Debug)]
struct InstInfo {
    is_ret: bool,
    is_branch: bool,
    operands: Vec<RegOperand>,
    inst: Inst,
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

        for b in lirfun.blocks.iter() {
            for s in b.sections.iter() {
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
                                todo!()
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
                                todo!()
                            }
                        }
                    }

                    let reg_inst = RegInst::new(inst_info.len());

                    let is_ret = data.operation.is_return();
                    let is_branch = data.operation.is_branch();
                    let info = InstInfo {
                        is_ret,
                        is_branch,
                        operands,
                        inst: *inst,
                    };
                    // println!(" irinst {irinst:?} with {info:?}");
                    inst_info.insert(reg_inst, info);
                }
            }
        }

        let num_insts = fun.stencil.dfg.num_insts();
        let num_blocks = fun.stencil.dfg.num_blocks();
        let num_values = fun.dfg.num_values();

        let cfg = cranelift_codegen::flowgraph::ControlFlowGraph::with_function(fun);

        for cbl in fun.layout.blocks() {
            // let cbl = IrBlock::from_u32(block.raw_u32());
            let regblock = RegBlock::new(cbl.as_u32() as usize);
            // let block = &fun.dfg.blocks[cbl];

            // Store all block parameter entries.
            let these_block_params = block_params.entry(regblock).or_default();
            for v in fun.dfg.block_params(cbl) {
                let valuetype = fun.dfg.value_type(*v);
                let regtype = if valuetype.is_int() {
                    RegClass::Int
                } else {
                    RegClass::Float
                };
                let vreg = VReg::new(v.as_u32() as usize, regtype);
                these_block_params.push(vreg);
            }

            let mut actual_instructions: Vec<IrInst> = vec![];
            for i in fun.layout.block_insts(cbl) {
                if let Some(previous) = actual_instructions.last() {
                    let prior_i = previous.as_u32();
                    let current = i.as_u32();
                    if prior_i + 1 != current {
                        panic!("block doesn't have consecutive range");
                    }
                }
                actual_instructions.push(i);
            }
            println!("actual_instructions: {actual_instructions:?}");
            let start = actual_instructions.first().unwrap();
            let last = actual_instructions.last().unwrap();
            let start = RegInst::new(start.as_u32() as usize);
            let last = RegInst::new(last.as_u32() as usize + 1);
            block_insn.insert(regblock, InstRange::new(start, last));

            block_succs.insert(
                regblock,
                cfg.succ_iter(cbl)
                    .map(|v| {
                        // let block = fun.layout.inst_block(&v.block).expect("instruction doesn't have block");
                        RegBlock::new(v.as_u32() as usize)
                    })
                    .collect(),
            );
            block_preds.insert(
                regblock,
                cfg.pred_iter(cbl)
                    .map(|v| {
                        // let block = fun.layout.inst_block(&v.block).expect("instruction doesn't have block");
                        RegBlock::new(v.block.as_u32() as usize)
                    })
                    .collect(),
            );
        }

        // Find the first instruction in the entry block, into that instruction we'll inject the
        // function arguments.
        if let Some(first_insn) = fun
            .layout
            .first_inst(fun.layout.entry_block().expect("should have entry block"))
        {
            let reg_inst = RegInst::new(first_insn.as_u32() as usize);
            let first_info = inst_info
                .get_mut(&reg_inst)
                .expect("no info for first instruction");
            let ir_entry_block = fun.layout.entry_block().expect("should have entry block");
            for (i, v) in fun.dfg.block_params(ir_entry_block).iter().enumerate() {
                let valuetype = fun.dfg.value_type(*v);
                let regtype = if valuetype.is_int() {
                    RegClass::Int
                } else {
                    RegClass::Float
                };
                let vreg = VReg::new(v.as_u32() as usize, regtype);
                let preg = regalloc2::PReg::new(i, regtype);

                println!("Note to self, hardcoded call convention; {vreg:?} -> {preg:?}");
                let operand = RegOperand::new(
                    vreg,
                    regalloc2::OperandConstraint::FixedReg(preg),
                    regalloc2::OperandKind::Def,
                    regalloc2::OperandPos::Early,
                );

                first_info.operands.push(operand);
            }
        }

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
