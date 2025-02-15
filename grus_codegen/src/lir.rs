#![allow(dead_code)]
#![allow(unused_imports)]
use cranelift_codegen::ir::Function as CraneliftIrFunction;
use cranelift_codegen::ir::Inst as IrInst;
use cranelift_codegen::ir::Value;
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
*/
use cranelift_codegen::isa::CallConv;

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
struct Inst(usize);
use Inst as LirInst;

#[derive(Copy, Clone, Debug)]
enum LirOperand {
    Virtual(Value),
    Machine(crate::codegen::Operand),
}

#[derive(Clone, Debug)]
struct InstructionData {
    operation: crate::codegen::Op,
    operands: Vec<LirOperand>,
}

#[derive(Clone, Debug)]
struct Section {
    lir_inst: Vec<LirInst>,
    ir_inst: Vec<IrInst>,
}

#[derive(Clone, Debug)]
struct Block {
    sections: Vec<Section>,
}

pub struct Function {
    blocks: Vec<Block>,
    entry_block: BlockId,
    call_convention: CallConv,
    original: CraneliftIrFunction,
}

impl Function {
    pub fn from_ir(fun: &CraneliftIrFunction) -> Self {
        Self {
            blocks: vec![],
            entry_block: BlockId::from(0),
            call_convention: CallConv::SystemV,
            original: fun.clone(),
        }
    }
    pub fn reg_wrapper(&self) -> RegWrapper {
        RegWrapper::new(&self.original)
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
}

impl RegWrapper {
    pub fn new(fun: &CraneliftIrFunction) -> Self {
        let entry_block = RegBlock::new(
            fun.layout
                .entry_block()
                .expect("regalloc function must have entry block")
                .as_u32() as usize,
        );

        let num_insts = fun.stencil.dfg.num_insts();
        let num_blocks = fun.stencil.dfg.num_blocks();
        let num_values = fun.dfg.num_values();

        let mut block_insn: HashMap<RegBlock, InstRange> = Default::default();
        let mut block_params: HashMap<RegBlock, Vec<VReg>> = Default::default();
        let mut block_succs: HashMap<RegBlock, Vec<RegBlock>> = Default::default();
        let mut block_preds: HashMap<RegBlock, Vec<RegBlock>> = Default::default();

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

        let mut inst_info: HashMap<RegInst, InstInfo> = Default::default();
        for cbl in fun.layout.blocks() {
            for irinst in fun.layout.block_insts(cbl) {
                let instdata = fun.dfg.insts[irinst];

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
                for r in fun.dfg.inst_results(irinst) {
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
                };
                println!(" irinst {irinst:?} with {info:?}");
                let reg_inst = RegInst::new(irinst.as_u32() as usize);
                inst_info.insert(reg_inst, info);
            }
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
