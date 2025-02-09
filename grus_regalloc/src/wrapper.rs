use cranelift_codegen::ir::Function as CraneliftIrFunction;
// use cranelift_codegen::ir::Block as IrBlock;
use cranelift_codegen::ir::Inst as IrInst;
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
pub struct IrFunction {
    num_insts: usize,
    num_blocks: usize,
    num_values: usize,
    entry_block: RegBlock,
    block_insn: HashMap<RegBlock, InstRange>,
    block_params: HashMap<RegBlock, Vec<VReg>>,
    inst_info: HashMap<RegInst, InstInfo>,
}

impl IrFunction {
    pub fn new(fun: &CraneliftIrFunction) -> Self {
        let num_insts = fun.stencil.dfg.num_insts();
        let num_blocks = fun.stencil.dfg.num_blocks();
        let num_values = fun.dfg.num_values();
        let entry_block = RegBlock::new(
            fun.layout
                .entry_block()
                .expect("regalloc function must have entry block")
                .as_u32() as usize,
        );

        let mut block_insn: HashMap<RegBlock, InstRange> = Default::default();
        let mut block_params: HashMap<RegBlock, Vec<VReg>> = Default::default();

        for cbl in fun.layout.blocks() {
            // let cbl = IrBlock::from_u32(block.raw_u32());
            let regblock = RegBlock::new(cbl.as_u32() as usize);
            // let block = &fun.dfg.blocks[cbl];

            for v in fun.dfg.block_params(cbl) {
                let valuetype = fun.dfg.value_type(*v);
                let regtype = if valuetype.is_int() {
                    RegClass::Int
                } else {
                    RegClass::Float
                };
                let vreg = VReg::new(v.as_u32() as usize, regtype);
                let these_block_params = block_params.entry(regblock).or_default();
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
            let start = actual_instructions.first().unwrap();
            let last = actual_instructions.last().unwrap();
            let start = RegInst::new(start.as_u32() as usize);
            let last = RegInst::new(last.as_u32() as usize);
            block_insn.insert(regblock, InstRange::new(start, last));
        }

        let mut inst_info: HashMap<RegInst, InstInfo> = Default::default();
        for cbl in fun.layout.blocks() {
            for irinst in fun.layout.block_insts(cbl) {
                let instdata = fun.dfg.insts[irinst];

                let mut operands = vec![];

                // Input arguments to instruction.
                let arguments = instdata.arguments(&fun.dfg.value_lists);
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
                let reg_inst = RegInst::new(irinst.as_u32() as usize);
                inst_info.insert(reg_inst, info);
            }
        }

        Self {
            num_insts,
            num_blocks,
            num_values,
            entry_block,
            block_insn,
            block_params,
            inst_info,
        }
    }
}

impl RegFunction for IrFunction {
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
    fn block_succs(&self, _: regalloc2::Block) -> &[regalloc2::Block] {
        todo!()
    }
    fn block_preds(&self, _: regalloc2::Block) -> &[regalloc2::Block] {
        todo!()
    }
    fn block_params(&self, block: regalloc2::Block) -> &[VReg] {
        // Stole this trick from cranelift, entry block has no parameters but we insert a first
        // instruction that populates registers with the input arguments.
        if block == self.entry_block() {
            return &[];
        }
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
    fn inst_clobbers(&self, _: regalloc2::Inst) -> PRegSet {
        todo!()
    }
    fn num_vregs(&self) -> usize {
        self.num_values
    }
    fn spillslot_size(&self, _: RegClass) -> usize {
        todo!()
    }
}
