use cranelift_codegen::ir;
use cranelift_codegen::ir::Block as IrBlock;
use cranelift_codegen::ir::Function as CraneliftIrFunction;
use cranelift_codegen::ir::Inst as IrInst;
use regalloc2::Function as RegFunction;
use regalloc2::Inst as RegInst;
use regalloc2::Output as RegOutput;
use regalloc2::{InstRange, MachineEnv, PRegSet, RegAllocError, RegClass, VReg};

// Similar function signature as regalloc2 itself.
pub fn run<F: RegFunction>(func: &F, env: &MachineEnv) -> Result<RegOutput, RegAllocError> {
    let _ = (func, env);
    todo!();
}

pub struct IrFunction {
    fun: CraneliftIrFunction,
}

// Here, we implement the Irfunction for the RegFunction... Such that we can interact with the
// IrFunction, and use the datastructure used in regalloc, such that we can eventually compare
// our output with its output easily.

impl RegFunction for IrFunction {
    fn num_insts(&self) -> usize {
        self.fun.stencil.dfg.num_insts()
    }
    fn num_blocks(&self) -> usize {
        self.fun.stencil.dfg.num_blocks()
    }
    fn entry_block(&self) -> regalloc2::Block {
        regalloc2::Block::new(
            self.fun
                .layout
                .entry_block()
                .expect("regalloc function must have entry block")
                .as_u32() as usize,
        )
    }
    fn block_insns(&self, block: regalloc2::Block) -> InstRange {
        let cbl = IrBlock::from_u32(block.raw_u32());
        let mut actual_instructions: Vec<IrInst> = vec![];
        for i in self.fun.layout.block_insts(cbl) {
            if let Some(previous) = actual_instructions.first() {
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
        InstRange::new(start, last)
    }
    fn block_succs(&self, _: regalloc2::Block) -> &[regalloc2::Block] {
        todo!()
    }
    fn block_preds(&self, _: regalloc2::Block) -> &[regalloc2::Block] {
        todo!()
    }
    fn block_params(&self, _: regalloc2::Block) -> &[VReg] {
        todo!()
    }
    fn is_ret(&self, reginst: regalloc2::Inst) -> bool {
        let irinst = IrInst::from_u32(reginst.raw_u32() as u32);
        let instdata = self.fun.dfg.insts[irinst];
        instdata.opcode() == ir::Opcode::Return
    }
    fn is_branch(&self, _: regalloc2::Inst) -> bool {
        todo!()
    }
    fn branch_blockparams(&self, _: regalloc2::Block, _: regalloc2::Inst, _: usize) -> &[VReg] {
        todo!()
    }
    fn inst_operands(&self, reginst: regalloc2::Inst) -> &[regalloc2::Operand] {
        let irinst = IrInst::from_u32(reginst.raw_u32() as u32);
        let instdata = self.fun.dfg.insts[irinst];

        let arguments = instdata.arguments(&self.fun.dfg.value_lists);
        // we can't return a vector here.... since this takes a slice.

        // new(
        // vreg: VReg,
        // constraint: OperandConstraint,
        // kind: OperandKind,
        // pos: OperandPos,
        // )
        todo!()
    }
    fn inst_clobbers(&self, _: regalloc2::Inst) -> PRegSet {
        todo!()
    }
    fn num_vregs(&self) -> usize {
        self.fun.dfg.num_values()
    }
    fn spillslot_size(&self, _: RegClass) -> usize {
        todo!()
    }
}
