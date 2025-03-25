use regalloc2::{
    Block, Function, Inst, InstRange, MachineEnv, Operand, Output, PReg, PRegSet, RegClass, VReg,
};

use std::collections::HashMap;

#[derive(Clone)]
struct InstructionData {
    is_return: bool,
    is_branch: bool,
    operands: Vec<Operand>,
}

struct DummyFunction {
    entry_block: Block,
    blocks: HashMap<Block, InstRange>,
    block_succs: HashMap<Block, Vec<Block>>,
    block_preds: HashMap<Block, Vec<Block>>,
    instructions: Vec<InstructionData>,
    num_values: usize,
}

impl Function for DummyFunction {
    fn num_insts(&self) -> usize {
        self.instructions.len()
    }
    fn num_blocks(&self) -> usize {
        self.blocks.len() + 10
    }
    fn entry_block(&self) -> Block {
        self.entry_block
    }
    fn block_insns(&self, block: regalloc2::Block) -> InstRange {
        println!("block: {block:?}");
        if let Some(r) = self.blocks.get(&block) {
            self.blocks[&block]
        } else {
            println!("Don't have this block, returning a dummy range");
            InstRange::new(Inst::new(0), Inst::new(1))
        }
    }
    fn block_succs(&self, block: regalloc2::Block) -> &[regalloc2::Block] {
        if let Some(exists) = self.block_succs.get(&block) {
            &exists
        } else {
            &[]
        }
    }
    fn block_preds(&self, block: regalloc2::Block) -> &[regalloc2::Block] {
        if let Some(exists) = self.block_preds.get(&block) {
            &exists
        } else {
            &[]
        }
    }
    fn block_params(&self, block: regalloc2::Block) -> &[VReg] {
        &[]
    }
    fn is_ret(&self, reginst: regalloc2::Inst) -> bool {
        self.instructions[reginst.raw_u32() as usize].is_return
    }
    fn is_branch(&self, reginst: regalloc2::Inst) -> bool {
        self.instructions[reginst.raw_u32() as usize].is_branch
    }
    fn branch_blockparams(
        &self,
        block: regalloc2::Block,
        inst: regalloc2::Inst,
        succs_index: usize,
    ) -> &[VReg] {
        &[]
    }
    fn inst_operands(&self, reginst: regalloc2::Inst) -> &[regalloc2::Operand] {
        &self.instructions[reginst.raw_u32() as usize].operands
    }
    fn inst_clobbers(&self, _inst: regalloc2::Inst) -> PRegSet {
        PRegSet::default()
        //todo!()
    }
    fn num_vregs(&self) -> usize {
        self.num_values
    }
    fn spillslot_size(&self, _: RegClass) -> usize {
        1
    }
}

pub fn simple_int_machine(pref: usize, non_pref: usize) -> MachineEnv {
    let pref_regs = (0..pref)
        .map(|z| PReg::new(z, RegClass::Int))
        .collect::<Vec<_>>();
    let non_pref_regs = (0..non_pref)
        .map(|z| PReg::new(z + pref, RegClass::Int))
        .collect::<Vec<_>>();
    regalloc2::MachineEnv {
        preferred_regs_by_class: [pref_regs, vec![], vec![]],
        non_preferred_regs_by_class: [non_pref_regs, vec![], vec![]],
        scratch_by_class: [None, None, None],
        fixed_stack_slots: vec![],
    }
}

fn main() -> Result<(), anyhow::Error> {
    // This is a bit reduced from what I had originally...
    /*


    test run
    test compile
    target x86_64

    function %branch_diverge(i64, i64) -> i64 system_v {

    block1(v0: i64, v1: i64):
        brif v0, block2, block2

    block2:
        v6 = iconst.i64 1
        v11 = iadd v6, v1
        return v11

    }
    */

    let block1 = Block::new(1);
    let block2 = Block::new(2);
    let v0 = VReg::new(0, RegClass::Int);
    let v1 = VReg::new(1, RegClass::Int);
    let v6 = VReg::new(6, RegClass::Int);
    let v11 = VReg::new(11, RegClass::Int);

    let brif_inst = InstructionData {
        is_return: false,
        is_branch: true,
        operands: vec![Operand::any_use(v0)],
    };

    let assignv6 = InstructionData {
        is_return: false,
        is_branch: false,
        operands: vec![Operand::any_def(v6)],
    };

    let addv11 = InstructionData {
        is_return: false,
        is_branch: false,
        operands: vec![
            Operand::any_def(v11),
            Operand::any_use(v6),
            Operand::any_use(v1),
        ],
    };

    let retv11 = InstructionData {
        is_return: true,
        is_branch: false,
        operands: vec![Operand::any_use(v11)],
    };

    let dummy_bad_block = DummyFunction {
        entry_block: block1,
        blocks: HashMap::from([
            (block1, InstRange::new(Inst::new(0), Inst::new(1))),
            (block2, InstRange::new(Inst::new(1), Inst::new(3))),
        ]),
        block_succs: HashMap::from([(block1, vec![block2]), (block2, vec![])]),
        block_preds: HashMap::from([(block2, vec![block1]), (block1, vec![])]),
        instructions: vec![
            brif_inst.clone(),
            assignv6.clone(),
            addv11.clone(),
            retv11.clone(),
        ],
        num_values: 12, // v0, v1, v6, v11
    };
    let options = regalloc2::RegallocOptions {
        verbose_log: false,
        validate_ssa: true,
        algorithm: regalloc2::Algorithm::Fastalloc,
        //algorithm: regalloc2::Algorithm::Ion,
    };

    let env = simple_int_machine(3, 2);
    let bad_block = regalloc2::run(&dummy_bad_block, &env, &options);
    println!("bad_block: {bad_block:?}");

    // Fix the blocks... then it panics on the values.
    let block0 = Block::new(0);
    let block1 = Block::new(1);

    let dummy_bad_values = DummyFunction {
        entry_block: block0,
        blocks: HashMap::from([
            (block0, InstRange::new(Inst::new(0), Inst::new(1))),
            (block1, InstRange::new(Inst::new(1), Inst::new(3))),
        ]),
        block_succs: HashMap::from([(block0, vec![block1])]),
        block_preds: HashMap::from([(block1, vec![block0])]),
        instructions: vec![brif_inst, assignv6, addv11, retv11],
        num_values: 4, // v0, v1, v6, v11
    };
    let bad_values = regalloc2::run(&dummy_bad_values, &env, &options);
    println!("bad_values: {bad_values:?}");

    let options = regalloc2::RegallocOptions {
        verbose_log: false,
        validate_ssa: true,
        algorithm: regalloc2::Algorithm::Ion,
    };

    // Check ion
    let bad_block = regalloc2::run(&dummy_bad_block, &env, &options);
    println!("bad_block: {bad_block:?}");
    let bad_values = regalloc2::run(&dummy_bad_values, &env, &options);
    println!("bad_values: {bad_values:?}");

    Ok(())
}
