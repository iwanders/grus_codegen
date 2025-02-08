use cranelift_codegen::ir;
use regalloc2::Function as RegFunction;
use regalloc2::Output as RegOutput;
use regalloc2::{MachineEnv, PReg, RegAllocError, VReg};

pub mod wrapper;

/*
Okay... so register allocation. 😰
Lets just aim to get something naive working first.
And lets start with local (in-block) allocation only.
*/

use std::collections::HashMap;

pub struct Machine {
    pub preferred_regs_by_class: [HashMap<PReg, Option<VReg>>; 3],
    pub non_preferred_regs_by_class: [HashMap<PReg, Option<VReg>>; 3],
}

impl Machine {
    pub fn new(env: &regalloc2::MachineEnv) -> Self {
        let mut preferred_regs_by_class: [HashMap<PReg, Option<VReg>>; 3] = Default::default();
        let mut non_preferred_regs_by_class: [HashMap<PReg, Option<VReg>>; 3] = Default::default();
        for i in 0..3 {
            for preg in env.preferred_regs_by_class[i].iter() {
                preferred_regs_by_class[i].insert(*preg, None);
            }
            for preg in env.non_preferred_regs_by_class[i].iter() {
                non_preferred_regs_by_class[i].insert(*preg, None);
            }
        }
        Self {
            preferred_regs_by_class,
            non_preferred_regs_by_class,
        }
    }

    pub fn in_register(&self, vreg: VReg) -> Option<PReg> {
        let reg_groups = [
            &self.preferred_regs_by_class,
            &self.non_preferred_regs_by_class,
        ];
        for group in reg_groups {
            for i in 0..3 {
                for (preg, value) in group[i].iter() {
                    if let Some(value) = value.as_ref() {
                        if *value == vreg {
                            return Some(*preg);
                        }
                    }
                }
            }
        }
        None
    }
}

mod winged {
    /*
    Lets just wing it... something like:
        Determine lifetimes of all variables.
            Dead variable registers are obviously free.
        Determine 'hotness' of variables based on whether they are used in the vicinity.
            Keep hot variable in the registers.
        If we run out of registers, the least hot variable goes onto the stack.
    */
    use super::*;

    use regalloc2::{Allocation, Inst, OperandKind, VReg};

    type VarMap = std::collections::HashMap<VReg, VariableState>;

    #[derive(Debug, Clone)]
    struct VariableState {
        /// Range this variable is alive, first duration is write / creation!
        duration: std::ops::RangeInclusive<Inst>,
        /// Instructions for which this variable is used.
        reads: Vec<Inst>,
    }

    impl Machine {
        fn evict_after_inst(&mut self, current_instruction: Inst, varmap: &VarMap) {
            // Iterate over preferred and non preferred maps to evict that which is no longer alive.
            for i in 0..3 {
                for v in self.preferred_regs_by_class[i].values_mut() {
                    if let Some(vreg) = v {
                        let state = &varmap[&vreg];
                        if state.duration.end().0 <= current_instruction.0 {
                            *v = None;
                        }
                    }
                }
            }
        }
    }

    pub fn run<F: RegFunction>(fun: &F, env: &MachineEnv) -> Result<RegOutput, RegAllocError> {
        let mut varmap: VarMap = Default::default();

        // Do a linear pass to populate the variable states.
        let entry_b = fun.entry_block();
        for entry_vreg in fun.block_params(entry_b) {
            let insn = fun.block_insns(entry_b).first();
            varmap.insert(
                *entry_vreg,
                VariableState {
                    duration: insn..=insn,
                    reads: vec![],
                },
            );
        }
        for insn in fun.block_insns(entry_b).iter() {
            let ops = fun.inst_operands(insn);
            for op in ops {
                if op.kind() == OperandKind::Def {
                    varmap.insert(
                        op.vreg(),
                        VariableState {
                            duration: insn..=insn,
                            reads: vec![],
                        },
                    );
                } else if op.kind() == OperandKind::Use {
                    // it is used, so it MUST be present in the map.
                    let entry = varmap
                        .get_mut(&op.vreg())
                        .expect("encountered vreg never seen before");
                    entry.duration = *entry.duration.start()..=insn;
                    entry.reads.push(insn);
                }
            }
        }

        // Now we have a map of variables and their lifetimes.
        // Next, we do another pass and track the variables against the machine registers
        // and keep track which Vreg is in what register, pushing them out or in depending on the
        // hotness of the variable.
        let mut machine = Machine::new(env);

        let mut allocs: Vec<Allocation> = vec![];
        let mut inst_alloc_offsets: Vec<u32> = vec![];
        println!("varmap: {varmap:#?}");

        for insn in fun.block_insns(entry_b).iter() {
            let ops = fun.inst_operands(insn);
            // Check if the current vregs are in registers.

            println!("ops: {ops:?}");
            for op in ops {
                if op.kind() == OperandKind::Use {
                    if machine.in_register(op.vreg()).is_none() {
                        todo!("move value into a register")
                    }
                }
                if op.kind() == OperandKind::Def {
                    todo!("pick a register")
                }
            }

            machine.evict_after_inst(insn, &varmap);
        }

        Ok(regalloc2::Output {
            allocs,
            inst_alloc_offsets,
            ..Default::default()
        })
    }
}

// Similar function signature as regalloc2 itself.
pub fn run<F: RegFunction>(fun: &F, env: &MachineEnv) -> Result<RegOutput, RegAllocError> {
    // let _ = (fun, env);
    winged::run(fun, env)
}

pub fn default_env() -> MachineEnv {
    regalloc2::MachineEnv {
        preferred_regs_by_class: [vec![], vec![], vec![]],
        non_preferred_regs_by_class: [vec![], vec![], vec![]],
        scratch_by_class: [None, None, None],
        fixed_stack_slots: vec![],
    }
}

pub fn run_ir(func: &ir::Function, env: &MachineEnv) -> Result<RegOutput, RegAllocError> {
    println!("func: {func:#?}");
    let regfun = wrapper::IrFunction::new(func);
    println!("regfun: {regfun:#?}");
    run(&regfun, env)
}
