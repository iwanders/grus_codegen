use cranelift_codegen::ir;
use regalloc2::Function as RegFunction;
use regalloc2::Output as RegOutput;
use regalloc2::{MachineEnv, RegAllocError};

pub mod wrapper;

/*
Okay... so register allocation. 😰
Lets just aim to get something naive working first.
And lets start with local (in-block) allocation only.
*/

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
    use regalloc2::Inst as RegInst;
    use regalloc2::{InstRange, OperandKind, VReg};

    pub fn run<F: RegFunction>(fun: &F, env: &MachineEnv) -> Result<RegOutput, RegAllocError> {
        #[derive(Debug)]
        struct VariableState {
            /// Range this variable is alive, first duration is write / creation!
            duration: std::ops::RangeInclusive<RegInst>,
            /// Instructions for which this variable is used.
            reads: Vec<RegInst>,
        }

        let mut varmap: std::collections::HashMap<VReg, VariableState> = Default::default();
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

        println!("varmap: {varmap:#?}");

        Ok(regalloc2::Output {
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
