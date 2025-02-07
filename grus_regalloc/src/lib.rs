use cranelift_codegen::ir;
use regalloc2::Function as RegFunction;
use regalloc2::Output as RegOutput;
use regalloc2::{MachineEnv, RegAllocError};

pub mod wrapper;

// Similar function signature as regalloc2 itself.
pub fn run<F: RegFunction>(func: &F, env: &MachineEnv) -> Result<RegOutput, RegAllocError> {
    let _ = (func, env);
    todo!();
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
    let regfun = wrapper::IrFunction::new(func);
    println!("regfun: {regfun:#?}");
    run(&regfun, env)
}
