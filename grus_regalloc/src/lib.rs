use regalloc2::Function as RegFunction;
use regalloc2::Output as RegOutput;
// use regalloc2::Inst as RegInst;
use regalloc2::{MachineEnv, RegAllocError};
// use regalloc2::{InstRange, MachineEnv, PRegSet, RegAllocError, RegClass, VReg};

pub mod wrapper;

// Similar function signature as regalloc2 itself.
pub fn run<F: RegFunction>(func: &F, env: &MachineEnv) -> Result<RegOutput, RegAllocError> {
    let _ = (func, env);
    todo!();
}
