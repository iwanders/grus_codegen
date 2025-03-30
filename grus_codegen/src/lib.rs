pub mod isa;
pub use isa::X86Isa;

pub mod clif_support;
pub mod codegen;
pub mod lir;

use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub enum RegisterMachine {
    Int1,
    Int4,
    Int8,
}
impl RegisterMachine {
    pub fn to_env(&self) -> regalloc2::MachineEnv {
        match *self {
            RegisterMachine::Int1 => grus_regalloc::simple_int_machine(1, 0),
            RegisterMachine::Int4 => grus_regalloc::simple_int_machine(4, 0),
            RegisterMachine::Int8 => grus_regalloc::simple_int_machine(8, 0),
        }
    }
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize, Default)]
pub enum RegisterAllocator {
    #[default]
    Winged,
    Regalloc2Ion,
    Regalloc2Fastalloc,
}
impl RegisterAllocator {
    pub fn to_regalloc2_algorithm(&self) -> Option<regalloc2::Algorithm> {
        match *self {
            RegisterAllocator::Winged => None,
            RegisterAllocator::Regalloc2Ion => Some(regalloc2::Algorithm::Ion),
            RegisterAllocator::Regalloc2Fastalloc => Some(regalloc2::Algorithm::Fastalloc),
        }
    }
}
