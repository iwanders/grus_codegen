pub mod isa;
pub use isa::X86Isa;

pub mod clif_support;
pub mod codegen;
pub mod lir;
pub mod trap;

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

    pub fn to_cg_reg(&self, index: usize) -> crate::codegen::Reg {
        let reg_order = [
            crate::codegen::Reg::EDI, // PReg(0),
            crate::codegen::Reg::ESI, // PReg(1),
            crate::codegen::Reg::EDX, // PReg(2),
            crate::codegen::Reg::ECX, // PReg(3),
                                      // Reg::R8,  // PReg(4),
                                      // Reg::R9,  // PReg(5),
                                      // Reg::R10, Reg::R11, Reg::R12, Reg::R13, Reg::R14, Reg::R15
        ];
        *match *self {
            RegisterMachine::Int1 => reg_order
                .get(index)
                .expect(&format!("Could not retrieve register {index}")),
            RegisterMachine::Int4 => reg_order
                .get(index)
                .expect(&format!("Could not retrieve register {index}")),
            RegisterMachine::Int8 => reg_order
                .get(index)
                .expect(&format!("Could not retrieve register {index}")),
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
