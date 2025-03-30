use cranelift_codegen::ir::Function;

use log::*;
use target_lexicon::Triple;

#[derive(Debug)]
pub struct X86Isa {
    triple: Triple,
}

impl X86Isa {
    pub fn new() -> Self {
        Self {
            triple: target_lexicon::triple!("x86_64"),
        }
    }
}
impl Default for X86Isa {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct CompiledCode {
    pub buffer: Vec<u8>,
    // pub frame_size: usize,
}

use crate::{RegisterAllocator, RegisterMachine};

pub struct CompileSettings {
    pub register_allocator: RegisterAllocator,
    pub register_machine: RegisterMachine,
    pub write_svg: Option<std::path::PathBuf>,
}

impl X86Isa {
    pub fn name(&self) -> &'static str {
        "X86Isa"
    }
    pub fn triple(&self) -> &Triple {
        &self.triple
    }

    pub fn compile_function(
        &self,
        func: &Function,
        settings: &CompileSettings,
    ) -> Result<CompiledCode, anyhow::Error> {
        let _ = func;

        // Okay, so now we get a stencil, that has a dfg, and we need to output instructions for that.
        let stencil = &func.stencil;
        // https://docs.rs/cranelift-codegen/0.116.1/cranelift_codegen/ir/function/struct.FunctionStencil.html

        // If we encounter any of these, we don't support that yet.
        if !stencil.sized_stack_slots.is_empty() {
            todo!("dynamic_stack_slots")
        }
        if !stencil.dynamic_stack_slots.is_empty() {
            todo!("dynamic_stack_slots")
        }
        if !stencil.global_values.is_empty() {
            todo!("global_values")
        }
        if !stencil.global_value_facts.is_empty() {
            todo!("global_value_facts")
        }

        /*
        Okay, now we need to do something with the dfg and layout

        https://docs.rs/cranelift-codegen/0.116.1/cranelift_codegen/ir/layout/struct.Layout.html
        > The Layout struct determines the layout of blocks and instructions in a function

        https://docs.rs/cranelift-codegen/0.116.1/cranelift_codegen/ir/dfg/struct.DataFlowGraph.html

        A data flow graph defines all instructions and basic blocks in a function as well as the
        data flow dependencies between them. The DFG also tracks values which can be either
        instruction results or block parameters.

        The layout of blocks in the function and of instructions in each block is recorded by the
        Layout data structure which forms the other half of the function representation.

        https://docs.rs/cranelift-codegen/0.116.1/cranelift_codegen/ir/instructions/enum.InstructionData.html
        */
        let env = settings.register_machine.to_env();

        let mut lirfun = crate::lir::Function::from_ir(&func);
        lirfun.lirify();
        lirfun.lower_first();
        let reg_wrapper = lirfun.reg_wrapper();
        println!("{lirfun:#?}");
        lirfun.dump();

        let reg_outputs = match settings.register_allocator {
            RegisterAllocator::Winged => grus_regalloc::run(&reg_wrapper, &env)?,
            RegisterAllocator::Regalloc2Ion | RegisterAllocator::Regalloc2Fastalloc => {
                let options = regalloc2::RegallocOptions {
                    verbose_log: false,
                    validate_ssa: true,
                    algorithm: settings
                        .register_allocator
                        .to_regalloc2_algorithm()
                        .unwrap(),
                };
                regalloc2::run(&reg_wrapper, &env, &options)?
            }
        };
        println!("reg_outputs: {reg_outputs:#?}");
        if let Some(svg_output_path) = &settings.write_svg {
            let options = Default::default();
            let document = grus_regalloc::svg::register_document(
                &reg_wrapper,
                &reg_outputs,
                &env,
                &options,
                &reg_wrapper,
            );
            grus_regalloc::svg::svg::save(svg_output_path, &document)?;
        }

        // let reg_outputs =
        // regalloc2::run(&reg_wrapper, &grus_regalloc::simple_int_machine(4, 0), &Default::default())?;

        lirfun.apply_regalloc(&reg_wrapper, &reg_outputs, &settings.register_machine);
        lirfun.lower_second();
        // lirfun.patch_returns();
        lirfun.patch_operations();

        let buffer = lirfun.assemble();

        let mut s = String::new();
        for b in buffer.iter() {
            s += &format!(" 0x{b:0>2x}");
        }
        warn!("buffer: echo {s}");
        Ok(CompiledCode {
            buffer,
            // Size of stack frame, in bytes.
            // frame_size,
        })
    }
}
