# Grus Codegen

A prototype x86 compiler backend / code generator, using [cranelift][cranelift] as a frontend.
Mostly to get a better understanding of the challenges solved by a compiler.

Naming is hard, [Grus grus](https://en.wikipedia.org/wiki/Common_crane) is the common crane.

## Notes

Main input are the `.clif` files used in cranelift's testing. These are found in [./filetests/](/filetests/).

Example invocation:
```
RUST_LOG=trace cargo r  --example tool --   --register-machine Int4 --write-svg /tmp/foo.svg test ./filetests/0007_for_loop.clif
```



Rough structure is:
- `grus_codegen`; this is the actual backend, it contains the `Isa` that can compile a function, the `lir` module that
  holds the lower intermediate representation and the `codegen` module to create x86 instructions. Convenience helpers
  are `trap` for printing all registers on `int3` and `clif_support` that holds helpers to load a clif file and run its tests.
- `grus_module`; this creates a object file in memory, it is a very trimmed down version of Cranelift's module crate.
  Relies on the `object` crate
- `grus_regalloc`; A very naive single-block register allocator, this worked fine for trivial single block functions but
  will need some work to handle multiple blocks. This crate does hold tooling to render a register assignment output in
  regalloc2's format to an svg. The allocator itself needs some work to handle multipule blocks, so the default for the
  tool in `grus_codegen` is regalloc2's Ion allocator.


### grus_codegen

This is where most things live;
- `clif_support`: tooling for testing `.clif` files.
- `codegen`: Standalone module that allows describing x86 instructions and emitting their bytecode.
- `isa`: Entrypoint for compilation of an `cranelift::ir::function::Function` (`IrFunction`).
- `lir`: Lower level intermediate presentation, this takes an `IrFunction` and lowers it to x86 in several steps.
- `trap`: Helper to print all registers and resume execution when an `Int3` is encountered.

The `clif_support`  is the entry point to running a `clif` file through the backend. It calls into the isa to compile a
function, puts it into a module that is placed in memory, and finally it builds a trampoline to call into the compiled
function in executable memory.

The `codegen` module (imported as `cg`) provides the instructions (and ultimately emits bytecode) for the implemented
x86 instructions, the `lir` module converts the instructions in the `IrFunction` into `cg::Instruction`, there's a minor
amount of validation that merely checks the operand count for each instruction. These instructions are the instructions
that exist in the Intel [Software Developer Manual](https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html),
if a instruction in the `IrFunction` needs mltiple machine instructions to be represented, the `lir` should take care
of that. The bulk of this module is a big switch over the available instructions to actually build the bytecode.

The `isa` merely provides a nice interface to compile an `IrFunction`, it converts that function to a `LirFunction`, then
it calls the correct lowering steps in the correct order and provides the register allocation. Finally, it returns the
assembled result.

The `lir` does the majority of the work, usually through the following steps.

#### Function & Lirify
The `Function` is created from a `IrFunction`, this just copies the function for local storage.
The `lirify` method is invoked, this iterates through the blocks of the `IrFunction` and creates a `Section` for each
(cranelift IR) instruction inside each block. These `Sections` initially hold just a single `Ir` instruction but they
can at the same told hold any number of `lir` instructions. A `lir` instruction can have operands, which can be either
`Virtual`, which refers to the `IrFunction` virtual registers, it can be a `Machine` type, which is a `cg::Operand`
(a register or immediate value) or it can be a `ProgramPoint`, which refers to a position in the final x86 bytecode.

### First lower
The `lower_first` method is called, this iterates through all the sections and their `IrInstruction`'s and populates
the `lir_inst` field in the section as best as possible. For some instructions this will become a single `LirInstruction`
others may lower into multiple. The function prologue is provided with a `Nop` that defines each argument there.
The majority of operands at the end of this step are still `Virtual`.

Any branches get the compare added, but the blocks and their destinations (and arguments) are stored in an optional
'special' field for the section. At the end of this method, the `split_blocks` method is invoked. This iterates over
all the sections again and breaks branching if statements into two separate blocks to eliminate branches with block
arguments, so it changes:
```
brif v9, block2(v8, v7), block3
```
into:
```
brif v9, block2Intermediate, block3

block2Intermediate:
  v9 = v8
  v10 = v7
  jump block2(v9, v10)
```

This ensures that moves exist in the right place in order to ensure the values can be placed in the right registers
before a non-conditional jump to the destination block.

#### Register assignment
Next is register allocation, the `LirFunction` provides a `reg_wrapper` method that implements the interface that
`regalloc2` (and `grus_regalloc`) require, this produces a `regalloc2::Output` struct, which can be applied with
`apply_regalloc`. This replaces the remaining `Virtual` operands with `Machine` operands, or in case of something being
a stack value this becomes a immediate offset relative to the `EBP` register. Currently there's no support for situations
where the machine instruction can't operate on a stack address. (Probably need this at some point, or just use a scratch
register and move it back and forth).

#### Second lower
When the second lower pass happens, it is known how many stack slots the function requires, so this is the point where
we insert the function prologue to the section that is marked as prologue. This is also where we place moves to ensure
that the arguments coming in through the calling convention are placed in the correct registers that the register
allocator assigned them. There's a bit of shuffling with jumps as well, to split the original single instruction that
was a single conditional jump with two destination (at the end of the block) into a conditional jump and a non
conditional one. At the function return it places the epilogue and moves the returned value into the return register
according to the calling convention.

#### Patch operations
This method is a bit of a hack, in the SSA addition is like; `v3 = v2 + v1`, but in x86 instructions an addition is
always of the shape `x = x + y`, so the new value is written to the first operand. To handle this we insert moves to
ensure that we move the first operand into the destination register first. We do this after the register allocation such
that we don't have constraints on our register allocator to handle this.

#### Assemble!
This is the last step of compiling the function. The `LirFunction` now holds `cg::Instructions` for all sections and all
operands are `Machine`, except the `ProgramPoint` operands, which are the destinations of jumps. These offsets are only
known after bytecode emission has happened because the instructions are all varying in length. So in this method there's
two passes to emit the bytecode of the instructions, the first to calculated the positions of the `ProgramPoint`'s and
the second pass to actually use the calculated offsets and produce the correct bytecode. Inefficient, but simpler than
updating the offsets afterwards in-place.



# License
License is [`Apache-2.0 WITH LLVM-exception`](./LICENSE), identical to cranelift.

[cranelift]: https://cranelift.dev/
[cranelift_codegen]: https://docs.rs/cranelift-codegen/latest/cranelift_codegen/
[regalloc2]: https://docs.rs/regalloc2/latest/regalloc2/
