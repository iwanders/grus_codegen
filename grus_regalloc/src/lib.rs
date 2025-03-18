#![allow(clippy::needless_range_loop)]
use cranelift_codegen::ir;
use regalloc2::Function as RegFunction;
use regalloc2::Output as RegOutput;
use regalloc2::{
    Allocation, Inst, MachineEnv, Operand, OperandConstraint, PReg, RegAllocError, RegClass, VReg,
};

pub mod svg;
pub mod wrapper;

/*
Todo: How do we specify that arguments exist in the machine at the start of the function?
    -> Probably... create an instruction that does a Def on regalloc2::OperandConstraint::FixedReg?
*/

/*
Okay... so register allocation. 😰
Lets just aim to get something naive working first.
And lets start with local (in-block) allocation only.
*/

use std::collections::HashMap;

#[derive(Default)]
struct AllocationTracker {
    allocs: Vec<Allocation>,
    /// Allocations used for one definition is allocs[inst_alloc_offsets[inst.index()]..inst_alloc_offsets[(inset.index()+1)]]
    inst_alloc_offsets: Vec<u32>,

    previous_inst: Option<Inst>,
}

impl AllocationTracker {
    pub fn new() -> Self {
        Self {
            allocs: vec![],
            inst_alloc_offsets: vec![0],
            previous_inst: None,
        }
    }
    pub fn add_allocation(&mut self, inst: Inst, alloc: Allocation) {
        println!("adding allocation for {inst:?} and {alloc:?}");
        if let Some(v) = self.previous_inst.as_mut() {
            if *v != inst {
                self.inst_alloc_offsets.push(self.allocs.len() as u32);
            }
        }
        self.allocs.push(alloc);
        self.previous_inst = Some(inst);
    }

    pub fn inst_alloc_offsets(&self) -> Vec<u32> {
        self.inst_alloc_offsets.clone()
    }
    pub fn allocs(&self) -> Vec<Allocation> {
        self.allocs.clone()
    }
}

#[derive(Debug, Clone)]
pub struct Machine {
    pub preferred_reg_orders: [Vec<PReg>; 3],
    pub preferred_regs_by_class: [HashMap<PReg, Option<VReg>>; 3],
    pub non_preferred_regs_by_class: [HashMap<PReg, Option<VReg>>; 3],
    pub non_preferred_reg_orders: [Vec<PReg>; 3],
}

impl Machine {
    pub fn new(env: &regalloc2::MachineEnv) -> Self {
        let mut preferred_regs_by_class: [HashMap<PReg, Option<VReg>>; 3] = Default::default();
        let mut preferred_reg_orders: [Vec<PReg>; 3] = Default::default();

        let mut non_preferred_regs_by_class: [HashMap<PReg, Option<VReg>>; 3] = Default::default();
        let mut non_preferred_reg_orders: [Vec<PReg>; 3] = Default::default();

        for i in 0..3 {
            for preg in env.preferred_regs_by_class[i].iter() {
                preferred_reg_orders[i].push(*preg);
                preferred_regs_by_class[i].insert(*preg, None);
            }
            for preg in env.non_preferred_regs_by_class[i].iter() {
                non_preferred_reg_orders[i].push(*preg);
                non_preferred_regs_by_class[i].insert(*preg, None);
            }
        }
        Self {
            preferred_reg_orders,
            preferred_regs_by_class,
            non_preferred_regs_by_class,
            non_preferred_reg_orders,
        }
    }

    pub fn get(&self, preg: PReg) -> Option<VReg> {
        let reg_groups = [
            &self.preferred_regs_by_class,
            &self.non_preferred_regs_by_class,
        ];
        for group in reg_groups {
            for i in 0..3 {
                if let Some(res) = group[i].get(&preg) {
                    return *res;
                }
            }
        }
        None
    }

    pub fn is_empty(&self, preg: PReg) -> bool {
        self.get(preg).is_none()
    }

    pub fn in_register(&self, vreg: VReg) -> Option<PReg> {
        let reg_groups = [
            &self.preferred_regs_by_class,
            &self.non_preferred_regs_by_class,
        ];
        for i in 0..3 {
            for group in reg_groups {
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

    pub fn assign(&mut self, preg: PReg, vreg: VReg) {
        // Should we return a move instruction here in case preg is not empty?

        let reg_groups = [
            &mut self.preferred_regs_by_class,
            &mut self.non_preferred_regs_by_class,
        ];
        for group in reg_groups {
            for i in 0..3 {
                if let Some(vreg_slot) = group[i].get_mut(&preg) {
                    if vreg_slot.is_some() {
                        todo!("vreg slot is full for {preg:?} and {vreg:?} but still assigned {vreg_slot:?}");
                    }
                    *vreg_slot = Some(vreg);
                    return;
                }
            }
        }
        unreachable!()
    }

    pub fn def_register(&mut self, op: &Operand) -> Result<Allocation, RegAllocError> {
        if op.kind() == regalloc2::OperandKind::Use {
            panic!("got use register in def register");
        }
        let reg_groups = [
            (
                &mut self.preferred_regs_by_class,
                &self.preferred_reg_orders,
            ),
            (
                &mut self.non_preferred_regs_by_class,
                &self.non_preferred_reg_orders,
            ),
        ];
        let class_index = op.class() as usize;
        for (group, order) in reg_groups {
            // for try_preg in self.
            for preg in order[class_index].iter() {
                if group[class_index][preg].is_none() {
                    *group[class_index].get_mut(preg).unwrap() = Some(op.vreg());
                    return Ok(Allocation::reg(*preg));
                }
            }
        }

        todo!("could not find a register, need to do something else")
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

        For if statements, we get diverging codepaths, that may - or may not - converge.
        What if we always put the input variables to a block on the stack, does that resolve
        the constraints between departure location and and block landing location registers?
        Each block could have its own stack slot?

        The situation
            brif v0, block5(v1), block5(v2)
        is still problematic then...

        https://godbolt.org/z/TEo1acP87

        #[no_mangle]
        extern "C" fn foo(v0: u64, v1: u64) -> u64 {
            let r = if v0 == 0{
                &v0
            } else {
                &v1
            };
            (*r).wrapping_add(3)
        }

        foo:
         mov QWORD PTR [rsp-0x28],rdi
         mov QWORD PTR [rsp-0x20],rsi
         cmp QWORD PTR [rsp-0x28],0x0
         jne 1e <foo+0x1e> ----------------+      Diverge point.
         lea rax,[rsp-0x28]                |
         mov QWORD PTR [rsp-0x18],rax      |      <-
         jmp 28 <foo+0x28>   -------------/.\--+    \ Both move rax into rsp-0x18
         lea rax,[rsp-0x20]  <-------------+   |    /
         mov QWORD PTR [rsp-0x18],rax          |  <-
         mov rax,QWORD PTR [rsp-0x18] <--------+  Converge point.
         mov rax,QWORD PTR [rax]
         mov QWORD PTR [rsp-0x10],rax
         mov QWORD PTR [rsp-0x8],0x3
         add rax,0x3
         mov QWORD PTR [rsp-0x30],rax
         mov rax,QWORD PTR [rsp-0x30]
         ret

        with
            (module
              (func (export "diverging_converging") (param i32 i32) (result i32)
                (if (result i32) (local.get 0)
                  (then
                    local.get 1
                  )
                  (else
                    local.get 0
                  )
                )
                i32.const 1
                i32.add
              )
            )

        the ir becomes:

        function u0:0(i64 vmctx, i64, i32, i32) -> i32 tail {
            gv0 = vmctx
            gv1 = load.i64 notrap aligned readonly gv0+8
            gv2 = load.i64 notrap aligned gv1+16
            stack_limit = gv2
            block0(v0: i64, v1: i64, v2: i32, v3: i32):
                brif v2, block2, block4
            block2:
                jump block3(v3)
            block4:
                jump block3(v2)
            block3(v5: i32):
                jump block1
            block1:
                v6 = iconst.i32 1
                v7 = iadd.i32 v5, v6  ; v6 = 1
                return v7
        }

        So two intermediate blocks are created for the jump to the last block.

    */
    use super::*;

    use regalloc2::{Inst, OperandKind, VReg};

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
                        let state = &varmap[vreg];
                        if state.duration.end().0 <= current_instruction.0 {
                            println!("evicted {vreg:?}");
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
        // let mut first_instruction = None;
        println!("block ins: {:?}", fun.block_insns(entry_b));
        let insn = fun.block_insns(entry_b).first();
        let first_instruction = Some(insn);
        for entry_vreg in fun.block_params(entry_b) {
            varmap.insert(
                *entry_vreg,
                VariableState {
                    duration: insn..=insn,
                    reads: vec![],
                },
            );
            // Handle the first instruction's early defs.
            let ops = fun.inst_operands(insn);
            for op in ops {
                if op.pos() != regalloc2::OperandPos::Early {
                    continue;
                }
                println!("entry instr Op: {op:?}, pos: {:?}", op.pos());
                if op.kind() == OperandKind::Def {
                    varmap.insert(
                        op.vreg(),
                        VariableState {
                            duration: insn..=insn,
                            reads: vec![],
                        },
                    );
                }
            }
        }

        // Clunky; iterate over the first blocks for now.
        let start_entry = vec![entry_b];
        let block_ids: Vec<_> = start_entry
            .iter()
            .chain(fun.block_succs(entry_b).iter())
            .collect();
        for block in block_ids.iter() {
            for insn in fun.block_insns(**block).iter() {
                println!("Instzzzz: {insn:?}");
                let ops = fun.inst_operands(insn);
                let is_early_first_instruction = Some(insn) == first_instruction;
                for stage in [regalloc2::OperandPos::Early, regalloc2::OperandPos::Late] {
                    for op in ops {
                        if op.pos() != stage {
                            continue;
                        }
                        println!("Op: {op:?}, pos: {:?}", op.pos());
                        if is_early_first_instruction
                            && op.kind() == OperandKind::Def
                            && op.pos() == regalloc2::OperandPos::Early
                        {
                            continue;
                        }

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
                            let entry = varmap.get_mut(&op.vreg()).unwrap_or_else(|| {
                                panic!("encountered vreg {op:?} never seen before at {insn:?}")
                            });
                            entry.duration = *entry.duration.start()..=insn;
                            entry.reads.push(insn);
                        }
                    }
                }
            }
        }

        // Now we have a map of variables and their lifetimes.
        // Next, we do another pass and track the variables against the machine registers
        // and keep track which Vreg is in what register, pushing them out or in depending on the
        // hotness of the variable.
        let mut machine = Machine::new(env);

        let mut tracker = AllocationTracker::new();

        println!("varmap: {varmap:#?}");

        // Populate the machine with the early def's from the first instruction.
        let ops = fun.inst_operands(
            *first_instruction
                .as_ref()
                .expect("must have first instruction"),
        );
        for op in ops {
            println!("op; {op:#?}");
            // println!("Machine; {machine:#?}");
            if op.kind() == OperandKind::Def && op.pos() == regalloc2::OperandPos::Early {
                match op.constraint() {
                    OperandConstraint::FixedReg(preg) => {
                        if machine.is_empty(preg) {
                            machine.assign(preg, op.vreg());
                        } else {
                            todo!("got a def on {preg:?} but that is occupied")
                        }
                    }
                    _ => {
                        todo!()
                    }
                }
            }
        }
        println!("Machine; {machine:#?}");
        for insn in fun.block_insns(entry_b).iter() {
            println!("zzzxxx insn: {insn:?}");
        }

        for block in block_ids.iter() {
            for insn in fun.block_insns(**block).iter() {
                let ops = fun.inst_operands(insn);
                let is_first_instruction = Some(insn) == first_instruction;
                // let is_first_instruction = false;

                println!("ops: {ops:?} insn: {insn:?}");
                for op in ops {
                    if op.kind() == OperandKind::Def {
                        if is_first_instruction && op.pos() == regalloc2::OperandPos::Early {
                            continue;
                        }
                        match op.constraint() {
                            OperandConstraint::FixedReg(preg) => {
                                if machine.is_empty(preg) {
                                    machine.assign(preg, op.vreg());
                                    tracker.add_allocation(insn, Allocation::reg(preg));
                                } else {
                                    todo!("got a def on {preg:?} but that is occupied")
                                }
                            }
                            OperandConstraint::Any => {
                                // Pick any free register.
                                let alloc = machine.def_register(op)?;
                                tracker.add_allocation(insn, alloc);
                            }
                            _ => todo!("operand constraint: {:?}", op.constraint()),
                        }
                    } else if op.kind() == OperandKind::Use {
                        if machine.in_register(op.vreg()).is_none() {
                            todo!("need to move {op:?} value into a register")
                        }
                        let alloc = machine.in_register(op.vreg()).unwrap();
                        tracker.add_allocation(insn, Allocation::reg(alloc));
                    }
                }

                // End of instruction, evict anything that is no longer necessary.
                machine.evict_after_inst(insn, &varmap);
            }
        }
        Ok(regalloc2::Output {
            allocs: tracker.allocs(),
            inst_alloc_offsets: tracker.inst_alloc_offsets(),
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

pub fn run_ir(func: &ir::Function, env: &MachineEnv) -> Result<RegOutput, RegAllocError> {
    println!("func: {func:#?}");
    let regfun = wrapper::IrFunction::new(func);
    println!("regfun: {regfun:#?}");
    run(&regfun, env)
    // let options = regalloc2::RegallocOptions::default();
    // regalloc2::run(&regfun, env, &options)
}
