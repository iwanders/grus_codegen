#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(irrefutable_let_patterns)]
use cranelift_codegen::ir::Function as CraneliftIrFunction;
use cranelift_codegen::ir::Inst as IrInst;
use cranelift_codegen::ir::InstructionData as IrInstructionData;
use cranelift_codegen::ir::{self, Value};
use cranelift_codegen::write::FuncWriter;

use crate::codegen as cg;

/**
    A low(er) level intermediate representation.
*/
/*

    It would be nice if we can go from cranelift IR to this lower level representation...
    Identifying things like 'Magic stuff [EAX] and [--][--], page 36', which uses a register as
    base address and another as offset.

    Want to be able to put the calling convention here.

    Handle offsets and jumps for branches and loops.

    Ideall, we can handle;
        Some instructions still IrInst, with values.
            - Only know registers after we've performed the register allocation.
        Some instructions have (partial) limitations on registers
            - Some instructions have hardware constraints.
            - Calling convention.
            - If a single Ir Instruction becomes a bunch of instructions.

    Should we instead use cranelift IR opcodes to express?
    Basically going from cranelift IR to cranelift IR?

    That doesn't work though for sections that MUST have specific registers... like with the
    calling convention, since the cranelift IR is all on Values. It would be nice to be able to
    reuse that though.


    What if we do:
        LirSection:
            - Vec<HwInstruction>
            - Vec<IrInstruction>

        That way we can gradually convert ir instructions into hw instructions?
        And we can do something whereh hw instructions aren't fully specified yet, potentially
        with partial register specifications and populate the remainder?

        A block can then be made up of multiple sections.
        The trivial translation is lirsections where the ir instructions is one long.
        This is necessary for example where an ir instruction lowers into multiple hardware
        instructions.

        And we can track which ir instructions resulted in which hw instructions, while still being
        able to create sections that contain just hw instructions, like with the calling convention?

    Stages:
        - Function::new(), just copies the cranelift function.
        - lirify; creates lir blocks, copies each Cranelift Instruction into its own section.
        - lower_first; Creates hw instructions for most section, without register allocations.
        - register allocation:
            - For already lowered instructions, virtual operands are substituted with registers.
            - For non lowered ir, registers are stored for second lower pass.
        - lower second; now that registers are available for all instructions remaining ir is lowered.
        - Can create bytecode for sections now.
        - Something with jumps and offsets?


    Todo:
      Handle function preamble / calling convention.
      Handle block params
      Figure out how other register allocator specify the operands and handle the block params / function start.


      links:
        https://github.com/bytecodealliance/regalloc2/pull/170  On blockparams in branches.
        Something with critical edges;
          https://github.com/bytecodealliance/regalloc2/blob/925df1b4674435a9322e21912926a68749517861/src/cfg.rs#L79
          https://en.wikipedia.org/wiki/Control-flow_graph#Special_edges

        > A critical edge is an edge which is neither the only edge leaving its source block, nor the only edge entering
        > its destination block. These edges must be split: a new block must be created in the middle of the edge, in
        > order to insert computations on the edge without affecting any other edges.
      Okay, that makes sense, lets just always split all branches into two blocks without arguments, and then put
      the moves in those blocks... then jump to the next block without arguments, that way we don't ahve to worry about
      any of this.

*/

/*
Need to give blocks clear 'these are the input registers' to the block, currently we don't have that.
*/
use cranelift_codegen::isa::CallConv;
use log::*;
use std::collections::{HashMap, HashSet};

impl From<ir::condcodes::IntCC> for cg::Condition {
    fn from(value: ir::condcodes::IntCC) -> Self {
        match value {
            ir::condcodes::IntCC::Equal => todo!(),
            ir::condcodes::IntCC::NotEqual => todo!(),
            ir::condcodes::IntCC::SignedLessThan => todo!(),
            ir::condcodes::IntCC::SignedGreaterThanOrEqual => todo!(),
            ir::condcodes::IntCC::SignedGreaterThan => todo!(),
            ir::condcodes::IntCC::SignedLessThanOrEqual => todo!(),
            ir::condcodes::IntCC::UnsignedLessThan => cg::Condition::IfLess,
            ir::condcodes::IntCC::UnsignedGreaterThanOrEqual => todo!(),
            ir::condcodes::IntCC::UnsignedGreaterThan => cg::Condition::IfGreater,
            ir::condcodes::IntCC::UnsignedLessThanOrEqual => todo!(),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
struct BlockId(usize);

#[derive(Copy, Clone, Debug)]
pub struct Inst(usize);
use Inst as LirInst;

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub struct ProgramPoint(usize);

#[derive(Copy, Clone, Debug)]
pub enum LirOperand {
    Virtual(Value),
    Machine(cg::Operand),
    ProgramPoint(ProgramPoint),
}
impl LirOperand {
    fn simple_string(&self) -> String {
        match self {
            LirOperand::Virtual(value) => format!("{value:?}"),
            LirOperand::Machine(operand) => format!("{operand:?}"),
            LirOperand::ProgramPoint(point) => format!("{point:?}"),
        }
    }
}
impl From<Value> for LirOperand {
    fn from(v: Value) -> LirOperand {
        LirOperand::Virtual(v)
    }
}
impl From<cg::Operand> for LirOperand {
    fn from(v: cg::Operand) -> LirOperand {
        LirOperand::Machine(v)
    }
}

impl From<ProgramPoint> for LirOperand {
    fn from(v: ProgramPoint) -> LirOperand {
        LirOperand::ProgramPoint(v)
    }
}
#[derive(Clone, Debug)]
pub struct InstructionData {
    pub operation: cg::Op,
    // Should we have def_operand and use_operand?
    // The register allocator can ONLY have a single def operand!
    pub def_operands: Vec<LirOperand>,
    pub use_operands: Vec<LirOperand>,
}
impl InstructionData {
    pub fn new(operation: cg::Op) -> Self {
        Self {
            operation,
            def_operands: vec![],
            use_operands: vec![],
        }
    }
    pub fn operands_iter_mut<'a>(&'a mut self) -> impl Iterator<Item = &'a mut LirOperand> {
        self.def_operands
            .iter_mut()
            .chain(self.use_operands.iter_mut())
    }

    pub fn with_use<T: Into<LirOperand> + Copy>(self, ops: &[T]) -> Self {
        Self {
            operation: self.operation,
            def_operands: self.def_operands,
            use_operands: ops.into_iter().map(|z| (*z).into()).collect(),
        }
    }
    pub fn with_def<T: Into<LirOperand> + Copy>(self, ops: &[T]) -> Self {
        Self {
            operation: self.operation,
            def_operands: ops.into_iter().map(|z| (*z).into()).collect(),
            use_operands: self.use_operands,
        }
    }
    pub fn has_virtuals(&self) -> bool {
        self.use_operands
            .iter()
            .any(|v| matches!(v, LirOperand::Virtual(_)))
            || self
                .def_operands
                .iter()
                .any(|v| matches!(v, LirOperand::Virtual(_)))
    }
    pub fn assemble(&self) -> Result<Vec<u8>, cg::CodegenError> {
        let mut opv = cg::OperandVec::new();
        for cont in [&self.def_operands, &self.use_operands] {
            for v in cont.iter() {
                match v {
                    LirOperand::Machine(r) => opv.push(*r),
                    LirOperand::Virtual(_) => {
                        todo!()
                    }
                    LirOperand::ProgramPoint(p) => {
                        todo!("handle program points: {p:?}")
                    }
                }
            }
        }
        let inst = cg::Instruction {
            op: self.operation,
            operands: opv,
        };
        inst.serialize()
    }
    pub fn simple_string(&self) -> String {
        let joined = |v: &[LirOperand]| {
            v.iter()
                .map(|z| z.simple_string())
                .collect::<Vec<String>>()
                .join(", ")
        };
        let def_string = joined(&self.def_operands);
        let use_string = joined(&self.use_operands);
        //let use_string = self.use_operands.iter().map(|z| format!("{z:?}")).join(" ");
        if self.def_operands.is_empty() {
            format!("{:?} {}", self.operation, use_string)
        } else {
            format!("{} = {:?} {}", def_string, self.operation, use_string)
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct BlockCall {
    block: BlockId,
    params: Vec<Value>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BrifData {
    condition: Value,
    params: [BlockCall; 2],
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct JumpData {
    call: BlockCall,
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum Special {
    /// Preamble at the beginning of the function
    ///
    /// This sets up the stack, if any.
    /// This creates 'n' Nop instructions that create the input values out of thin air.
    Prologue,
    /// Branch if data
    ///
    /// Holds the values used to call the other branches.
    Brif(BrifData),
    /// Jumps to another block.
    ///
    /// Holds the values used to call the other branches.
    Jump(JumpData),
}

impl Special {
    pub fn block_call_count(&self) -> usize {
        match self {
            Special::Prologue => 0,
            Special::Brif(brif_data) => brif_data.params.len(),
            Special::Jump(_jump_data) => 1,
        }
    }
    pub fn get_block_call(&self, index: usize) -> &BlockCall {
        match self {
            Special::Prologue => unreachable!(),
            Special::Brif(brif_data) => &brif_data.params[index],
            Special::Jump(jump_data) => &jump_data.call,
        }
    }
}

#[derive(Clone, Debug, Default)]
struct Section {
    lir_inst: Vec<LirInst>,
    ir_inst: Vec<IrInst>,
    ir_regs: Vec<Vec<cg::Reg>>,
    special: Option<Special>,
    is_lowered: bool,
}
impl Section {
    fn from_ir(v: IrInst) -> Self {
        Self {
            lir_inst: vec![],
            ir_inst: vec![v],
            ir_regs: vec![],
            special: None,
            is_lowered: false,
        }
    }
    fn preamble() -> Self {
        Self {
            lir_inst: vec![],
            ir_inst: vec![],
            ir_regs: vec![],
            special: Some(Special::Prologue),
            is_lowered: true,
        }
    }
    pub fn is_lowered(&self) -> bool {
        self.is_lowered
    }
    pub fn is_prologue(&self) -> bool {
        self.special == Some(Special::Prologue)
    }
    pub fn is_branch(&self) -> bool {
        matches!(self.special, Some(Special::Brif(_)))
    }
    pub fn is_jump(&self) -> bool {
        matches!(self.special, Some(Special::Jump(_)))
    }
}

#[derive(Clone, Debug)]
struct Block {
    sections: Vec<Section>,
    id: BlockId,

    // block_param:
    block_preds: HashSet<BlockId>,
    block_succs: HashSet<BlockId>,

    block_params: Vec<Value>,
}
impl Block {
    fn new(id: BlockId) -> Self {
        Self {
            sections: vec![],
            block_succs: Default::default(),
            block_preds: Default::default(),
            block_params: vec![],
            id,
        }
    }
    pub fn push_section(&mut self, section: Section) {
        self.sections.push(section)
    }
}

use std::cell::RefCell;

#[derive(Debug, Eq, PartialEq, Hash)]
enum CodePosition {
    BlockStart(BlockId),
}

impl From<BlockId> for CodePosition {
    fn from(v: BlockId) -> CodePosition {
        CodePosition::BlockStart(v)
    }
}

// The anchor is in the rear.
// -----------------P-------------Q----------|end
// We track points by distance from point to end
// From P to Q should be positive.
// P->Q would be (P <-> End) - (Q <-> End)
#[derive(Debug, Default)]
struct CodePointTracker {
    offsets: HashMap<ProgramPoint, usize>,
}
impl CodePointTracker {
    pub fn add_blockstart(&mut self, f: &Function, b: BlockId, offset: usize) {
        let point_map = f.program_points.borrow();
        for (k, v) in point_map.iter() {
            if let CodePosition::BlockStart(needle) = &v {
                if b == *needle {
                    self.offsets.insert(*k, offset);
                }
            }
        }
    }
    pub fn offset(&self, p: ProgramPoint) -> usize {
        *self
            .offsets
            .get(&p)
            .expect(&format!("failed to find program point {p:?}"))
    }
    pub fn relative(&self, p: ProgramPoint, current_pos: usize) -> i64 {
        let o = self.offset(p) as i64;
        current_pos as i64 - o
    }
}

#[derive(Debug)]
pub struct Function {
    blocks: Vec<Block>,
    entry_block: Option<BlockId>,

    instdata: Vec<InstructionData>,

    call_convention: CallConv,
    fun: CraneliftIrFunction,
    fun_args: Vec<Value>,

    block_counter: RefCell<usize>,
    block_map: RefCell<HashMap<ir::Block, BlockId>>,

    program_points: RefCell<HashMap<ProgramPoint, CodePosition>>,

    value_counter: RefCell<u32>,

    stack_slots: usize,
}

impl Function {
    pub fn from_ir(fun: &CraneliftIrFunction) -> Self {
        Self {
            blocks: vec![],
            instdata: vec![],
            fun_args: vec![],
            entry_block: None,
            call_convention: CallConv::SystemV,
            fun: fun.clone(),
            block_counter: Default::default(),
            block_map: Default::default(),
            value_counter: Default::default(),
            program_points: Default::default(),
            stack_slots: Default::default(),
        }
    }

    fn new_value(&self) -> Value {
        let current_counter = *self.value_counter.borrow();
        let v = Value::from_u32(current_counter + 1);
        *self.value_counter.borrow_mut() += 1;
        v
    }

    /// Get a block id, or if none, create a new one.
    fn get_blockid(&self, original: Option<ir::Block>) -> BlockId {
        let mut map = self.block_map.borrow_mut();
        // Lookup if it already exists
        if let Some(original_block_id) = &original {
            if let Some(block_id) = map.get(&original_block_id) {
                return *block_id;
            }
        }
        // it doesn't exist, add it to the map and advance the index.
        {
            let mut counter = self.block_counter.borrow_mut();
            *counter = (*counter).max(map.len());
        }
        let v = BlockId(*self.block_counter.borrow());
        if let Some(original) = original {
            map.insert(original, v);
        }
        *self.block_counter.borrow_mut() += 1;
        v
    }

    /// Create a new program point.
    fn new_position<T: Into<CodePosition>>(&self, v: T) -> ProgramPoint {
        let mut map = self.program_points.borrow_mut();
        let new_point = ProgramPoint(map.len());
        let new_pos: CodePosition = v.into();
        for (k, v) in map.iter() {
            if *v == new_pos {
                return *k;
            }
        }
        map.insert(new_point, new_pos);
        new_point
    }

    pub fn lirify(&mut self) {
        let dfg = &self.fun.stencil.dfg;
        let layout = &self.fun.stencil.layout;

        let cfg = cranelift_codegen::flowgraph::ControlFlowGraph::with_function(&self.fun);

        // Collect all the Vregs
        let all_values: Vec<Value> = dfg.values().collect();
        let mut highest_value_counter = 0;
        for v in all_values {
            highest_value_counter = highest_value_counter.max(v.as_u32());
        }
        *(self.value_counter.borrow_mut()) = highest_value_counter;

        let ir_entry_block = self
            .fun
            .layout
            .entry_block()
            .expect("should have entry block");
        for v in dfg.block_params(ir_entry_block).iter() {
            self.fun_args.push(*v);
        }

        for b in layout.blocks() {
            let lirblockid = self.get_blockid(Some(b));
            let mut lirblock = Block::new(lirblockid);

            debug!("b: {b:?}");
            let block_data = &dfg.blocks[b];
            debug!(
                "block_data params: {:?}",
                block_data.params(&dfg.value_lists)
            );

            lirblock.block_succs = cfg
                .succ_iter(b)
                .map(|v| self.get_blockid(Some(v)))
                .collect();
            lirblock.block_preds = cfg
                .pred_iter(b)
                .map(|v| self.get_blockid(Some(v.block)))
                .collect();

            lirblock.block_params = dfg.block_params(b).iter().copied().collect();

            if ir_entry_block == b {
                // This is the entry block, push a section for the function preamble.
                lirblock.push_section(Section::preamble());
            }

            for inst in layout.block_insts(b) {
                let s = Section::from_ir(inst);
                lirblock.push_section(s);

                debug!("inst: {inst:?}");
                let instdata = dfg.insts[inst];
                debug!("  instruction_data: {instdata:?}");
                let arguments = instdata.arguments(&dfg.value_lists);
                let type_of = |v: &ir::Value| dfg.value_type(*v);
                let types_of =
                    |v: &[ir::Value]| v.iter().map(|z| dfg.value_type(*z)).collect::<Vec<_>>();
                debug!("  args: {:?} types: {:?}", arguments, types_of(arguments));

                // let allocs_args = regs.inst_allocs(RegInst::new(inst.as_u32() as usize));
                // debug!("  allocs: {allocs_args:?}");
                // let use_allocs = &allocs_args[0..arguments.len()];
                // debug!("  use_allocs: {use_allocs:?}");
                // let def_allocs = &allocs_args[arguments.len().min(arguments.len() + 1)..];
                // debug!("  def_allocs: {def_allocs:?}");

                debug!(
                    "  results? {:?} -> {:?}  (types: {:?}) ",
                    dfg.has_results(inst),
                    dfg.inst_results(inst),
                    types_of(dfg.inst_results(inst))
                );
                let typevar_operand = instdata.typevar_operand(&dfg.value_lists);
                debug!(
                    "  typevar_operand: {:?}, type: {:?}",
                    typevar_operand,
                    typevar_operand.as_ref().map(type_of)
                );
                debug!("  opcode: {:?}", instdata.opcode());
            }
            self.blocks.push(lirblock);
        }

        let irentry = layout.entry_block().expect("should have entry block");
        let entryblockid = self.get_blockid(Some(irentry));
        self.entry_block = Some(entryblockid);
    }

    pub fn split_blocks(&mut self) {
        /* If we encounter
          brif v3, block1(v0), block2(v2)
          ->
          brif v3, block1_intermediate, block2_intermediate

          block1_intermediate:
            v8 = v0
            jump block2(v8)

          block2_intermediate:
            v9 = v2
            jump block2(v9)

          How do we express jump with use's? Do they define? or do they use?
          oh... regfun.branch_blockparams probably takes care of that?

          Lets also do jumps?
          jump block3(v1, v3)

          -> Jump block3a
          mov _ _
          mov _ _
          jump block3
        */

        #[derive(Debug)]
        struct BlockUpdate {
            additional_block: Block,
            call: BlockCall,
            dest_old_blockid: BlockId,
        }

        #[derive(Debug)]
        struct BrifUpdate {
            block_idx: usize,
            section_idx: usize,
            containing_blockid: BlockId,
            is_branch: bool,
            calls: Vec<BlockUpdate>,
        }

        let mut new_blocks = vec![];
        for (bi, b) in self.blocks.iter().enumerate() {
            for (si, s) in b.sections.iter().enumerate() {
                if !s.is_branch() && !s.is_jump() {
                    continue;
                }

                if let Some(z) = s.special.as_ref() {
                    let mut update = BrifUpdate {
                        block_idx: bi,
                        section_idx: si,
                        containing_blockid: b.id,
                        is_branch: s.is_branch(),
                        calls: vec![],
                    };
                    for call_idx in 0..z.block_call_count() {
                        let this_call = z.get_block_call(call_idx);

                        update.calls.push(BlockUpdate {
                            additional_block: {
                                let new_true_block_id = self.get_blockid(None);
                                println!("New block id: {new_true_block_id:?}");
                                let new_block = Block::new(new_true_block_id);
                                new_block
                            },
                            call: this_call.clone(),
                            dest_old_blockid: this_call.block,
                        });

                        let block_update = &mut update.calls[call_idx];
                        let new_block = &mut block_update.additional_block;
                        new_block.block_succs.insert(block_update.call.block);
                        new_block.block_preds.insert(b.id);
                        block_update.call.params.clear();

                        let mut new_values = vec![];
                        // Now, add the moves.
                        for p in this_call.params.iter() {
                            let new_value = self.new_value();
                            new_values.push(new_value);

                            let instdata = InstructionData::new(cg::Op::Mov(cg::Width::W64))
                                .with_use(&[p.clone()])
                                .with_def(&[new_value]);

                            let new_id = Inst(self.instdata.len());
                            self.instdata.push(instdata);

                            let new_section = Section {
                                lir_inst: vec![new_id],
                                is_lowered: true,
                                ..Default::default()
                            };
                            new_block.sections.push(new_section);
                        }

                        // Finally, insert the now branchless jump at the end of the section.
                        // Collect the program point to the original section.
                        let dest_point = self.new_position(block_update.call.block);
                        let instdata = InstructionData::new(cg::Op::Jump).with_use(&[dest_point]);
                        let new_id = Inst(self.instdata.len());
                        self.instdata.push(instdata);
                        let mut jump_section = Section {
                            lir_inst: vec![new_id],
                            is_lowered: true,
                            ..Default::default()
                        };
                        jump_section.special = Some(Special::Jump(JumpData {
                            call: BlockCall {
                                block: block_update.call.block,
                                params: new_values,
                            },
                        }));
                        new_block.sections.push(jump_section);

                        block_update.dest_old_blockid = block_update.call.block;
                        block_update.call.block = new_block.id;
                    }

                    new_blocks.push(update);
                }
            }
        }
        println!("New blocks; {new_blocks:?}");
        let new_blocks = new_blocks.drain(..).rev().collect::<Vec<_>>();
        for brif_update in new_blocks {
            let bi = brif_update.block_idx;
            let si = brif_update.section_idx;
            let containing_blockid = brif_update.containing_blockid;

            self.blocks[bi].sections[si].is_lowered = true;

            let dest_uses = brif_update
                .calls
                .iter()
                .map(|z| self.new_position(z.additional_block.id).into())
                .collect::<Vec<_>>();

            if brif_update.is_branch {
                let new_id = Inst(self.instdata.len());
                let l = InstructionData {
                    operation: cg::Op::Jcc(cg::Condition::IfEqual),
                    def_operands: vec![],
                    use_operands: dest_uses,
                };
                self.instdata.push(l);
                self.blocks[bi].sections[si].lir_inst.push(new_id);
            } else {
                // it is a jump. Jump to the new block.
                let new_id = Inst(self.instdata.len());
                let l = InstructionData {
                    operation: cg::Op::Jump,
                    def_operands: vec![],
                    use_operands: dest_uses,
                };
                self.instdata.push(l);
                self.blocks[bi].sections[si].lir_inst.push(new_id);
            }

            for (pi, call_update) in brif_update.calls.iter().enumerate() {
                //let mut orig_section = &mut self.blocks[bi].sections[si];
                if call_update.call.params.is_empty() {
                    //continue;
                }

                let new_dest = call_update.additional_block.id;
                let old_dest = call_update.dest_old_blockid;

                // Now we need to do two actions.
                // Remove the old destination.
                // Add the new destination.
                let original_dest_block =
                    self.blocks.iter_mut().find(|z| z.id == old_dest).unwrap();
                original_dest_block.block_preds.remove(&containing_blockid);
                original_dest_block.block_preds.insert(new_dest);

                let original_source_block = self
                    .blocks
                    .iter_mut()
                    .find(|z| z.id == containing_blockid)
                    .unwrap();
                original_source_block.block_succs.remove(&old_dest);
                original_source_block.block_succs.insert(new_dest);

                // Finally, replace the block call.
                let orig_block = &mut self.blocks[bi];
                if let Some(v) = orig_block.sections[si].special.as_mut() {
                    if let Special::Brif(data) = v {
                        data.params[pi] = brif_update.calls[pi].call.clone();
                    }
                    if let Special::Jump(data) = v {
                        data.call = brif_update.calls[pi].call.clone();
                    }
                }
            }

            // Then, insert the new block just after the previous id block.
            if let Some(position) = self.blocks.iter().position(|z| z.id == containing_blockid) {
                for c in brif_update.calls.iter() {
                    self.blocks.insert(position + 1, c.additional_block.clone());
                }
            }
        }
        //self.blocks.extend(new_blocks);
    }

    /// Lower IR instructions to partial machine instructions
    pub fn lower_first(&mut self) {
        let new_op = InstructionData::new;
        use cg::{Op, Operand, Width};
        let dfg = &self.fun.dfg;
        let type_of = |v: &ir::Value| dfg.value_type(*v);
        let _types_of = |v: &[ir::Value]| v.iter().map(|z| dfg.value_type(*z)).collect::<Vec<_>>();

        let block_map = self.block_map.borrow().clone();

        for b in self.blocks.iter_mut() {
            for s in b.sections.iter_mut() {
                // let lirs = &mut s.lir_inst;
                let mut lirs = vec![];
                if s.is_prologue() {
                    // Create a nop for each argument... that will ultimately def the argument.
                    for arg_v in self.fun_args.iter() {
                        lirs.push(new_op(Op::Nop).with_def(&[(*arg_v)]));
                    }
                }
                for inst in s.ir_inst.iter() {
                    let instdata = self.fun.dfg.insts[*inst];
                    let def_ir = self.fun.dfg.inst_results(*inst);
                    let use_ir = instdata.arguments(&self.fun.dfg.value_lists);
                    let typevar_operand = instdata.typevar_operand(&dfg.value_lists);
                    match instdata {
                        IrInstructionData::UnaryImm { opcode, imm } => match opcode {
                            ir::Opcode::Iconst => {
                                lirs.push(
                                    new_op(Op::Mov(Width::W64))
                                        .with_use(&[Operand::Immediate(imm.bits())])
                                        .with_def(&[def_ir[0]]),
                                );
                            }
                            _ => todo!(
                                "unimplemented opcode: {:?} in {:?}, of {:?}",
                                opcode,
                                inst,
                                self.fun.name
                            ),
                        },
                        IrInstructionData::MultiAry { opcode, args } => match opcode {
                            ir::Opcode::Return => {
                                if args.len(&self.fun.dfg.value_lists) != 1 {
                                    todo!()
                                }
                                // Lowered in second pass.
                            }
                            _ => todo!(
                                "unimplemented opcode: {:?} in {:?}, of {:?}",
                                opcode,
                                inst,
                                self.fun.name
                            ),
                        },

                        IrInstructionData::Binary { opcode, args: _ } => match opcode {
                            ir::Opcode::Iadd => {
                                let width: Width = typevar_operand
                                    .as_ref()
                                    .map(type_of)
                                    .map(|z| z.into())
                                    .unwrap();

                                // x86 add overwrites the first operand, so for now:
                                //    Move operand one into destination.
                                //    Add operand two to destination.
                                // lirs.push(
                                // new_op(Op::Mov(width))
                                // .with_use(&[use_ir[0]])
                                // .with_def(&[def_ir[0]]),
                                // );
                                // That violated ssa!
                                lirs.push(
                                    new_op(Op::IAdd(width))
                                        .with_use(&[use_ir[0], use_ir[1]])
                                        .with_def(&[def_ir[0]]),
                                );
                            }
                            ir::Opcode::Isub => {
                                let width: Width = typevar_operand
                                    .as_ref()
                                    .map(type_of)
                                    .map(|z| z.into())
                                    .unwrap();

                                // x86 add overwrites the first operand, so for now:
                                //    Move operand one into destination.
                                //    Add operand two to destination.
                                lirs.push(
                                    new_op(Op::ISub(width))
                                        .with_use(&[use_ir[0], use_ir[1]])
                                        .with_def(&[def_ir[0]]),
                                );
                            }

                            ir::Opcode::Imul => {
                                let width: Width = typevar_operand
                                    .as_ref()
                                    .map(type_of)
                                    .map(|z| z.into())
                                    .unwrap();

                                // x86 integer mul supports three forms, we use the two-operand
                                // version here, this multiplies into the destination.

                                lirs.push(
                                    new_op(Op::IMul(width))
                                        .with_use(&[use_ir[0], use_ir[1]])
                                        .with_def(&[def_ir[0]]),
                                );
                            }
                            _ => todo!(
                                "unimplemented opcode: {:?} in {:?}, of {:?}",
                                opcode,
                                inst,
                                self.fun.name
                            ),
                        },

                        IrInstructionData::BinaryImm64 {
                            opcode,
                            arg: _,
                            imm,
                        } => match opcode {
                            ir::Opcode::IaddImm => {
                                let width: Width = typevar_operand
                                    .as_ref()
                                    .map(type_of)
                                    .map(|z| z.into())
                                    .unwrap();
                                let use_vals: [LirOperand; 2] = [
                                    use_ir[0].into(),
                                    LirOperand::Machine(cg::Operand::Immediate(imm.bits())).into(),
                                ];

                                lirs.push(
                                    new_op(Op::IAdd(width))
                                        .with_use(&use_vals)
                                        .with_def(&[def_ir[0]]),
                                );
                            }
                            _ => todo!(
                                "unimplemented opcode: {:?} in {:?}, of {:?}",
                                opcode,
                                inst,
                                self.fun.name
                            ),
                        },
                        IrInstructionData::Brif {
                            opcode: _,
                            arg,
                            blocks,
                        } => {
                            // Conditional branch when cond (arg) is non-zero
                            // take 'then' (first block) when c != 0 and the 'else' branch otherwise.
                            // In the ideal case, we'd have the register allocator shuffle the values in the two
                            // blocks we need to handle the if statement.
                            debug!("Brif : {arg:#?}  {blocks:#?}");
                            // Model the branch as an instruction that reads one value... ignoring the fact that it
                            // writes values used by the branch for now.
                            lirs.push(new_op(Op::Test(Width::W64)).with_use::<LirOperand>(&[
                                use_ir[0].into(),
                                LirOperand::Machine(Operand::Immediate(i64::MAX)).into(),
                            ]));

                            // Collect the arguments that we'll end up using...
                            // The second lower will split this into the two blocks, so here it is a single block.
                            /*lirs.push(
                                new_op(Op::Jcc(cg::JumpCondition::IsZero)).with_use(&use_args),
                            );*/
                            let block_true = BlockCall {
                                block: block_map[&blocks[0].block(&dfg.value_lists)],
                                params: blocks[0]
                                    .args_slice(&dfg.value_lists)
                                    .iter()
                                    .map(|z| (*z).into())
                                    .collect(),
                            };
                            let block_false = BlockCall {
                                block: block_map[&blocks[1].block(&dfg.value_lists)],
                                params: blocks[1]
                                    .args_slice(&dfg.value_lists)
                                    .iter()
                                    .map(|z| (*z).into())
                                    .collect(),
                            };
                            let brif_data = BrifData {
                                condition: arg,
                                params: [block_true, block_false],
                            };
                            let brif_special = Special::Brif(brif_data);
                            s.special = Some(brif_special);
                        }

                        IrInstructionData::Jump {
                            opcode: _,
                            destination,
                        } => {
                            debug!("Jump : {destination:#?}");
                            let jump_data = JumpData {
                                call: BlockCall {
                                    block: block_map[&destination.block(&dfg.value_lists)],
                                    params: destination
                                        .args_slice(&dfg.value_lists)
                                        .iter()
                                        .map(|z| (*z).into())
                                        .collect(),
                                },
                            };

                            let jump_special = Special::Jump(jump_data);
                            s.special = Some(jump_special);
                        }
                        IrInstructionData::IntCompare {
                            opcode: _,
                            args: _,
                            cond,
                        } => {
                            let width: Width = typevar_operand
                                .as_ref()
                                .map(type_of)
                                .map(|z| z.into())
                                .unwrap();

                            // Lowers into a compare operation between the two registers.
                            lirs.push(new_op(Op::Cmp(width)).with_use(&[use_ir[0], use_ir[1]]));
                            // Followed by a conditional set based on the desired condition.
                            lirs.push(new_op(Op::SetCC(cond.into())).with_def(&[def_ir[0]]));
                        }

                        _ => todo!(
                            "unimplemented structure: {:?} in {:?}, of {:?}",
                            instdata,
                            inst,
                            self.fun.name
                        ),
                    }
                }

                // Use the lirs, actually put instruction indices.
                for l in lirs {
                    let new_id = Inst(self.instdata.len());
                    self.instdata.push(l);
                    s.lir_inst.push(new_id);
                    s.is_lowered = true;
                }
            }
        }
        // After that is done, we do a pass to split the brif blocks.
        self.split_blocks();

        // Check that we don't have duplicate block ids.
        {
            let mut blocks: std::collections::HashSet<BlockId> = Default::default();
            for b in self.blocks.iter() {
                if blocks.contains(&b.id) {
                    panic!("duplicate block id found in blocks");
                }
                blocks.insert(b.id);
            }
        }
    }

    pub fn prune(&mut self) {
        todo!("remove unused instructions, blocks")
    }

    pub fn reg_wrapper(&self) -> RegWrapper {
        RegWrapper::new(&self)
    }

    pub fn inst_data(&self, inst: Inst) -> Option<&InstructionData> {
        self.instdata.get(inst.0)
    }

    pub fn apply_regalloc(
        &mut self,
        wrapper: &RegWrapper,
        regs: &RegOutput,
        register_machine: &crate::RegisterMachine,
    ) {
        use cg::Reg;

        let rg2x = |p: regalloc2::PReg| register_machine.to_cg_reg(p.index());

        if !regs.edits.is_empty() {
            todo!("Need to handle edits");
        }

        self.stack_slots = regs.num_spillslots;

        // Loop must match the order in the wrapper creator.
        for (reginst, info) in wrapper.inst_info.iter() {
            //println!("regs: {regs:#?}");
            match info.inst {
                LirOrIrInst::Lir(inst) => {
                    let inst_data = &mut self.instdata[inst.0];
                    if !inst_data.has_virtuals() {
                        continue;
                    }
                    println!("@ {reginst:?} inst_data: {inst_data:#?}");
                    let allocs_args = regs.inst_allocs(*reginst);
                    let use_allocs = &allocs_args[0..];
                    println!("use_allocs.len: {} {:?}", use_allocs.len(), use_allocs);
                    let mut index = 0;
                    //let use_allocs = &allocs_args[0..inst_data.use_operands.len()];

                    for cont in [&mut inst_data.use_operands, &mut inst_data.def_operands] {
                        for z in cont.iter_mut() {
                            match z {
                                LirOperand::Virtual(_v) => {
                                    let this_alloc = use_allocs[index];
                                    if this_alloc.is_reg() {
                                        *z = LirOperand::Machine(cg::Operand::Reg(rg2x(
                                            use_allocs[index].as_reg().unwrap(),
                                        )));
                                    } else if this_alloc.is_stack() {
                                        let stack_slot = this_alloc.as_stack().unwrap();
                                        let stack_index = stack_slot.index();
                                        let stack_pos = (stack_index * 8) as i64;
                                        *z = LirOperand::Machine(cg::Operand::RegOffset(
                                            cg::RegOffset {
                                                reg: cg::Reg::EBP,
                                                offset: cg::Offset::Immediate(stack_pos),
                                            },
                                        ))
                                    } else {
                                        todo!("need to handle allocation: {this_alloc:?}");
                                    }
                                    index += 1;
                                }
                                _ => {}
                            }
                        }
                    }
                    // println!("after: {inst_data:#?}, index: {index} ops: {}", info.operands.len());
                    if index != use_allocs.len() {
                        panic!();
                    }
                }
                LirOrIrInst::Ir(inst) => {
                    //println!("reginst: {reginst:?}");
                    let allocs_args = regs.inst_allocs(*reginst);
                    let allocs = &allocs_args[0..];

                    // Find the section that has this instruction.
                    let mut block_si_and_ii = None;
                    for (bi, b) in self.blocks.iter().enumerate() {
                        for (si, s) in b.sections.iter().enumerate() {
                            for (ii, i) in s.ir_inst.iter().enumerate() {
                                if *i == inst {
                                    block_si_and_ii = Some((bi, si, ii));
                                }
                            }
                        }
                    }
                    let (bi, si, ii) =
                        block_si_and_ii.expect("could not find expected instruction");
                    self.blocks[bi].sections[si].ir_regs.resize(ii + 1, vec![]);
                    self.blocks[bi].sections[si].ir_regs[ii] =
                        allocs.iter().map(|v| rg2x(v.as_reg().unwrap())).collect();
                }
            }
        }
    }

    pub fn lower_second(&mut self) {
        let new_op = InstructionData::new;
        use cg::{Op, Operand, Width};

        for b in self.blocks.iter_mut() {
            for s in b.sections.iter_mut() {
                if s.is_prologue() {
                    // We only need a preamble once we need to use stack variables... though technically we have the
                    // red zone https://en.wikipedia.org/wiki/Red_zone_(computing) because we don't yet support calling
                    // other functions.

                    // Handle the calling convention, currently the nops are ordered correctly.
                    let arg_count = self.fun_args.len();
                    if arg_count > 6 {
                        todo!("handle functions with more than 6 arguments");
                    }

                    for (arg_i, input_reg) in cg::CALLING_CONVENTION_REGISTERS
                        .iter()
                        .take(arg_count)
                        .enumerate()
                    {
                        // Replace the NOP with a move.
                        let instructiondata = &mut self.instdata[s.lir_inst[arg_i].0];
                        println!("instructiondata: {instructiondata:?}");
                        instructiondata.operation = cg::Op::Mov(Width::W64);
                        instructiondata.use_operands = vec![cg::Operand::Reg(*input_reg).into()];
                        println!("instructiondata: {instructiondata:?}");
                    }

                    // Then insert the prologue
                    //

                    // For now, lets always add the function preamble.

                    // prologue is
                    //   1. push ebp
                    //   2. move esp into ebp
                    //   3. sub esp N
                    // 1. Push ebp:
                    let instdata = InstructionData::new(cg::Op::Push)
                        .with_use(&[LirOperand::Machine(cg::Operand::Reg(cg::Reg::EBP))]);
                    let new_id = Inst(self.instdata.len());
                    self.instdata.push(instdata);
                    s.lir_inst.insert(0, new_id);
                    // 2. move esp into ebp
                    //
                    let instdata = InstructionData::new(cg::Op::Mov(cg::Width::W64))
                        .with_use(&[LirOperand::Machine(cg::Operand::Reg(cg::Reg::ESP))])
                        .with_def(&[LirOperand::Machine(cg::Operand::Reg(cg::Reg::EBP))]);

                    let new_id = Inst(self.instdata.len());
                    self.instdata.push(instdata);
                    s.lir_inst.insert(1, new_id);

                    // 3. Finally, subtract from the stack pointer by the number of stack slots.
                    //
                    // I'm actually not sure we need this... given that we can rely on the stack to magically
                    // grow when we just use it...
                    //
                    if false {
                        let stack_slots = self.stack_slots;
                        let instdata = InstructionData::new(cg::Op::ISub(cg::Width::W64))
                            .with_use(&[
                                LirOperand::Machine(cg::Operand::Reg(cg::Reg::ESP)),
                                LirOperand::Machine(cg::Operand::Immediate(stack_slots as i64)),
                            ])
                            .with_def(&[LirOperand::Machine(cg::Operand::Reg(cg::Reg::ESP))]);
                        let new_id = Inst(self.instdata.len());
                        self.instdata.push(instdata);
                        s.lir_inst.insert(2, new_id);
                    }
                }
                println!("section: {s:?}");

                // Up to this point, jcc points towards to program points...
                // For the code generation is may only have one jump.
                // So we need to rewrite jcc(test) (dest1, dest2)
                // to
                // jcc test -> dest1
                // jump -> dest2
                // Jcc instructions are _ALWAYS_ at the end of the block, by definition
                // so we can just append the secondary jump at the end, and change jcc to have one
                // program point operand.
                // So here we handle jccs at the end of the block and add instructions we need
                if let Some(inst) = s.lir_inst.last().copied() {
                    let last_inst = self.instdata[inst.0].clone();
                    if let cg::Op::Jcc(jump_cond) = last_inst.operation {
                        // First, replace the last instruction with an instruction that jumps to dest1.
                        let new_id = Inst(self.instdata.len());
                        let l = new_op(Op::Jcc(jump_cond)).with_use(&[last_inst.use_operands[0]]);
                        self.instdata.push(l);
                        *s.lir_inst.last_mut().unwrap() = new_id;

                        // Else, jump to two.
                        let new_id = Inst(self.instdata.len());
                        let l = new_op(Op::Jump).with_use(&[last_inst.use_operands[1]]);
                        s.lir_inst.push(new_id);
                        self.instdata.push(l);
                    }
                }

                if s.is_lowered() {
                    continue;
                }
                let mut lirs = vec![];
                for (ii, inst) in s.ir_inst.iter().enumerate() {
                    let instdata = self.fun.dfg.insts[*inst];
                    match instdata {
                        IrInstructionData::MultiAry { opcode, args } => match opcode {
                            ir::Opcode::Return => {
                                if args.len(&self.fun.dfg.value_lists) != 1 {
                                    todo!()
                                }

                                // Pop the ebp register.
                                /*
                                let instdata = InstructionData::new(cg::Op::Mov(cg::Width::W64))
                                    .with_use(&[LirOperand::Machine(cg::Operand::Reg(
                                        cg::Reg::ESP,
                                    ))])
                                    .with_def(&[LirOperand::Machine(cg::Operand::Reg(
                                        cg::Reg::EBP,
                                    ))]);

                                let new_id = Inst(self.instdata.len());
                                self.instdata.push(instdata);
                                s.lir_inst.insert(1, new_id);*/

                                // This is thus the epilogue.
                                //  Move ebp back into esp
                                //  pop ebp
                                //  return.

                                lirs.push(
                                    new_op(Op::Mov(Width::W64))
                                        .with_use(&[Operand::Reg(cg::Reg::EBP)])
                                        .with_def(&[Operand::Reg(cg::Reg::ESP)]),
                                );

                                lirs.push(new_op(Op::Pop).with_use(&[Operand::Reg(cg::Reg::EBP)]));

                                // Move the return value.
                                lirs.push(
                                    new_op(Op::Mov(Width::W64))
                                        .with_use(&[Operand::Reg(s.ir_regs[ii][0])])
                                        .with_def(&[Operand::Reg(cg::Reg::EAX)]),
                                );
                                // Return.
                                lirs.push(new_op(Op::Return));
                            }
                            _ => todo!(
                                "unimplemented opcode: {:?} in {:?}, of {:?}",
                                opcode,
                                inst,
                                self.fun.name
                            ),
                        },
                        IrInstructionData::Brif {
                            opcode: _,
                            arg: _,
                            blocks: _,
                        } => {
                            panic!("should not be able to reach this");
                        }
                        _ => todo!(
                            "unimplemented structure: {:?} in {:?}, of {:?}",
                            instdata,
                            inst,
                            self.fun.name
                        ),
                    }
                }
                // Use the lirs, actually put instruction indices.
                for l in lirs {
                    let new_id = Inst(self.instdata.len());
                    self.instdata.push(l);
                    s.lir_inst.push(new_id);
                    s.is_lowered = true;
                }
            }
        }
    }

    pub fn patch_operations(&mut self) {
        let new_op = InstructionData::new;
        use cg::{Op, Operand, Width};

        for b in self.blocks.iter_mut() {
            for s in b.sections.iter_mut() {
                let mut new_inst = vec![];
                for (si, sint) in s.lir_inst.iter().enumerate() {
                    let instdata = &mut self.instdata[sint.0];
                    match instdata.operation {
                        cg::Op::IAdd(_) | cg::Op::ISub(_) | cg::Op::IMul(_) => {
                            let dest = instdata.def_operands[0];
                            let src0 = instdata.use_operands[0];
                            // let src1 = instdata.use_operands[1];

                            // Insert a new move that moves src0 into dest.
                            instdata.use_operands.remove(0);
                            new_inst.push((
                                si,
                                new_op(Op::Mov(Width::W64))
                                    .with_use(&[src0])
                                    .with_def(&[dest]),
                            ));
                        }
                        _ => {}
                    }
                }
                new_inst.reverse();
                for (si, d) in new_inst {
                    let new_id = Inst(self.instdata.len());
                    s.lir_inst.insert(si, new_id);
                    self.instdata.push(d);
                }
            }
        }
    }

    pub fn assemble(&self) -> Vec<u8> {
        let mut v = vec![];

        let mut tracker = CodePointTracker::default();

        // Build the function from the rear, that way we know for sure that all code points have been encountered.
        //

        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        enum Stage {
            CodePoints,
            Actual,
        }
        for stage in [Stage::CodePoints, Stage::Actual] {
            v.clear();
            let mut dist_from_rear = 0;
            if stage == Stage::Actual {
                println!("tracker: {tracker:#?}");
            }
            for b in self.blocks.iter().rev() {
                for s in b.sections.iter().rev() {
                    for i in s.lir_inst.iter().rev() {
                        debug!("assembling: {:?}", self.instdata[i.0]);
                        let mut instdata_copy = self.instdata[i.0].clone();
                        for op in instdata_copy.operands_iter_mut() {
                            if let LirOperand::ProgramPoint(p) = op {
                                match stage {
                                    Stage::CodePoints => {
                                        *op = LirOperand::Machine(cg::Operand::Immediate(0));
                                    }
                                    Stage::Actual => {
                                        let mut offset = tracker.relative(*p, dist_from_rear);
                                        if offset == 0x5f {
                                            dbg!(s);
                                        }
                                        *op = LirOperand::Machine(cg::Operand::Immediate(offset));
                                    }
                                }
                            }
                        }
                        println!("instdata_copy: {instdata_copy:?}");
                        let z = instdata_copy.assemble().unwrap();
                        dist_from_rear += z.len();
                        v.push(z);
                    }
                }
                if stage == Stage::CodePoints {
                    // Only when we reach the end of the sections in the block, do we find the block start position.
                    tracker.add_blockstart(&self, b.id, dist_from_rear);
                }
            }
        }

        // Finally, reverse the assembled instructions, and out falls the function.
        let mut fun: Vec<u8> = vec![];
        for machine_inst in v.drain(..).rev() {
            fun.extend(machine_inst);
        }

        fun
    }

    pub fn dump(&self) {
        for b in self.blocks.iter() {
            println!(
                "{:?}  ({:?})  (pred: {:?}) (succ: {:?})",
                b.id, b.block_params, b.block_preds, b.block_succs
            );
            for (si, s) in b.sections.iter().enumerate() {
                println!(" s{si}  {:?}", s.special);
                if s.is_lowered() {
                    for inst in s.lir_inst.iter() {
                        let instdata = &self.instdata[inst.0];
                        println!("   {instdata:?}");
                    }
                } else {
                    for inst in s.ir_inst.iter() {
                        let instdata = self.fun.dfg.insts[*inst];
                        println!("   {instdata:?}");
                    }
                }
            }
            println!();
        }
    }
}

// use cranelift_codegen::ir::Inst as IrInst;
use regalloc2::Block as RegBlock;
use regalloc2::Function as RegFunction;
use regalloc2::Inst as RegInst;
use regalloc2::Operand as RegOperand;
use regalloc2::Output as RegOutput;
use regalloc2::{InstRange, PRegSet, RegClass, VReg};

#[derive(Debug)]
enum LirOrIrInst {
    Lir(Inst),
    Ir(IrInst),
}
impl From<Inst> for LirOrIrInst {
    fn from(v: Inst) -> LirOrIrInst {
        LirOrIrInst::Lir(v)
    }
}
impl From<IrInst> for LirOrIrInst {
    fn from(v: IrInst) -> LirOrIrInst {
        LirOrIrInst::Ir(v)
    }
}

#[derive(Debug)]
struct InstInfo {
    is_ret: bool,
    is_branch: bool,
    operands: Vec<RegOperand>,
    inst: LirOrIrInst,
    description: String,
}

#[derive(Debug)]
pub struct RegWrapper {
    num_insts: usize,
    num_blocks: usize,
    num_values: usize,
    entry_block: RegBlock,
    block_insn: HashMap<RegBlock, InstRange>,
    block_params: HashMap<RegBlock, Vec<VReg>>,
    block_succs: HashMap<RegBlock, Vec<RegBlock>>,
    branch_blockparam: HashMap<(RegBlock, RegInst), Vec<Vec<VReg>>>,
    block_preds: HashMap<RegBlock, Vec<RegBlock>>,
    inst_info: HashMap<RegInst, InstInfo>,
    value_info: HashMap<Value, VReg>,

    regblock_to_block: HashMap<RegBlock, BlockId>,

    fun: CraneliftIrFunction,
}

impl RegWrapper {
    pub fn new(lirfun: &Function) -> Self {
        let mut block_insn: HashMap<RegBlock, InstRange> = Default::default();
        let mut block_params: HashMap<RegBlock, Vec<VReg>> = Default::default();
        let mut block_succs: HashMap<RegBlock, Vec<RegBlock>> = Default::default();
        let mut block_preds: HashMap<RegBlock, Vec<RegBlock>> = Default::default();
        let mut inst_info: HashMap<RegInst, InstInfo> = Default::default();
        let mut value_info: HashMap<Value, VReg> = Default::default();
        let mut branch_blockparam: HashMap<(RegBlock, RegInst), Vec<Vec<VReg>>> =
            Default::default();
        let mut regblock_to_block: HashMap<RegBlock, BlockId> = Default::default();
        let mut block_to_regblock: HashMap<BlockId, RegBlock> = Default::default();

        // Make the block lookup.
        {
            for (i, b) in lirfun.blocks.iter().enumerate() {
                let lirid = b.id;
                let regblock = RegBlock::new(i);
                regblock_to_block.insert(regblock, lirid);
                block_to_regblock.insert(lirid, regblock);
            }
        }

        let fun = &lirfun.fun;
        let entry_block: RegBlock = block_to_regblock[&lirfun
            .entry_block
            .expect("regalloc function must have entry block")];

        let mut first_inst_in_fun = None;

        for b in lirfun.blocks.iter() {
            let regblock = block_to_regblock[&b.id];

            let mut first_inst = None;
            let mut last_inst = None;

            for s in b.sections.iter() {
                if s.is_lowered() {
                    // if it is lowered, we operate on the LIR.
                    let insts_in_section = s.lir_inst.len();
                    for (inst_i, inst) in s.lir_inst.iter().enumerate() {
                        let data = lirfun.inst_data(*inst).expect("ill formed function");
                        let is_last_inst = inst_i == insts_in_section - 1;
                        let mut operands = vec![];
                        for z in data.use_operands.iter() {
                            match z {
                                LirOperand::Virtual(v) => {
                                    // Should do something here with retrieving the constraints of the opcode
                                    let regtype = RegClass::Int;
                                    let vreg = value_info
                                        .entry(*v)
                                        .or_insert_with(|| VReg::new(v.as_u32() as usize, regtype));
                                    let operand = RegOperand::new(
                                        *vreg,
                                        regalloc2::OperandConstraint::Any,
                                        regalloc2::OperandKind::Use,
                                        regalloc2::OperandPos::Early,
                                    );
                                    operands.push(operand);
                                }
                                LirOperand::Machine(r) => {
                                    match r {
                                        cg::Operand::Immediate(_) => {
                                            // Not actually an operand for register allocation.
                                        }
                                        cg::Operand::Reg(r) => {
                                            todo!("{r:?}");
                                        }
                                        cg::Operand::RegOffset(r) => {
                                            todo!("{r:?}");
                                        }
                                    }
                                }
                                LirOperand::ProgramPoint(_) => {}
                            }
                        }
                        for z in data.def_operands.iter() {
                            match z {
                                LirOperand::Virtual(v) => {
                                    // Should do something here with retrieving the constraints of the opcode
                                    let regtype = RegClass::Int;
                                    let vreg = value_info
                                        .entry(*v)
                                        .or_insert_with(|| VReg::new(v.as_u32() as usize, regtype));
                                    let operand = RegOperand::new(
                                        *vreg,
                                        regalloc2::OperandConstraint::Any,
                                        regalloc2::OperandKind::Def,
                                        regalloc2::OperandPos::Late,
                                    );
                                    operands.push(operand);
                                }
                                LirOperand::Machine(r) => {
                                    match r {
                                        cg::Operand::Immediate(_) => {
                                            // Not actually an operand for register allocation.
                                        }
                                        cg::Operand::Reg(r) => {
                                            todo!("{r:?} for {data:?}");
                                        }
                                        cg::Operand::RegOffset(r) => {
                                            todo!("{r:?}");
                                        }
                                    }
                                }
                                LirOperand::ProgramPoint(_) => {}
                            }
                        }

                        let reg_inst = RegInst::new(inst_info.len());

                        if is_last_inst {
                            if let Some(special) = s.special.as_ref() {
                                match special {
                                    Special::Prologue => {}
                                    Special::Brif(brif_data) => {
                                        let key = (regblock, reg_inst);
                                        let mut block_values = vec![];
                                        for b in brif_data.params.iter() {
                                            let mut this_call_values = vec![];
                                            for v in b.params.iter() {
                                                let vreg = value_info
                                                    .entry(*v)
                                                    .or_insert_with(|| {
                                                        let regtype = RegClass::Int;
                                                        VReg::new(v.as_u32() as usize, regtype)
                                                    })
                                                    .clone();
                                                this_call_values.push(vreg);
                                            }
                                            block_values.push(this_call_values);
                                        }
                                        branch_blockparam.insert(key, block_values);
                                    }
                                    Special::Jump(jump_data) => {
                                        let key = (regblock, reg_inst);
                                        let mut block_values = vec![];
                                        let mut this_call_values = vec![];
                                        for v in jump_data.call.params.iter() {
                                            let vreg = value_info
                                                .entry(*v)
                                                .or_insert_with(|| {
                                                    let regtype = RegClass::Int;
                                                    VReg::new(v.as_u32() as usize, regtype)
                                                })
                                                .clone();
                                            this_call_values.push(vreg);
                                        }
                                        block_values.push(this_call_values);
                                        branch_blockparam.insert(key, block_values);
                                    }
                                }
                            }
                        }

                        if first_inst.is_none() {
                            first_inst = Some(reg_inst);
                        }
                        if first_inst_in_fun.is_none() {
                            first_inst_in_fun = Some(reg_inst);
                        }
                        last_inst = Some(reg_inst);

                        let is_ret = data.operation.is_return() && is_last_inst;
                        let is_branch = data.operation.is_branch() && is_last_inst;
                        let mut description = data.simple_string();
                        if let Some(special) = s.special.as_ref() {
                            match special {
                                Special::Prologue => {}
                                Special::Brif(brif_data) => {
                                    if is_last_inst {
                                        description += &format!(
                                            " -> {}",
                                            brif_data
                                                .params
                                                .iter()
                                                .map(|v| format!("{:?}", v.block))
                                                .collect::<Vec<String>>()
                                                .join(", ")
                                        );
                                    }
                                }
                                Special::Jump(jump_data) => {
                                    if is_last_inst {
                                        description += &format!(" -> {:?}", jump_data.call.block);
                                    }
                                }
                            }
                        }

                        let info = InstInfo {
                            is_ret,
                            is_branch,
                            operands,
                            inst: (*inst).into(),
                            description,
                        };
                        inst_info.insert(reg_inst, info);
                    }
                } else {
                    // if it is not lowered, we operate on the IR.
                    for irinst in s.ir_inst.iter() {
                        let instdata = fun.dfg.insts[*irinst];

                        let mut operands = vec![];

                        // Input arguments to instruction.
                        let arguments = instdata.arguments(&fun.dfg.value_lists);
                        // println!("Adding arguments to ir inst: {arguments:?} {irinst:?}");
                        for v in arguments {
                            let valuetype = fun.dfg.value_type(*v);
                            let regtype = if valuetype.is_int() {
                                RegClass::Int
                            } else {
                                RegClass::Float
                            };
                            let operand = RegOperand::new(
                                VReg::new(v.as_u32() as usize, regtype),
                                regalloc2::OperandConstraint::Any,
                                regalloc2::OperandKind::Use,
                                regalloc2::OperandPos::Early,
                            );
                            operands.push(operand);
                        }
                        // If this is a branch, add the values used by the block calls as use operands.
                        if let Some(special) = &s.special {
                            match &special {
                                Special::Prologue => todo!(),
                                Special::Brif(data) => {
                                    for b in data.params.iter() {
                                        for branch_param in b.params.iter() {
                                            let valuetype = fun.dfg.value_type(*branch_param);
                                            let regtype = if valuetype.is_int() {
                                                RegClass::Int
                                            } else {
                                                RegClass::Float
                                            };
                                            let operand = RegOperand::new(
                                                VReg::new(branch_param.as_u32() as usize, regtype),
                                                regalloc2::OperandConstraint::Any,
                                                regalloc2::OperandKind::Use,
                                                regalloc2::OperandPos::Early,
                                            );
                                            operands.push(operand);
                                        }
                                    }
                                }
                                Special::Jump(jump_data) => {
                                    for branch_param in jump_data.call.params.iter() {
                                        let valuetype = fun.dfg.value_type(*branch_param);
                                        let regtype = if valuetype.is_int() {
                                            RegClass::Int
                                        } else {
                                            RegClass::Float
                                        };
                                        let operand = RegOperand::new(
                                            VReg::new(branch_param.as_u32() as usize, regtype),
                                            regalloc2::OperandConstraint::Any,
                                            regalloc2::OperandKind::Use,
                                            regalloc2::OperandPos::Early,
                                        );
                                        operands.push(operand);
                                    }
                                }
                            }
                        }

                        // Results of instruction.
                        for r in fun.dfg.inst_results(*irinst) {
                            let valuetype = fun.dfg.value_type(*r);
                            let regtype = if valuetype.is_int() {
                                RegClass::Int
                            } else {
                                RegClass::Float
                            };
                            let operand = RegOperand::new(
                                VReg::new(r.as_u32() as usize, regtype),
                                regalloc2::OperandConstraint::Any,
                                regalloc2::OperandKind::Def,
                                regalloc2::OperandPos::Late,
                            );
                            operands.push(operand);
                        }

                        let is_ret = instdata.opcode().is_return();
                        let is_branch = instdata.opcode().is_branch()
                            || matches!(s.special, Some(Special::Brif(_)) | Some(Special::Jump(_)));
                        let mut w = cranelift_codegen::write::PlainWriter {};
                        let mut description = String::new();
                        w.write_instruction(
                            &mut description,
                            &fun,
                            &Default::default(),
                            *irinst,
                            0,
                        )
                        .expect("write failed");
                        let info = InstInfo {
                            is_ret,
                            is_branch,
                            operands,
                            inst: (*irinst).into(),
                            description,
                        };
                        println!(" irinst {irinst:?} with {info:?}");
                        let reg_inst = RegInst::new(inst_info.len());

                        // If it is a branch, add the data for this branch to the map that's used to lookup the branch
                        // parameters for the register allocation.

                        if let Some(special) = s.special.as_ref() {
                            match special {
                                Special::Prologue => {}
                                Special::Brif(brif_data) => {
                                    let key = (regblock, reg_inst);
                                    let mut block_values = vec![];
                                    for b in brif_data.params.iter() {
                                        let mut this_call_values = vec![];
                                        for v in b.params.iter() {
                                            let vreg = value_info
                                                .entry(*v)
                                                .or_insert_with(|| {
                                                    let regtype = RegClass::Int;
                                                    VReg::new(v.as_u32() as usize, regtype)
                                                })
                                                .clone();
                                            this_call_values.push(vreg);
                                        }
                                        block_values.push(this_call_values);
                                    }
                                    branch_blockparam.insert(key, block_values);
                                }
                                Special::Jump(jump_data) => {
                                    let key = (regblock, reg_inst);
                                    let mut block_values = vec![];
                                    let mut this_call_values = vec![];
                                    for v in jump_data.call.params.iter() {
                                        let vreg = value_info
                                            .entry(*v)
                                            .or_insert_with(|| {
                                                let regtype = RegClass::Int;
                                                VReg::new(v.as_u32() as usize, regtype)
                                            })
                                            .clone();
                                        this_call_values.push(vreg);
                                    }
                                    block_values.push(this_call_values);
                                    branch_blockparam.insert(key, block_values);
                                }
                            }
                        }

                        if first_inst.is_none() {
                            first_inst = Some(reg_inst);
                        }
                        if first_inst_in_fun.is_none() {
                            first_inst_in_fun = Some(reg_inst);
                        }
                        last_inst = Some(reg_inst);

                        inst_info.insert(reg_inst, info);
                    }
                }
            }

            let last_plus_one = last_inst.map(|v| RegInst::new(v.0 as usize + 1));

            let range = InstRange::new(
                first_inst.expect("block should have instruction"),
                last_plus_one.expect("block should have instruction"),
            );
            println!("Assigning {regblock:?} with {range:?}");
            block_insn.insert(regblock, range);

            block_succs.insert(
                regblock,
                b.block_succs
                    .iter()
                    .map(|v| block_to_regblock[&v])
                    .collect(),
            );

            block_preds.insert(
                regblock,
                b.block_preds
                    .iter()
                    .map(|v| block_to_regblock[&v])
                    .collect(),
            );

            let these_block_params = block_params.entry(regblock).or_default();
            for v in b.block_params.iter() {
                let regtype = RegClass::Int;
                let vreg = value_info
                    .entry(*v)
                    .or_insert_with(|| VReg::new(v.as_u32() as usize, regtype));
                these_block_params.push(*vreg);
            }
        }

        let num_insts = inst_info.len();
        let num_blocks = block_insn.len();
        //println!("num_blocks: {num_blocks:#?}");

        let num_values = value_info.len();

        //println!("block_insn: {block_insn:#?}");
        //println!("block_succs: {block_succs:#?}");
        //println!("block_preds: {block_preds:#?}");

        {
            // Check that the values are consecutively numbered.
            let mut keys = value_info.keys().collect::<Vec<_>>();
            keys.sort();
            for (i, k) in keys.iter().enumerate() {
                println!("i: {i:?}, k: {k:?}");
                if i != k.as_u32() as usize {
                    //panic!("value keys aren't consecutive");
                }
            }
        }

        let fun = fun.clone();
        Self {
            num_insts,
            num_blocks,
            num_values,
            entry_block,
            block_insn,
            block_params,
            block_succs,
            block_preds,
            regblock_to_block,
            branch_blockparam,
            inst_info,
            value_info,
            fun,
        }
    }
}

impl RegFunction for RegWrapper {
    fn num_insts(&self) -> usize {
        self.num_insts
    }
    fn num_blocks(&self) -> usize {
        self.num_blocks
    }
    fn entry_block(&self) -> RegBlock {
        self.entry_block
    }
    fn block_insns(&self, block: regalloc2::Block) -> InstRange {
        self.block_insn[&block]
    }
    fn block_succs(&self, block: regalloc2::Block) -> &[regalloc2::Block] {
        //println!("block succ: {block:?}");
        //println!("all: {:?}", self.block_succs);
        &self.block_succs[&block]
    }
    fn block_preds(&self, block: regalloc2::Block) -> &[regalloc2::Block] {
        //println!("block pred: {block:?}");
        //println!("all: {:?}", self.block_preds);
        &self.block_preds[&block]
    }
    fn block_params(&self, block: regalloc2::Block) -> &[VReg] {
        //println!("block pred: {block:?}");
        //println!("blockparams: {:?}", self.block_params);
        if block == self.entry_block {
            return &[];
        }
        &self.block_params[&block]
    }
    fn is_ret(&self, reginst: regalloc2::Inst) -> bool {
        self.inst_info[&reginst].is_ret
    }
    fn is_branch(&self, reginst: regalloc2::Inst) -> bool {
        self.inst_info[&reginst].is_branch
    }
    fn branch_blockparams(
        &self,
        block: regalloc2::Block,
        inst: regalloc2::Inst,
        succs_index: usize,
    ) -> &[VReg] {
        if let Some(res) = self.branch_blockparam.get(&(block, inst)) {
            &res[succs_index]
        } else {
            &[]
        }
    }
    fn inst_operands(&self, reginst: regalloc2::Inst) -> &[regalloc2::Operand] {
        &self.inst_info[&reginst].operands
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

impl grus_regalloc::svg::FunPrinter for RegWrapper {
    fn inst(&self, inst: RegInst) -> String {
        self.inst_info[&inst].description.clone()
    }

    fn block_start(&self, block: RegBlock) -> String {
        let _ = block;
        todo!()
    }
}

pub fn write_regfunction(
    fun: &impl RegFunction,
    printer: &impl grus_regalloc::svg::FunPrinter,
) -> Vec<String> {
    let mut rows = vec![];

    let mut block_stack: std::collections::VecDeque<RegBlock> = Default::default();
    block_stack.push_back(fun.entry_block());

    while let Some(block) = block_stack.pop_front() {
        // Block start:
        let bparam = fun.block_params(block);

        let joined = bparam
            .iter()
            .map(|z| format!("v{}", z.vreg()))
            .collect::<Vec<String>>()
            .join(", ");

        rows.push(format!("block{:}({joined:})", block.raw_u32()));

        // Instructions.
        for inst in fun.block_insns(block).iter() {
            rows.push(format!("{inst:?}  {}", printer.inst(inst)));
        }

        rows.push(format!("{}", ""));
        block_stack.extend(fun.block_succs(block));
    }
    rows
}
