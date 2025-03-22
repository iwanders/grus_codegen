use regalloc2::Allocation as RegAllocation;
use regalloc2::Block as RegBlock;
use regalloc2::Function as RegFunction;
use regalloc2::Inst as RegInst;
use regalloc2::MachineEnv as RegMachineEnv;
use regalloc2::Operand as RegOperand;
use regalloc2::OperandKind;
use regalloc2::Output as RegOutput;
use regalloc2::PReg as RegPReg;
use svg::Document;
use svg::Node;

pub use svg;

#[derive(Debug)]
pub struct VisualisationOptions {
    pub row_height: f32,
    pub dpi: f32,
    pub font_size: f32,
    pub row_code_width: f32,
    pub row_position_width: f32,
}
impl Default for VisualisationOptions {
    fn default() -> Self {
        VisualisationOptions {
            row_height: 10.0,
            dpi: 100.0,
            font_size: 1.0,
            row_code_width: 150.0,
            row_position_width: 20.0,
        }
    }
}

pub trait FunPrinter {
    /// Print a single instruction.
    fn inst(&self, inst: RegInst) -> String;
    /// Print the start of block section.
    fn block_start(&self, block: RegBlock) -> String;
}

#[derive(Clone, Debug, Copy)]
enum RowType {
    Space,
    BlockStart(RegBlock),
    Instruction(RegInst),
}

#[derive(Clone, Debug)]
struct Row {
    row_type: RowType,
    descr: String,
    operands: Vec<RegOperand>,
    alloc: Vec<RegAllocation>,
}
impl Row {
    pub fn space() -> Self {
        Row {
            row_type: RowType::Space,
            descr: "".to_owned(),
            operands: vec![],
            alloc: vec![],
        }
    }
}

#[derive(Clone, Debug)]
struct ValuePosition {
    offset: f32,
    heading: String,
    alloc: RegAllocation,
}

#[derive(Default, Clone, Debug)]
struct RegisterGrid {
    rows: Vec<Row>,
    values: Vec<ValuePosition>,
}
use svg::node::element::{Group, Text};
impl RegisterGrid {
    fn to_group(&self, options: &VisualisationOptions) -> Group {
        let mut group = svg::node::element::Group::new();
        for (i, r) in self.rows.iter().enumerate() {
            let pos_y = (i + 1) as f32 * options.row_height;

            let t = Text::new(&r.descr)
                .set("x", 0.0)
                .set("y", pos_y)
                .set("text-anchor", "start")
                .set("font-size", format!("{}em", options.font_size))
                .set("fill", "black");
            group.append(t);
        }

        let mut preg_pos = std::collections::HashMap::new();

        for value in self.values.iter() {
            preg_pos.insert(value.alloc, value.offset);
            let t = Text::new(&value.heading)
                .set("x", value.offset)
                .set("y", 0.0)
                .set("text-anchor", "start")
                .set("font-size", format!("{}em", options.font_size))
                .set("fill", "black");
            group.append(t);
        }

        for (i, r) in self.rows.iter().enumerate() {
            let pos_y = (i + 1) as f32 * options.row_height;

            for (op_i, def_v) in r.operands.iter().enumerate() {
                if op_i >= r.alloc.len() {
                    println!("failed to find alloc for {op_i}, at {r:?}");
                    continue;
                }
                let this_alloc = r.alloc[op_i];
                let vreg = def_v.vreg();
                if let Some(_alloc_preg) = this_alloc.as_reg() {
                    let pos = preg_pos[&this_alloc];
                    let t = Text::new(format!("v{:?}", vreg.vreg()))
                        .set("x", pos)
                        .set("y", pos_y)
                        .set("text-anchor", "start")
                        .set("font-size", format!("{}em", options.font_size))
                        .set("fill", "black");
                    group.append(t);
                }
                if def_v.kind() == OperandKind::Def {}
            }
        }

        group
    }
}

/*
Simple grid, at each row we have:
- A space
- A block start
- An instruction
Rows like:
|   row string | reg0 | reg1 | reg2 | ... | spill0 | spill1 | spill2...

Then at the assignment, we draw `v#` into the cell.
Color down until a new value is assigned?

Order is just the block order.
Instructions are just by instruction order.
*/

pub fn short_preg_str(preg: &RegPReg) -> String {
    match preg.class() {
        regalloc2::RegClass::Int => format!("i{}", preg.index()),
        regalloc2::RegClass::Float => format!("e{}", preg.index()),
        regalloc2::RegClass::Vector => format!("v{}", preg.index()),
    }
}

pub fn register_document(
    fun: &impl RegFunction,
    output: &RegOutput,
    env: &RegMachineEnv,
    options: &VisualisationOptions,
    printer: &impl FunPrinter,
) -> Document {
    use svg::node::element::path::Data;
    use svg::node::element::Path;

    let mut grid = RegisterGrid::default();

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

        let block_start_row = Row {
            row_type: RowType::BlockStart(block),
            descr: format!("block{:}({joined:})", block.raw_u32()),
            operands: vec![],
            alloc: vec![],
        };
        grid.rows.push(block_start_row);

        // Instructions.
        for inst in fun.block_insns(block).iter() {
            let allocs_args = output.inst_allocs(inst);
            let use_allocs = &allocs_args[0..];
            let allocations = use_allocs.to_vec();
            let inst_row = Row {
                row_type: RowType::Instruction(inst),
                descr: format!("{}", printer.inst(inst)),
                operands: fun.inst_operands(inst).to_vec(),
                alloc: allocations,
            };
            grid.rows.push(inst_row);
        }

        grid.rows.push(Row::space());
        block_stack.extend(fun.block_succs(block));
    }

    if output.num_spillslots != 0 {
        todo!("still need to implement spillslot handling");
    }
    if !env.fixed_stack_slots.is_empty() {
        todo!();
    }

    let mut xoffset = options.row_code_width;
    for pref_regs in env.preferred_regs_by_class.iter() {
        for preg in pref_regs.iter() {
            let value_entry = ValuePosition {
                offset: xoffset,
                heading: short_preg_str(preg),
                alloc: RegAllocation::reg(*preg),
            };
            grid.values.push(value_entry);
            xoffset += options.row_position_width;
        }
    }

    // Iterate through the registers in our machine.
    //

    /*
     */
    let data = Data::new()
        .move_to((-1000, 1000))
        .line_to((1000, 1000))
        .line_to((1000, -1000))
        .line_to((-1000, -1000))
        .close();

    let path = Path::new().set("fill", "white").set("d", data);
    let rows = grid.rows.len();
    let height = rows as f32 * options.row_height;
    let width = 100.0;

    let group = grid.to_group(&options);

    Document::new()
        .set("viewBox", (0, 0, 400, height)) // from -200,-200, width and height of 400.
        .set("width", format!("{}px", width * options.dpi))
        .set("height", format!("{}px", height * options.dpi))
        .add(path)
        .add(group)
}
