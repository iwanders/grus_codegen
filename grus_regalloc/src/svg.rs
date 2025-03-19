use regalloc2::Block as RegBlock;
use regalloc2::Function as RegFunction;
use regalloc2::Inst as RegInst;
use regalloc2::MachineEnv as RegMachineEnv;
use regalloc2::Operand as RegOperand;
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
}
impl Default for VisualisationOptions {
    fn default() -> Self {
        VisualisationOptions {
            row_height: 10.0,
            dpi: 100.0,
            font_size: 1.0,
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
}
impl Row {
    pub fn space() -> Self {
        Row {
            row_type: RowType::Space,
            descr: "".to_owned(),
            operands: vec![],
        }
    }
}

#[derive(Default, Clone, Debug)]
struct RegisterGrid {
    rows: Vec<Row>,
}
use svg::node::element::{Group, Text};
impl RegisterGrid {
    fn to_group(&self, options: &VisualisationOptions) -> Group {
        let mut group = svg::node::element::Group::new();
        for (i, r) in self.rows.iter().enumerate() {
            let pos_y = i as f32 * options.row_height;

            let t = Text::new(&r.descr)
                .set("x", 0.0)
                .set("y", pos_y)
                .set("text-anchor", "start")
                .set("font-size", format!("{}em", options.font_size))
                .set("fill", "black");
            group.append(t);
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
        let block_start_row = Row {
            row_type: RowType::BlockStart(block),
            descr: format!("block({bparam:?}"),
            operands: vec![],
        };
        grid.rows.push(block_start_row);

        // Instructions.
        for inst in fun.block_insns(block).iter() {
            let inst_row = Row {
                row_type: RowType::Instruction(inst),
                descr: format!("{}", printer.inst(inst)),
                operands: fun.inst_operands(inst).to_vec(),
            };
            grid.rows.push(inst_row);
        }

        grid.rows.push(Row::space());
        block_stack.extend(fun.block_succs(block));
    }

    /*
    let data = Data::new()
        .move_to((-1000, 1000))
        .line_to((1000, 1000))
        .line_to((1000, -1000))
        .line_to((-1000, -1000))
        .close();

    let path = Path::new().set("fill", "black").set("d", data);
    */
    let rows = grid.rows.len();
    let height = rows as f32 * options.row_height;
    let width = 100.0;

    let group = grid.to_group(&options);

    Document::new()
        .set("viewBox", (0, 0, 400, height)) // from -200,-200, width and height of 400.
        .set("width", format!("{}px", width * options.dpi))
        .set("height", format!("{}px", height * options.dpi))
        .add(group)
}
