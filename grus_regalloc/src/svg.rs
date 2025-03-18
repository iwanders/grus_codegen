use regalloc2::Block as RegBlock;
use regalloc2::Function as RegFunction;
use regalloc2::Inst as RegInst;
use regalloc2::Output as RegOutput;
use svg::Document;
use svg::Node;

pub use svg;

#[derive(Debug, Default)]
pub struct VisualisationOptions {}

pub trait FunPrinter {
    /// Print a single instruction.
    fn inst(&self, inst: RegInst) -> String;
    /// Print the start of block section.
    fn block_start(&self, block: RegBlock) -> String;
}

pub fn register_document(
    fun: &impl RegFunction,
    output: &RegOutput,
    options: &VisualisationOptions,
    printer: &impl FunPrinter,
) -> Document {
    use svg::node::element::path::Data;
    use svg::node::element::Path;

    let data = Data::new()
        .move_to((-1000, 1000))
        .line_to((1000, 1000))
        .line_to((1000, -1000))
        .line_to((-1000, -1000))
        .close();

    let path = Path::new().set("fill", "black").set("d", data);

    Document::new()
        .set("viewBox", (-200, -200, 400, 400)) // from -200,-200, width and height of 400.
        .set("width", "2000px")
        .set("height", "2000px")
        .add(path)
}
