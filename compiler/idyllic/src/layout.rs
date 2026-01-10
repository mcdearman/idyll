#[derive(Debug, Clone, PartialEq)]
pub struct LayoutState {
    pub indent_level: usize,
    pub pending_newline: bool,
    pub layout_stack: Vec<LayoutKind>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LayoutKind {
    Block,
    Indent(usize),
}
