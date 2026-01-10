#[derive(Debug, Clone, PartialEq)]
pub struct LayoutState {
    pub indent_level: usize,
    pub pending_newline: bool,
}