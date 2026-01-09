use crate::utils::{InternedString, Span};

#[derive(Debug, Clone, PartialEq)]
pub struct TokenTree {
    pub kind: TokenTreeKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenTreeKind {
    Delim(Delim, Vec<TokenTreeKind>),
    Punct(Punct),
    Ident(Ident),
    Lit(Lit),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Delim {
    Paren,
    Brace,
    Bracket,
    Layout,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Punct {
    Bang,
    Backslash,
    Colon,
    Semi,
    Comma,
    Dot,
    Eq,
    LArrow,
    RArrow,
    LFatArrow,
    Bar,
    Underscore,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ident {
    Error,
    Lower(InternedString),
    Upper(InternedString),
    Op,
    ConOp,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Int(i64),
    Real(f64),
    String(InternedString),
    Char(char),
}
