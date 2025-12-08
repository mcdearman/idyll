use idyllic_utils::intern::InternedString;

use crate::token::TokenKind;

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name: Ident,
    pub use_decls: Vec<UseDecl>,
    pub record_defs: Vec<StructDef>,
    pub data_defs: Vec<DataDef>,
    pub type_aliases: Vec<TypeAlias>,
    pub defs: Vec<Def>,
    pub fn_defs: Vec<FnDef>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UseDecl {
    pub module: Ident,
    pub alias: Option<Ident>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDef {
    pub name: Ident,
    pub type_params: Vec<Ident>,
    pub fields: Vec<(Ident, AnnId)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DataDef {
    pub name: Ident,
    pub type_params: Vec<Ident>,
    pub cons: Vec<(Ident, Vec<AnnId>)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAlias {
    pub name: Ident,
    pub type_params: Vec<Ident>,
    pub ty_anno: AnnId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Def {
    pub pat: PatId,
    pub expr: ExprId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnDef {
    pub name: Ident,
    pub ty_anno: AnnId,
    pub params: Vec<PatId>,
    pub body: ExprId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprId(pub usize);

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Lit(Lit),
    Var(Ident),
    App(ExprId, Vec<ExprId>),
    Lam(Vec<PatId>, ExprId),
    UnaryOp(UnaryOp, ExprId),
    BinaryOp(BinaryOp, ExprId, ExprId),
    Or(ExprId, ExprId),
    And(ExprId, ExprId),
    Let(Bind, ExprId),
    If(ExprId, ExprId, ExprId),
    Match(ExprId, Vec<(PatId, ExprId)>),
    List(Vec<ExprId>),
    Unit,
    Error,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Bind {
    Pat(PatId, ExprId),
    Fn(Ident, Vec<PatId>, ExprId),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum UnaryOp {
    Neg,
}

impl From<TokenKind> for UnaryOp {
    fn from(t: TokenKind) -> Self {
        match t {
            TokenKind::Minus => Self::Neg,
            _ => unreachable!(),
        }
    }
}

impl ToString for UnaryOp {
    fn to_string(&self) -> String {
        match self {
            Self::Neg => "__neg__",
        }
        .to_string()
    }
}

impl From<UnaryOp> for InternedString {
    fn from(value: UnaryOp) -> Self {
        InternedString::from(value.to_string())
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Pow,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
}

impl From<TokenKind> for BinaryOp {
    fn from(t: TokenKind) -> Self {
        match t {
            TokenKind::Plus => Self::Add,
            TokenKind::Minus => Self::Sub,
            TokenKind::Star => Self::Mul,
            TokenKind::Slash => Self::Div,
            TokenKind::Percent => Self::Rem,
            TokenKind::Caret => Self::Pow,
            TokenKind::Eq => Self::Eq,
            TokenKind::Neq => Self::Neq,
            TokenKind::Lt => Self::Lt,
            TokenKind::Leq => Self::Lte,
            TokenKind::Gt => Self::Gt,
            TokenKind::Geq => Self::Gte,
            _ => unreachable!(),
        }
    }
}

impl ToString for BinaryOp {
    fn to_string(&self) -> String {
        match self {
            Self::Add => "__add__",
            Self::Sub => "__sub__",
            Self::Mul => "__mul__",
            Self::Div => "__div__",
            Self::Rem => "__rem__",
            Self::Pow => "__pow__",
            Self::Eq => "__eq__",
            Self::Neq => "__neq__",
            Self::Lt => "__lt__",
            Self::Lte => "__lte__",
            Self::Gt => "__gt__",
            Self::Gte => "__gte__",
        }
        .to_string()
    }
}

impl From<BinaryOp> for InternedString {
    fn from(value: BinaryOp) -> Self {
        InternedString::from(value.to_string())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct PatId(pub usize);

#[derive(Debug, Clone, PartialEq)]
pub enum Pat {
    Wildcard,
    Lit(Lit),
    Ident(Ident, Option<AnnId>),
    List(Vec<PatId>),
    Con(Ident, Vec<PatId>),
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AnnId(pub usize);

#[derive(Debug, Clone, PartialEq)]
pub enum Ann {
    Byte,
    Int,
    Rational,
    Real,
    Bool,
    String,
    Char,
    Ident(Ident),
    Fn(AnnId, AnnId),
    List(AnnId),
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    pub name: InternedString,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Lit {
    Int,
    Real,
    String,
    Char,
}
