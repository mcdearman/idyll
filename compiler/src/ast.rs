use crate::utils::{InternedString, Located};

type Prog = Located<Package>;

#[derive(Debug, Clone, PartialEq)]
pub struct Package {
    pub name: InternedString,
    pub modules: Vec<LModule>,
}

type LModule = Located<Module>;

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name: InternedString,
    pub decls: Vec<LDecl>,
}

type LDecl = Located<Decl>;

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Def(Def),
    Fun(Fun),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Def {
    pub name: Ident,
    pub params: Vec<Pat>,
    pub body: LExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Fun {
    pub name: Ident,
    pub params: Vec<LPat>,
    pub body: LExpr,
}

type LExpr = Located<Expr>;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Lit(Lit),
    Ident(Ident),
    App(Box<LExpr>, Vec<LExpr>),
    Lambda(Vec<LPat>, Box<LExpr>),
    Let(LPat, Box<LExpr>),
    If(Box<LExpr>, Box<LExpr>, Box<LExpr>),
    Match(Box<LExpr>, Vec<(LPat, LExpr)>),
    Prefix(PrefixOp, Box<LExpr>),
    Infix(Box<LExpr>, InfixOp, Box<LExpr>),
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrefixOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub enum InfixOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
}

type LPat = Located<Pat>;

#[derive(Debug, Clone, PartialEq)]
pub enum Pat {
    Wildcard,
    Lit(Lit),
    Ident(Ident),
    Tuple(Vec<Pat>),
    List(Vec<Pat>),
    Cons(Ident, Box<Pat>),
    Unit,
}

type Ident = Located<InternedString>;

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Int(i64),
    Real(f64),
    String(InternedString),
    Char(char),
}
