use crate::utils::{InternedString, Located};

type Prog = Located<Package>;

#[derive(Debug, Clone, PartialEq)]
pub struct Package {
    pub name: String,
    pub modules: Vec<Module>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name: String,
    pub decls: Vec<Decl>,
}

type Decl = Located<DeclKind>;

#[derive(Debug, Clone, PartialEq)]
pub enum DeclKind {
    Def(Def),
    Fun(Fun),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Def {
    pub name: Ident,
    pub params: Vec<Pat>,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Fun {
    pub name: Ident,
    pub params: Vec<Pat>,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Lit(Lit),
    Ident(Ident),
    App(Box<Expr>, Vec<Expr>),
    Lambda(Vec<Pat>, Box<Expr>),
    Let(Pat, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Match(Box<Expr>, Vec<(Pat, Expr)>),
    Unit,
}

type Pat = Located<PatKind>;

#[derive(Debug, Clone, PartialEq)]
pub enum PatKind {
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