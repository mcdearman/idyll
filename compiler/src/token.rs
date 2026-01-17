use crate::utils::{InternedString, Located};
use logos::Logos;
use std::fmt::Display;

pub type Token = Located<TokenKind>;

#[derive(Logos, Debug, Clone, PartialEq)]
pub enum TokenKind {
    Eof,
    #[regex(r"--.*?")]
    Comment,
    #[regex(r"[ \t\n\r]+", logos::skip)]
    Whitespace,
    // Literals and identifiers
    #[regex(
        r"-?((0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0))", 
        |lex| lex.slice().parse().ok(),
        priority = 3
    )]
    Int(i64),
    #[regex(
        r"([0-9]*[.])?[0-9]+", 
        |lex| lex.slice().parse().ok(),
        priority = 2
    )]
    Real(f64),
    // #[regex(
    //     r"-?((0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0))(/-?((0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0)))",
    //     |lex| lex.slice().parse().ok())]
    // Rational(Rational64),
    #[regex(r#""(\\.|[^"\\])*""#, |lex| InternedString::from(lex.slice()))]
    String(InternedString),
    #[regex(r"'(\\.|[^'\\])'", |lex| lex.slice().chars().nth(1))]
    Char(char),
    #[regex(r"[a-z][a-zA-Z0-9'_]*", |lex| InternedString::from(lex.slice()), priority = 2)]
    LowerIdent(InternedString),
    #[regex(r"[A-Z][a-zA-Z0-9']*", |lex| InternedString::from(lex.slice()))]
    UpperIdent(InternedString),
    // #[regex(r"[!$%&*+./<=>?@\|\\\^-z~:]+", |lex| InternedString::from(lex.slice()))]
    // OpIdent(InternedString),
    // #[regex(r":[!$%&*+./<=>?@\|\\\^-z~:]+", |lex| InternedString::from(lex.slice()))]
    // ConOpIdent(InternedString),

    // Punctuation
    #[token("_")]
    Wildcard,
    #[token("\\")]
    Backslash,
    #[token("<-")]
    LArrow,
    #[token("->")]
    RArrow,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("^")]
    Caret,
    #[token("or")]
    Or,
    #[token("and")]
    And,
    #[token("=")]
    Eq,
    #[token("!=")]
    Neq,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("<=")]
    Leq,
    #[token(">=")]
    Geq,
    #[token("!")]
    Bang,
    #[token(",")]
    Comma,
    #[token(".")]
    Period,
    #[token("..")]
    DoublePeriod,
    #[token("..=")]
    DoublePeriodEq,
    #[token(":")]
    Colon,
    #[token(";")]
    SemiColon,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBrack,
    #[token("]")]
    RBrack,
    #[token("#")]
    Hash,
    #[token("|")]
    Bar,
    #[token("<|")]
    LPipe,
    #[token("|>")]
    RPipe,
    #[token("@")]
    At,
    #[token("`")]
    Backtick,

    // Keywords
    #[token("end")]
    End,
    #[token("use")]
    Use,
    #[token("fun")]
    Fun,
    #[token("def")]
    Def,
    #[token("let")]
    Let,
    #[token("in")]
    In,
    #[token("match")]
    Match,
    #[token("with")]
    With,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,
    #[token("data")]
    Data,
    #[token("type")]
    Type,
    #[token("as")]
    As,
    #[token("ref")]
    Ref,
    Error,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenKind::*;
        match self {
            Eof => write!(f, "Eof"),
            Comment => write!(f, "Comment"),
            Whitespace => write!(f, "Whitespace"),
            Int(i) => write!(f, "Int({})", i),
            Real(r) => write!(f, "Real({})", r),
            // Rational(r) => write!(f, "Rational({})", r),
            String(s) => write!(f, "String({})", s),
            Char(c) => write!(f, "Char({})", c),
            LowerIdent(s) => write!(f, "LowerIdent({})", s),
            UpperIdent(s) => write!(f, "UpperIdent({})", s),
            // OpIdent(s) => write!(f, "OpIdent({})", s),
            // ConOpIdent(s) => write!(f, "ConOpIdent({})", s),
            Wildcard => write!(f, "Wildcard"),
            Backslash => write!(f, "Backslash"),
            LArrow => write!(f, "LArrow"),
            RArrow => write!(f, "RArrow"),
            Plus => write!(f, "Plus"),
            Minus => write!(f, "Minus"),
            Star => write!(f, "Star"),
            Slash => write!(f, "Slash"),
            Percent => write!(f, "Percent"),
            Caret => write!(f, "Caret"),
            Or => write!(f, "Or"),
            And => write!(f, "And"),
            Eq => write!(f, "Eq"),
            Neq => write!(f, "Neq"),
            Lt => write!(f, "Lt"),
            Gt => write!(f, "Gt"),
            Leq => write!(f, "Leq"),
            Geq => write!(f, "Geq"),
            Bang => write!(f, "Bang"),
            Comma => write!(f, "Comma"),
            Period => write!(f, "Period"),
            DoublePeriod => write!(f, "DoublePeriod"),
            DoublePeriodEq => write!(f, "DoublePeriodEq"),
            Colon => write!(f, "Colon"),
            SemiColon => write!(f, "SemiColon"),
            LParen => write!(f, "LParen"),
            RParen => write!(f, "RParen"),
            LBrace => write!(f, "LBrace"),
            RBrace => write!(f, "RBrace"),
            LBrack => write!(f, "LBrack"),
            RBrack => write!(f, "RBrack"),
            Hash => write!(f, "HashLBrack"),
            Bar => write!(f, "Bar"),
            LPipe => write!(f, "LPipe"),
            RPipe => write!(f, "RPipe"),
            At => write!(f, "At"),
            Backtick => write!(f, "Backtick"),

            End => write!(f, "End"),
            Use => write!(f, "Use"),
            Fun => write!(f, "Fun"),
            Def => write!(f, "Def"),
            Let => write!(f, "Let"),
            In => write!(f, "In"),
            If => write!(f, "If"),
            Match => write!(f, "Match"),
            With => write!(f, "With"),
            Then => write!(f, "Then"),
            Else => write!(f, "Else"),
            Data => write!(f, "Data"),
            Type => write!(f, "Type"),
            As => write!(f, "As"),
            Ref => write!(f, "Ref"),
            Error => write!(f, "Error"),
        }
    }
}
