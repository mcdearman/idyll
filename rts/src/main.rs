pub mod vm;

use iced::theme;
use iced::widget::{
    column, container, horizontal_rule, row, scrollable, text, text_editor, text_input, Space,
};
use iced::Renderer;
use iced::{
    mouse, Alignment, Application, Color, Command, Element, Length, Pixels, Point, Rectangle,
    Settings, Theme,
};

use iced::widget::canvas;
use iced::widget::canvas::{Canvas, Fill, Frame, Path, Stroke};

use std::cell::RefCell;
use std::rc::Rc;

/* ========================= App ========================= */

pub fn main() -> iced::Result {
    Workbench::run(Settings {
        antialiasing: true,
        ..Settings::default()
    })
}

struct Workbench {
    source: text_editor::Content,
    cst: Vec<Token>,
    ast: Result<Expr, Err>,
    logs: Vec<String>,
    node_view: NodeEditor,

    // edit UI state
    edit_path: Option<Vec<usize>>, // path of selected node
    edit_kind: Option<EditKind>,
    edit_text: String, // input field for Var/Int
}

impl Default for Workbench {
    fn default() -> Self {
        Self {
            source: text_editor::Content::with_text(
                "let fact n = if n == 0 then 1 else n (fact (n - 1)) in fact 5\n",
            ),
            cst: Vec::new(),
            ast: Err(Err {
                msg: "empty".into(),
                at: 0,
            }),
            logs: Vec::new(),
            node_view: NodeEditor::default(),
            edit_path: None,
            edit_kind: None,
            edit_text: String::new(),
        }
    }
}

#[derive(Debug, Clone)]
enum Msg {
    Editor(text_editor::Action),
    NodeEditor(NodeEvent),

    // Edit panel
    BeginEdit,               // start editing currently selected
    EditTextChanged(String), // text_input change
    ApplyEdit,               // apply edit
    CancelEdit,              // cancel
}

/* ========================= Editable kinds ========================= */

#[derive(Clone, Debug)]
enum EditKind {
    RenameVar, // uses edit_text (identifier)
    SetInt,    // uses edit_text (i64)
    ToggleOp,  // toggles between == and -
}

/* ========================= Application impl ========================= */

impl Application for Workbench {
    type Executor = iced::executor::Default;
    type Message = Msg;
    type Theme = Theme;
    type Flags = ();

    fn new(_flags: ()) -> (Self, Command<Self::Message>) {
        let mut app = Workbench::default();
        app.reparse_now();
        (app, Command::none())
    }

    fn title(&self) -> String {
        "Live Parser + Editable Node Editor (iced 0.12)".into()
    }

    fn theme(&self) -> Theme {
        Theme::Dark
    }

    fn update(&mut self, msg: Msg) -> Command<Msg> {
        match msg {
            Msg::Editor(action) => {
                self.source.perform(action);
                self.reparse_now();
            }
            Msg::NodeEditor(evt) => {
                self.node_view.handle(evt);
                // Reflect selection into edit panel (but don't start editing yet)
                if let Some(sel) = self.node_view.selected_path() {
                    self.edit_path = Some(sel.clone());
                    // infer edit kind from AST node at path
                    self.edit_kind = self.infer_edit_kind_at_path(&sel);
                    // preload edit_text for Var/Int
                    self.edit_text = self.read_display_value_at_path(&sel);
                } else {
                    self.edit_path = None;
                    self.edit_kind = None;
                    self.edit_text.clear();
                }
            }

            Msg::BeginEdit => {
                // nothing special; UI already loaded edit_kind/text from selection
            }
            Msg::EditTextChanged(s) => {
                self.edit_text = s;
            }
            Msg::ApplyEdit => {
                if let (Ok(ast), Some(path), Some(kind)) = (
                    &mut self.ast,
                    self.edit_path.clone(),
                    self.edit_kind.clone(),
                ) {
                    // clone current AST, mutate, pretty-print, put back into editor & reparse
                    let mut ast2 = ast.clone();
                    if apply_edit_at_path(&mut ast2, &path, kind, &self.edit_text) {
                        let new_src = pretty_print(&ast2);
                        self.source = text_editor::Content::with_text(&new_src);
                        self.reparse_now();
                    } else {
                        self.logs
                            .push("Edit could not be applied at this node".into());
                    }
                }
            }
            Msg::CancelEdit => {
                self.edit_text = String::new();
                self.edit_kind = None;
                // keep selection
            }
        }
        Command::none()
    }

    fn view(&self) -> Element<Msg> {
        // Left: editor + logs
        let editor = text_editor(&self.source)
            .on_action(Msg::Editor)
            .height(Length::Fixed(240.0));

        let left = column![
            text("Source").size(18),
            editor,
            horizontal_rule(1),
            text("Logs").size(18),
            scrollable(
                self.logs
                    .iter()
                    .fold(column![], |col, line| col.push(text(line)))
            )
            .height(Length::Fill),
        ]
        .spacing(8)
        .padding(8)
        .width(Length::Fixed(460.0));

        // Right: tokens + node editor + edit panel
        let cst_lines = render_tokens(&self.cst);
        let cst_pane = container(
            scrollable(cst_lines.into_iter().fold(column![], |col, l| {
                col.push(text(l).font(iced::Font::MONOSPACE))
            }))
            .height(Length::Fixed(200.0)),
        )
        .padding(8)
        .style(theme::Container::Box)
        .width(Length::Fill);

        let node_canvas: Canvas<NodeEditor, Msg> = Canvas::new(self.node_view.clone())
            .width(Length::Fill)
            .height(Length::Fill);

        let edit_panel = self.render_edit_panel();

        let right = column![
            text("CST (tokens)").size(18),
            cst_pane,
            horizontal_rule(1),
            text("AST Node Editor (drag / select, then edit below)").size(18),
            container(node_canvas)
                .padding(8)
                .style(theme::Container::Box)
                .width(Length::Fill)
                .height(Length::Fixed(360.0)),
            horizontal_rule(1),
            edit_panel,
        ]
        .padding(8)
        .height(Length::Fill);

        container(
            column![
                row![
                    text("Tiny Haskell-ish parser + editable node editor").size(22),
                    Space::with_width(Length::Fill)
                ]
                .align_items(Alignment::Center),
                horizontal_rule(1),
                row![left, right].spacing(8)
            ]
            .spacing(6)
            .padding(8),
        )
        .into()
    }
}

/* ========================= Reparse ========================= */

impl Workbench {
    fn reparse_now(&mut self) {
        self.logs.clear();
        let src = self.source.text().to_string();
        let (tokens, lex_log) = lex(&src);
        self.logs.extend(lex_log);
        let ast = parse(&tokens);
        self.cst = tokens;
        self.ast = ast;

        // rebuild node graph with paths
        match &self.ast {
            Ok(expr) => {
                let graph = Graph::from_ast(expr);
                self.node_view.set_graph(graph);
            }
            Err(e) => {
                let mut g = Graph::default();
                g.nodes.push(VisNode {
                    id: "error".into(),
                    label: format!("parse error @{}: {}", e.at, e.msg),
                    pos: (40.0, 40.0),
                    size: (240.0, 48.0),
                    path: vec![],
                });
                self.node_view.set_graph(g);
            }
        }
    }

    fn infer_edit_kind_at_path(&self, path: &[usize]) -> Option<EditKind> {
        if let Ok(ast) = &self.ast {
            match get_node_at_path(ast, path) {
                Some(Expr::Var(_)) => Some(EditKind::RenameVar),
                Some(Expr::Int(_)) => Some(EditKind::SetInt),
                Some(Expr::Bin(_, _, _)) => Some(EditKind::ToggleOp),
                _ => None,
            }
        } else {
            None
        }
    }

    fn read_display_value_at_path(&self, path: &[usize]) -> String {
        if let Ok(ast) = &self.ast {
            match get_node_at_path(ast, path) {
                Some(Expr::Var(s)) => s.clone(),
                Some(Expr::Int(n)) => n.to_string(),
                Some(Expr::Bin(_, op, _)) => match op {
                    Op::EqEq => "==".into(),
                    Op::Minus => "-".into(),
                },
                _ => String::new(),
            }
        } else {
            String::new()
        }
    }

    fn render_edit_panel(&self) -> Element<Msg> {
        let mut panel = column![text("Selected node").size(18)].spacing(6);

        match (&self.edit_path, &self.edit_kind) {
            (Some(_path), Some(EditKind::RenameVar)) => {
                panel = panel
                    .push(text("Kind: Var (rename)"))
                    .push(text_input("identifier", &self.edit_text).on_input(Msg::EditTextChanged))
                    .push(
                        row![
                            iced::widget::button("Apply").on_press(Msg::ApplyEdit),
                            iced::widget::button("Cancel").on_press(Msg::CancelEdit)
                        ]
                        .spacing(8),
                    );
            }
            (Some(_path), Some(EditKind::SetInt)) => {
                panel = panel
                    .push(text("Kind: Int (set value)"))
                    .push(text_input("integer", &self.edit_text).on_input(Msg::EditTextChanged))
                    .push(
                        row![
                            iced::widget::button("Apply").on_press(Msg::ApplyEdit),
                            iced::widget::button("Cancel").on_press(Msg::CancelEdit)
                        ]
                        .spacing(8),
                    );
            }
            (Some(_path), Some(EditKind::ToggleOp)) => {
                panel = panel.push(text("Kind: Binary Op (toggle == / -)")).push(
                    row![
                        iced::widget::button("Toggle & Apply").on_press(Msg::ApplyEdit),
                        iced::widget::button("Cancel").on_press(Msg::CancelEdit)
                    ]
                    .spacing(8),
                );
            }
            _ => {
                panel = panel.push(text("Select a Var, Int, or Bin node to edit."));
            }
        }

        container(panel)
            .padding(8)
            .style(theme::Container::Box)
            .width(Length::Fill)
            .into()
    }
}

/* ========================= Lexer ========================= */

#[derive(Clone, Debug, PartialEq)]
enum Token {
    Ident(String),
    Int(i64),
    KwLet,
    KwIn,
    KwIf,
    KwThen,
    KwElse,
    Lambda, // '\'
    Arrow,  // '->'
    Eq,     // '='
    EqEq,   // '=='
    Minus,  // '-'
    LParen,
    RParen,
}

#[derive(Clone, Debug)]
struct Err {
    msg: String,
    at: usize, // token index
}

fn lex(src: &str) -> (Vec<Token>, Vec<String>) {
    use Token::*;
    let mut out = Vec::new();
    let mut logs = Vec::new();
    let mut i = 0usize;
    let b = src.as_bytes();
    while i < b.len() {
        let c = b[i] as char;
        if c.is_whitespace() {
            i += 1;
            continue;
        }
        match c {
            '(' => {
                out.push(LParen);
                i += 1;
            }
            ')' => {
                out.push(RParen);
                i += 1;
            }
            '=' => {
                if i + 1 < b.len() && b[i + 1] as char == '=' {
                    out.push(EqEq);
                    i += 2;
                } else {
                    out.push(Eq);
                    i += 1;
                }
            }
            '\\' | 'λ' => {
                out.push(Lambda);
                i += 1;
            }
            '-' => {
                if i + 1 < b.len() && b[i + 1] as char == '>' {
                    out.push(Arrow);
                    i += 2;
                } else {
                    out.push(Minus);
                    i += 1;
                }
            }
            _ => {
                if c.is_ascii_digit() {
                    let (n, j) = read_int(src, i);
                    out.push(Int(n));
                    i = j;
                } else if is_ident_start(c) {
                    let (id, j) = read_ident(src, i);
                    match id.as_str() {
                        "let" => out.push(KwLet),
                        "in" => out.push(KwIn),
                        "if" => out.push(KwIf),
                        "then" => out.push(KwThen),
                        "else" => out.push(KwElse),
                        _ => out.push(Ident(id)),
                    }
                    i = j;
                } else {
                    logs.push(format!("Skipping unrecognized char `{}` at byte {}", c, i));
                    i += 1;
                }
            }
        }
    }
    (out, logs)
}

fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}
fn is_ident_continue(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

fn read_ident(src: &str, mut i: usize) -> (String, usize) {
    let mut s = String::new();
    for ch in src[i..].chars() {
        if s.is_empty() {
            if !is_ident_start(ch) {
                break;
            }
        } else if !is_ident_continue(ch) {
            break;
        }
        s.push(ch);
        i += ch.len_utf8();
    }
    (s, i)
}
fn read_int(src: &str, mut i: usize) -> (i64, usize) {
    let mut s = String::new();
    for ch in src[i..].chars() {
        if ch.is_ascii_digit() {
            s.push(ch);
            i += 1;
        } else {
            break;
        }
    }
    (s.parse().unwrap_or(0), i)
}

/* ========================= AST + Parser ========================= */

#[derive(Clone, Debug)]
enum Expr {
    Var(String),
    Int(i64),
    App(Box<Expr>, Box<Expr>),
    Lam(Vec<String>, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Bin(Box<Expr>, Op, Box<Expr>),
}

#[derive(Clone, Debug)]
enum Op {
    EqEq,
    Minus,
}

fn parse(tokens: &[Token]) -> Result<Expr, Err> {
    let mut p = Parser { toks: tokens, i: 0 };
    let e = p.parse_expr()?;
    if p.i != p.toks.len() {
        Err(Err {
            msg: "trailing tokens".into(),
            at: p.i,
        })
    } else {
        Ok(e)
    }
}

struct Parser<'a> {
    toks: &'a [Token],
    i: usize,
}

impl<'a> Parser<'a> {
    fn peek(&self) -> Option<&'a Token> {
        self.toks.get(self.i)
    }
    fn bump(&mut self) -> Option<&'a Token> {
        let t = self.toks.get(self.i);
        if t.is_some() {
            self.i += 1;
        }
        t
    }
    fn expect(&mut self, want: &Token) -> Result<(), Err> {
        match self.bump() {
            Some(t) if std::mem::discriminant(t) == std::mem::discriminant(want) => Ok(()),
            _ => Err(Err {
                msg: format!("expected {}", want_kind(want)),
                at: self.i,
            }),
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, Err> {
        self.parse_prec(0)
    }

    fn starts_atom(t: &Token) -> bool {
        matches!(
            t,
            Token::Ident(_)
                | Token::Int(_)
                | Token::LParen
                | Token::Lambda
                | Token::KwIf
                | Token::KwLet
        )
    }

    fn parse_primary(&mut self) -> Result<Expr, Err> {
        match self.peek() {
            Some(Token::KwLet) => return self.parse_let(),
            Some(Token::Lambda) => return self.parse_lambda(),
            Some(Token::KwIf) => return self.parse_if(),
            _ => {}
        }
        match self.bump() {
            Some(Token::Ident(s)) => Ok(Expr::Var(s.clone())),
            Some(Token::Int(n)) => Ok(Expr::Int(*n)),
            Some(Token::LParen) => {
                let e = self.parse_expr()?;
                self.expect(&Token::RParen)?;
                Ok(e)
            }
            other => Err(Err {
                msg: format!("expected atom, got {}", map_tok(other)),
                at: self.i,
            }),
        }
    }

    fn parse_with_app(&mut self) -> Result<Expr, Err> {
        let mut head = self.parse_primary()?;
        loop {
            let next = self.peek().cloned();
            match next {
                Some(t) if Self::starts_atom(&t) => {
                    let arg = self.parse_primary()?;
                    head = Expr::App(Box::new(head), Box::new(arg));
                }
                _ => break,
            }
        }
        Ok(head)
    }

    fn op_prec(tok: &Token) -> Option<(u8, Op)> {
        match tok {
            Token::EqEq => Some((40, Op::EqEq)),
            Token::Minus => Some((60, Op::Minus)),
            _ => None,
        }
    }

    fn parse_prec(&mut self, min_prec: u8) -> Result<Expr, Err> {
        let mut left = self.parse_with_app()?;
        loop {
            let (prec, op) = match self.peek().and_then(Self::op_prec) {
                Some(x) => x,
                None => break,
            };
            if prec < min_prec {
                break;
            }
            self.bump();
            let right = self.parse_prec(prec + 1)?;
            left = Expr::Bin(Box::new(left), op, Box::new(right));
        }
        Ok(left)
    }

    fn parse_lambda(&mut self) -> Result<Expr, Err> {
        self.expect(&Token::Lambda)?;
        let mut params = Vec::new();
        loop {
            match self.peek() {
                Some(Token::Ident(name)) => {
                    params.push(name.clone());
                    self.bump();
                }
                Some(Token::Arrow) => {
                    self.bump();
                    break;
                }
                _ => {
                    return Err(Err {
                        msg: "expected parameter or '->'".into(),
                        at: self.i,
                    })
                }
            }
        }
        let body = self.parse_expr()?;
        Ok(Expr::Lam(params, Box::new(body)))
    }

    fn parse_let(&mut self) -> Result<Expr, Err> {
        self.expect(&Token::KwLet)?;
        let name = match self.bump() {
            Some(Token::Ident(s)) => s.clone(),
            _ => {
                return Err(Err {
                    msg: "expected identifier after 'let'".into(),
                    at: self.i,
                })
            }
        };
        let mut params = Vec::new();
        loop {
            match self.peek() {
                Some(Token::Ident(p)) => {
                    params.push(p.clone());
                    self.bump();
                }
                Some(Token::Eq) => break,
                _ => break,
            }
        }
        self.expect(&Token::Eq)?;
        let mut rhs = self.parse_expr()?;
        if !params.is_empty() {
            rhs = Expr::Lam(params, Box::new(rhs));
        }
        match self.bump() {
            Some(Token::KwIn) => {
                let body = self.parse_expr()?;
                Ok(Expr::Let(name, Box::new(rhs), Box::new(body)))
            }
            _ => Err(Err {
                msg: "expected 'in' after let binding".into(),
                at: self.i,
            }),
        }
    }

    fn parse_if(&mut self) -> Result<Expr, Err> {
        self.expect(&Token::KwIf)?;
        let c = self.parse_expr()?;
        match self.bump() {
            Some(Token::KwThen) => {}
            _ => {
                return Err(Err {
                    msg: "expected 'then'".into(),
                    at: self.i,
                })
            }
        }
        let t = self.parse_expr()?;
        match self.bump() {
            Some(Token::KwElse) => {}
            _ => {
                return Err(Err {
                    msg: "expected 'else'".into(),
                    at: self.i,
                })
            }
        }
        let e = self.parse_expr()?;
        Ok(Expr::If(Box::new(c), Box::new(t), Box::new(e)))
    }
}

/* ========================= Pretty-print ========================= */

fn pretty_print(e: &Expr) -> String {
    fn pp(e: &Expr, ctx_prec: u8) -> String {
        match e {
            Expr::Var(s) => s.clone(),
            Expr::Int(n) => n.to_string(),
            Expr::Lam(params, body) => {
                let s = format!("\\{} -> {}", params.join(" "), pp(body, 0));
                if ctx_prec > 0 {
                    format!("({s})")
                } else {
                    s
                }
            }
            Expr::Let(name, v, b) => {
                let s = format!("let {} = {} in {}", name, pp(v, 0), pp(b, 0));
                if ctx_prec > 0 {
                    format!("({s})")
                } else {
                    s
                }
            }
            Expr::If(c, t, e2) => {
                let s = format!("if {} then {} else {}", pp(c, 0), pp(t, 0), pp(e2, 0));
                if ctx_prec > 0 {
                    format!("({s})")
                } else {
                    s
                }
            }
            Expr::App(f, a) => {
                let s = format!("{} {}", pp(f, 70), pp(a, 71)); // app binds tighter than infix
                if ctx_prec > 70 {
                    format!("({s})")
                } else {
                    s
                }
            }
            Expr::Bin(l, op, r) => {
                let (prec, sym) = match op {
                    Op::Minus => (60u8, "-"),
                    Op::EqEq => (40u8, "=="),
                };
                let s = format!("{} {} {}", pp(l, prec), sym, pp(r, prec + 1));
                if ctx_prec > prec {
                    format!("({s})")
                } else {
                    s
                }
            }
        }
    }
    pp(e, 0) + "\n"
}

/* ========================= AST path utils + edits ========================= */

fn get_node_at_path<'a>(e: &'a Expr, path: &[usize]) -> Option<&'a Expr> {
    let mut cur = e;
    for &i in path {
        cur = match (cur, i) {
            (Expr::App(l, r), 0) => &*l,
            (Expr::App(l, r), 1) => &*r,
            (Expr::Lam(_, b), 0) => &*b,
            (Expr::Let(_, v, b), 0) => &*v,
            (Expr::Let(_, v, b), 1) => &*b,
            (Expr::If(c, t, e2), 0) => &*c,
            (Expr::If(c, t, e2), 1) => &*t,
            (Expr::If(c, t, e2), 2) => &*e2,
            (Expr::Bin(l, _, r), 0) => &*l,
            (Expr::Bin(l, _, r), 1) => &*r,
            _ => return None,
        };
    }
    Some(cur)
}

fn apply_edit_at_path(root: &mut Expr, path: &[usize], kind: EditKind, text: &str) -> bool {
    // recurse and rebuild in-place
    fn go(e: &mut Expr, path: &[usize], kind: EditKind, text: &str) -> bool {
        if path.is_empty() {
            match (e, kind) {
                (Expr::Var(ref mut s), EditKind::RenameVar) => {
                    if text.is_empty() || !is_ident_start(text.chars().next().unwrap_or('_')) {
                        return false;
                    }
                    *s = text.to_string();
                    true
                }
                (Expr::Int(ref mut n), EditKind::SetInt) => {
                    if let Ok(v) = text.parse::<i64>() {
                        *n = v;
                        true
                    } else {
                        false
                    }
                }
                (Expr::Bin(_, ref mut op, _), EditKind::ToggleOp) => {
                    *op = match op {
                        Op::EqEq => Op::Minus,
                        Op::Minus => Op::EqEq,
                    };
                    true
                }
                _ => false,
            }
        } else {
            let i = path[0];
            match e {
                Expr::App(l, r) => {
                    if i == 0 {
                        go(l, &path[1..], kind, text)
                    } else if i == 1 {
                        go(r, &path[1..], kind, text)
                    } else {
                        false
                    }
                }
                Expr::Lam(_, b) => {
                    if i == 0 {
                        go(b, &path[1..], kind, text)
                    } else {
                        false
                    }
                }
                Expr::Let(_, v, b) => {
                    if i == 0 {
                        go(v, &path[1..], kind, text)
                    } else if i == 1 {
                        go(b, &path[1..], kind, text)
                    } else {
                        false
                    }
                }
                Expr::If(c, t, e2) => {
                    if i == 0 {
                        go(c, &path[1..], kind, text)
                    } else if i == 1 {
                        go(t, &path[1..], kind, text)
                    } else if i == 2 {
                        go(e2, &path[1..], kind, text)
                    } else {
                        false
                    }
                }
                Expr::Bin(l, _, r) => {
                    if i == 0 {
                        go(l, &path[1..], kind, text)
                    } else if i == 1 {
                        go(r, &path[1..], kind, text)
                    } else {
                        false
                    }
                }
                _ => false,
            }
        }
    }
    go(root, path, kind, text)
}

/* ========================= Render helpers ========================= */

fn render_tokens(ts: &[Token]) -> Vec<String> {
    ts.iter()
        .enumerate()
        .map(|(i, t)| format!("{:>3}: {}", i, show_tok(t)))
        .collect()
}
fn show_tok(t: &Token) -> String {
    use Token::*;
    match t {
        Ident(s) => format!("Ident({s})"),
        Int(n) => format!("Int({n})"),
        KwLet => "Kw(let)".into(),
        KwIn => "Kw(in)".into(),
        KwIf => "Kw(if)".into(),
        KwThen => "Kw(then)".into(),
        KwElse => "Kw(else)".into(),
        Lambda => "Lambda(\\)".into(),
        Arrow => "Arrow(->)".into(),
        Eq => "Eq(=)".into(),
        EqEq => "EqEq(==)".into(),
        Minus => "Minus(-)".into(),
        LParen => "LParen".into(),
        RParen => "RParen".into(),
    }
}
fn want_kind(t: &Token) -> &'static str {
    use Token::*;
    match t {
        Ident(_) => "identifier",
        Int(_) => "int",
        KwLet => "let",
        KwIn => "in",
        KwIf => "if",
        KwThen => "then",
        KwElse => "else",
        Lambda => "\\",
        Arrow => "->",
        Eq => "=",
        EqEq => "==",
        Minus => "-",
        LParen => "(",
        RParen => ")",
    }
}
fn map_tok(t: Option<&Token>) -> &'static str {
    match t {
        None => "EOF",
        Some(Token::Ident(_)) => "ident",
        Some(Token::Int(_)) => "int",
        Some(Token::KwLet) => "let",
        Some(Token::KwIn) => "in",
        Some(Token::KwIf) => "if",
        Some(Token::KwThen) => "then",
        Some(Token::KwElse) => "else",
        Some(Token::Lambda) => "\\",
        Some(Token::Arrow) => "->",
        Some(Token::Eq) => "=",
        Some(Token::EqEq) => "==",
        Some(Token::Minus) => "-",
        Some(Token::LParen) => "(",
        Some(Token::RParen) => ")",
    }
}

/* ========================= Graph from AST (with paths) ========================= */

#[derive(Default, Clone)]
struct Graph {
    nodes: Vec<VisNode>,
    edges: Vec<(usize, usize)>,
}

#[derive(Clone)]
struct VisNode {
    id: String,
    label: String,
    pos: (f32, f32),
    size: (f32, f32),
    path: Vec<usize>, // <— path from root (used for edits)
}

impl Graph {
    fn from_ast(ast: &Expr) -> Self {
        let mut g = Graph::default();
        let mut y = 20.0;
        let x_step = 180.0;
        let y_step = 90.0;

        fn add_expr(
            g: &mut Graph,
            e: &Expr,
            path: &mut Vec<usize>,
            depth: usize,
            y_cursor: &mut f32,
            x_step: f32,
            y_step: f32,
        ) -> usize {
            let id = path
                .iter()
                .map(|i| i.to_string())
                .collect::<Vec<_>>()
                .join(".");

            let label = match e {
                Expr::Var(s) => format!("Var {s}"),
                Expr::Int(n) => format!("Int {n}"),
                Expr::App(_, _) => "App".into(),
                Expr::Lam(ps, _) => format!("Lam {}", ps.join(" ")),
                Expr::Let(name, _, _) => format!("Let {name}"),
                Expr::If(_, _, _) => "If".into(),
                Expr::Bin(_, op, _) => match op {
                    Op::EqEq => "Bin ==".into(),
                    Op::Minus => "Bin -".into(),
                },
            };
            let idx = g.nodes.len();
            g.nodes.push(VisNode {
                id,
                label,
                pos: (20.0 + depth as f32 * x_step, *y_cursor),
                size: (160.0, 44.0),
                path: path.clone(),
            });

            match e {
                Expr::App(f, a) => {
                    path.push(0);
                    let l = add_expr(g, f, path, depth + 1, y_cursor, x_step, y_step);
                    path.pop();
                    g.edges.push((idx, l));
                    path.push(1);
                    let r = add_expr(g, a, path, depth + 1, y_cursor, x_step, y_step);
                    path.pop();
                    g.edges.push((idx, r));
                }
                Expr::Lam(_, b) => {
                    path.push(0);
                    let bidx = add_expr(g, b, path, depth + 1, y_cursor, x_step, y_step);
                    path.pop();
                    g.edges.push((idx, bidx));
                }
                Expr::Let(_, v, b) => {
                    path.push(0);
                    let vdx = add_expr(g, v, path, depth + 1, y_cursor, x_step, y_step);
                    path.pop();
                    g.edges.push((idx, vdx));
                    path.push(1);
                    let bdx = add_expr(g, b, path, depth + 1, y_cursor, x_step, y_step);
                    path.pop();
                    g.edges.push((idx, bdx));
                }
                Expr::If(c, t, e2) => {
                    path.push(0);
                    let cdx = add_expr(g, c, path, depth + 1, y_cursor, x_step, y_step);
                    path.pop();
                    g.edges.push((idx, cdx));
                    path.push(1);
                    let tdx = add_expr(g, t, path, depth + 1, y_cursor, x_step, y_step);
                    path.pop();
                    g.edges.push((idx, tdx));
                    path.push(2);
                    let edx = add_expr(g, e2, path, depth + 1, y_cursor, x_step, y_step);
                    path.pop();
                    g.edges.push((idx, edx));
                }
                Expr::Bin(l, _, r) => {
                    path.push(0);
                    let li = add_expr(g, l, path, depth + 1, y_cursor, x_step, y_step);
                    path.pop();
                    g.edges.push((idx, li));
                    path.push(1);
                    let ri = add_expr(g, r, path, depth + 1, y_cursor, x_step, y_step);
                    path.pop();
                    g.edges.push((idx, ri));
                }
                _ => {}
            }

            *y_cursor += y_step;
            idx
        }

        let mut path = vec![];
        add_expr(&mut g, ast, &mut path, 0, &mut y, x_step, y_step);
        g
    }
}

/* ========================= Node Editor (Canvas) ========================= */

#[derive(Debug, Clone)]
enum NodeEvent {
    MouseDown(Point),
    MouseUp,
    MouseMove(Point),
}

#[derive(Clone, Default)]
struct NodeEditor {
    state: Rc<RefCell<NodeState>>,
}

#[derive(Default)]
struct NodeState {
    graph: Graph,
    selected: Option<usize>,
    dragging: bool,
    drag_offset: (f32, f32), // cursor - node.topleft
}

impl NodeEditor {
    fn set_graph(&mut self, g: Graph) {
        let mut st = self.state.borrow_mut();
        st.graph = g;
        st.selected = None;
        st.dragging = false;
    }

    fn handle(&mut self, evt: NodeEvent) {
        let mut st = self.state.borrow_mut();
        match evt {
            NodeEvent::MouseDown(p) => {
                let mut hit: Option<usize> = None;
                for (i, n) in st.graph.nodes.iter().enumerate().rev() {
                    let rect = node_rect(n);
                    if contains_point(rect, p) {
                        hit = Some(i);
                        break;
                    }
                }
                st.selected = hit;
                if let Some(i) = hit {
                    st.dragging = true;
                    let n = &st.graph.nodes[i];
                    st.drag_offset = (p.x - n.pos.0, p.y - n.pos.1);
                }
            }
            NodeEvent::MouseUp => {
                st.dragging = false;
            }
            NodeEvent::MouseMove(p) => {
                if st.dragging {
                    if let Some(i) = st.selected {
                        let (dx, dy) = st.drag_offset;
                        let n = &mut st.graph.nodes[i];
                        n.pos = (p.x - dx, p.y - dy);
                    }
                }
            }
        }
    }

    fn selected_path(&self) -> Option<Vec<usize>> {
        let st = self.state.borrow();
        st.selected
            .and_then(|i| st.graph.nodes.get(i).map(|n| n.path.clone()))
    }
}

impl canvas::Program<Msg> for NodeEditor {
    type State = ();

    fn draw(
        &self,
        _state: &Self::State,
        renderer: &Renderer,
        _theme: &Theme,
        bounds: Rectangle,
        _cursor: mouse::Cursor,
    ) -> Vec<canvas::Geometry> {
        let mut frame = Frame::new(renderer, bounds.size());

        let st = self.state.borrow();

        // edges
        for &(p, c) in &st.graph.edges {
            let a = &st.graph.nodes[p];
            let b = &st.graph.nodes[c];
            let a_center = Point::new(a.pos.0 + a.size.0, a.pos.1 + a.size.1 * 0.5);
            let b_center = Point::new(b.pos.0, b.pos.1 + b.size.1 * 0.5);
            let path = Path::line(a_center, b_center);
            frame.stroke(
                &path,
                Stroke {
                    style: canvas::Style::Solid(Color::from_rgb(0.6, 0.6, 0.9)),
                    width: 1.5,
                    ..Stroke::default()
                },
            );
        }

        // nodes
        for (i, n) in st.graph.nodes.iter().enumerate() {
            let rect = node_rect(n);
            let is_sel = st.selected == Some(i);

            let bg = if is_sel {
                Color::from_rgba(0.20, 0.35, 0.55, 0.95)
            } else {
                Color::from_rgba(0.15, 0.18, 0.22, 0.95)
            };
            let fg = if is_sel {
                Color::from_rgb(0.95, 0.98, 1.0)
            } else {
                Color::from_rgb(0.85, 0.90, 0.95)
            };
            let border = if is_sel {
                Color::from_rgb(0.40, 0.70, 1.0)
            } else {
                Color::from_rgb(0.35, 0.40, 0.48)
            };

            let path = Path::rectangle(rect.position(), rect.size());
            frame.fill(&path, Fill::from(bg));
            frame.stroke(
                &path,
                Stroke {
                    style: canvas::Style::Solid(border),
                    width: 1.0,
                    ..Stroke::default()
                },
            );

            let text_pos = Point::new(n.pos.0 + 10.0, n.pos.1 + 16.0);
            frame.fill_text(canvas::Text {
                content: n.label.clone(),
                position: text_pos,
                size: Pixels(16.0),
                color: fg,
                ..Default::default()
            });
        }

        vec![frame.into_geometry()]
    }

    fn update(
        &self,
        _state: &mut Self::State,
        event: canvas::Event,
        bounds: Rectangle,
        cursor: mouse::Cursor,
    ) -> (canvas::event::Status, Option<Msg>) {
        match event {
            canvas::Event::Mouse(me) => {
                if let Some(pos) = cursor.position_in(bounds) {
                    use iced::mouse::Event as ME;
                    match me {
                        ME::ButtonPressed(iced::mouse::Button::Left) => {
                            return (
                                canvas::event::Status::Captured,
                                Some(Msg::NodeEditor(NodeEvent::MouseDown(pos))),
                            );
                        }
                        ME::ButtonReleased(iced::mouse::Button::Left) => {
                            return (
                                canvas::event::Status::Captured,
                                Some(Msg::NodeEditor(NodeEvent::MouseUp)),
                            );
                        }
                        ME::CursorMoved { .. } => {
                            return (
                                canvas::event::Status::Captured,
                                Some(Msg::NodeEditor(NodeEvent::MouseMove(pos))),
                            );
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
        (canvas::event::Status::Ignored, None)
    }
}

/* ========================= Node editor utils ========================= */

fn node_rect(n: &VisNode) -> Rectangle {
    Rectangle::new(
        Point::new(n.pos.0, n.pos.1),
        iced::Size::new(n.size.0, n.size.1),
    )
}
fn contains_point(rect: Rectangle, p: Point) -> bool {
    p.x >= rect.x && p.x <= rect.x + rect.width && p.y >= rect.y && p.y <= rect.y + rect.height
}

/* ========================= END ========================= */
