use im::{HashMap, HashSet};
use sexp::Atom::*;
use sexp::*;

#[derive(Debug)]
pub enum Err {
    Overflow,
    Type,
}

#[derive(Debug)]
pub enum Val {
    Reg(Reg),
    Imm(i64),
    RegOffset(Reg, i32),
    True,
    False,
    Error(Err),
}

#[derive(Debug)]
pub enum Reg {
    RAX,
    RSP,
    RDI,
    RCX,
    RDX,
    RBP,
}

#[derive(Debug)]
pub enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    ISar(Val, Val),
    IAnd(Val, Val),
    ITest(Val, Val),
    ICmp(Val, Val),
    ILabel(String),
    IJmp(String),
    IJe(String),
    IJne(String),
    IJl(String),
    IJle(String),
    IJg(String),
    IJge(String),
    IJz(String),
    IJnz(String),
    IJo,
    ICall(String),
    IPush(Val),
}

#[derive(Debug)]
pub enum Op1 {
    Add1,
    Sub1,
    IsNum,
    IsBool,
}

#[derive(Debug)]
pub enum Op2 {
    Plus,
    Minus,
    Times,
    Equal,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

#[derive(Debug)]
pub enum Expr {
    Number(i64),
    Boolean(bool),
    Id(String),
    Input,
    UnOp(Op1, Box<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Set(String, Box<Expr>),
    Print(Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Let(Vec<(String, Expr)>, Box<Expr>),
    Block(Vec<Expr>),
    Call(String, Vec<Expr>),
}

#[derive(Debug)]
pub struct Prog {
    pub defs: Vec<Defn>,
    pub expr: Box<Expr>,
    pub fn_count: HashMap<String, i32>,
}

#[derive(Debug)]
pub enum Defn {
    Fun(String, Vec<String>, Box<Expr>),
}

pub fn reg_to_asm(r: &Reg) -> String {
    match r {
        Reg::RAX => "rax".to_string(),
        Reg::RSP => "rsp".to_string(),
        Reg::RDI => "rdi".to_string(),
        Reg::RCX => "rcx".to_string(),
        Reg::RDX => "rdx".to_string(),
        Reg::RBP => "rbp".to_string(),
    }
}

pub fn val_to_asm(v: &Val) -> String {
    match v {
        Val::Reg(r) => reg_to_asm(r),
        Val::Imm(i) => format!("{}", i),
        Val::RegOffset(r, i) => format!("[{} - 8*({})]", reg_to_asm(&r), i),
        Val::True => format!("3"),
        Val::False => format!("1"),
        Val::Error(e) => match e {
            Err::Overflow => format!("112233"),
            Err::Type => format!("556677"),
        },
    }
}

pub fn assemble(instrs: &Vec<Instr>) -> String {
    instrs
        .iter()
        .map(|i| match i {
            Instr::IMov(v1, v2) => format!("\tmov {}, {}", val_to_asm(v1), val_to_asm(v2)),
            Instr::IAdd(v1, v2) => format!("\tadd {}, {}", val_to_asm(v1), val_to_asm(v2)),
            Instr::ISub(v1, v2) => format!("\tsub {}, {}", val_to_asm(v1), val_to_asm(v2)),
            Instr::IMul(v1, v2) => format!("\timul {}, {}", val_to_asm(v1), val_to_asm(v2)),
            Instr::ISar(v1, v2) => format!("\tsar {}, {}", val_to_asm(v1), val_to_asm(v2)),
            Instr::IAnd(v1, v2) => format!("\tand {}, {}", val_to_asm(v1), val_to_asm(v2)),
            Instr::ITest(v1, v2) => format!("\ttest {}, {}", val_to_asm(v1), val_to_asm(v2)),
            Instr::ICmp(v1, v2) => format!("\tcmp {}, {}", val_to_asm(v1), val_to_asm(v2)),
            Instr::ILabel(l) => format!("{}:", l),
            Instr::IJmp(l) => format!("\tjmp {}", l),
            Instr::IJe(l) => format!("\tje {}", l),
            Instr::IJne(l) => format!("\tjne {}", l),
            Instr::IJl(l) => format!("\tjl {}", l),
            Instr::IJle(l) => format!("\tjle {}", l),
            Instr::IJg(l) => format!("\tjg {}", l),
            Instr::IJge(l) => format!("\tjge {}", l),
            Instr::IJz(l) => format!("\tjz {}", l),
            Instr::IJnz(l) => format!("\tjnz {}", l),
            Instr::IJo => format!("\tjo label_error"),
            Instr::ICall(l) => format!("\tcall {}", l),
            Instr::IPush(v) => format!("\tpush {}", val_to_asm(v)),
        })
        .collect::<Vec<String>>()
        .join("\n")
}

pub fn check_keyword(id: &str) -> bool {
    let keywords = [
        "let", "block", "set!", "if", "add1", "sub1", "isnum", "isbool", "loop", "break", "+", "-",
        "*", "<", ">", "<=", ">=", "=", "true", "false", "input", "print",
    ];
    keywords.contains(&id)
}

pub fn parse_bindings(bind: &Vec<Sexp>) -> Vec<(String, Expr)> {
    // use set to track duplicate identifiers
    let mut set = im::HashSet::<String>::new();
    bind.iter()
        .map(|b| match b {
            Sexp::List(b) => match &b[..] {
                [Sexp::Atom(S(id)), e] => {
                    if check_keyword(id) {
                        panic!("Identifier invalid, matching keyword {id}");
                    }
                    if set.contains(id) {
                        panic!("Duplicate binding {id}")
                    }
                    set = set.update(id.to_string());
                    (id.to_string(), parse_expr(&e))
                }
                _ => panic!("Invalid binding format"),
            },
            _ => panic!("Invalid binding list"),
        })
        .collect()
}

pub fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => {
            let n = i64::try_from(*n).expect("Invalid number in sexp");
            if (n > 4611686018427387903) || (n < -4611686018427387904) {
                panic!("Invalid int (overflow after shift)")
            }
            Expr::Number(n)
        }
        Sexp::Atom(S(s)) => match s.as_str() {
            "true" => Expr::Boolean(true),
            "false" => Expr::Boolean(false),
            "input" => Expr::Input,
            _ => Expr::Id(s.to_string()),
        },
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(func)), e @ ..] if !check_keyword(func) => {
                Expr::Call(func.to_string(), e.iter().map(|e| parse_expr(e)).collect())
            }
            [Sexp::Atom(S(op)), Sexp::List(eb), e] if op == "let" => {
                if eb.is_empty() {
                    panic!("Invalid (empty binding)")
                }
                Expr::Let(parse_bindings(eb), Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), es @ ..] if op == "block" => {
                let content:Vec<Expr> = es.iter().map(|e| parse_expr(e)).collect();
                print!("content {:?}\n", &content);
                if content.is_empty() {
                    panic!("Invalid block (empty block)")
                }
                Expr::Block(content)
            }
            [Sexp::Atom(S(op)), e] => match op.as_str() {
                "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
                "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),
                "isnum" => Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),
                "isbool" => Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),
                "loop" => Expr::Loop(Box::new(parse_expr(e))),
                "break" => Expr::Break(Box::new(parse_expr(e))),
                "print" => Expr::Print(Box::new(parse_expr(e))),
                _ => panic!("Invalid Sexp List Op1"),
            },
            [Sexp::Atom(S(op)), Sexp::Atom(S(s)), e] if op == "set!" => {
                Expr::Set(s.to_string(), Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), e1, e2] => match op.as_str() {
                "+" => Expr::BinOp(
                    Op2::Plus,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                "-" => Expr::BinOp(
                    Op2::Minus,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                "*" => Expr::BinOp(
                    Op2::Times,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                "<" => Expr::BinOp(
                    Op2::Less,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                ">" => Expr::BinOp(
                    Op2::Greater,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                "<=" => Expr::BinOp(
                    Op2::LessEqual,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                ">=" => Expr::BinOp(
                    Op2::GreaterEqual,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                "=" => Expr::BinOp(
                    Op2::Equal,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                _ => panic!("Invalid Sexp List Op2"),
            },
            [Sexp::Atom(S(op)), cond, e1, e2] if op == "if" => Expr::If(
                Box::new(parse_expr(cond)),
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
            ),
            _ => panic!("Invalid Sexp List Op {:?}", vec),
        },
        _ => panic!("Invalid Sexp {}", s),
    }
}

pub fn parse_defn(s: &Sexp, fn_count: &mut HashMap<String, i32>) -> Defn {
    let Sexp::List(es) = s else {
        panic!("Invalid syntax error: expected a list")
    };
    match &es[..] {
        [Sexp::Atom(S(op)), Sexp::List(es), body] if op == "fun" => {
            let [name, params @ ..] = &es[..] else {
                panic!("Invalid missing function name");
            };
            let body_ptr = Box::new(parse_expr(body));
            // let name = parse_ident(name);
            let args: Vec<String> = params
                .iter()
                .map(|e| match e {
                    Sexp::Atom(S(id)) => id.to_string(),
                    _ => panic!("Invalid syntax error: expected an identifier"),
                })
                .collect();
            if let Sexp::Atom(S(fn_name)) = name {
                // check dup function names
                if check_keyword(fn_name) {
                    panic!("Invalid function name {fn_name}")
                }
                if fn_count.contains_key(fn_name) {
                    panic!("Duplicate function name {fn_name}")
                }
                // find if there is any dup string in args
                let mut arg_set = HashSet::new();
                for a in &args {
                    if arg_set.contains(a) {
                        panic!("Duplicate argument {a} in function {fn_name}")
                    }
                    arg_set = arg_set.update(a.to_string());
                }

                *fn_count = fn_count.update(fn_name.to_string(), args.len() as i32);
                Defn::Fun(fn_name.to_string(), args, body_ptr)
            } else {
                panic!("Invalid syntax error: expected an identifier")
            }
        }
        _ => panic!("Invalid syntax error: expected a list of 4 elements"),
    }
}

pub fn parse_program(s: &str) -> Prog {
    let s = format!("({})", s);
    let s = sexp::parse(&s).expect("Invalid Sexp");
    let Sexp::List(es) = s else {
        panic!("Invalid syntax error: expected a list")
    };
    if let [defs_sexp @ .., expr_sexp] = &es[..] {
        let mut fn_count: HashMap<String, i32> = HashMap::new();
        let defs: Vec<Defn> = defs_sexp
            .iter()
            .map(|e| parse_defn(e, &mut fn_count))
            .collect();
        let expr = Box::new(parse_expr(expr_sexp));
        Prog {
            defs,
            expr,
            fn_count,
        }
    } else {
        panic!("Invalid syntax error: program must contain a main expression")
    }
}
