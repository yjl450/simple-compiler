use sexp::Atom::*;
use sexp::*;
use std::env;
use std::fs::File;
use std::io::prelude::*;

#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i32),
    RegOffset(Reg, i32),
}

#[derive(Debug)]
enum Reg {
    RAX,
    RSP,
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
}

#[derive(Debug)]
enum Op1 {
    Add1,
    Sub1,
}

#[derive(Debug)]
enum Op2 {
    Plus,
    Minus,
    Times,
}

#[derive(Debug)]
enum Expr {
    Number(i32),
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
}

fn parse_bindings(bind: &Vec<Sexp>) -> Vec<(String, Expr)> {
    // use set to track duplicate identifiers
    let mut set = im::HashSet::<String>::new();

    bind.iter()
        .map(|b| match b {
            Sexp::List(b) => match &b[..] {
                [Sexp::Atom(S(id)), e] => {
                    if set.contains(id) {
                        panic!("Duplicate binding")
                    }
                    set = set.update(id.to_string());
                    (id.to_string(), parse_expr(&e))
                }
                _ => todo!("Incorrect binding format"),
            },
            _ => todo!("Not a binding"),
        })
        .collect()
}

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => Expr::Number(i32::try_from(*n).unwrap()),
        Sexp::Atom(S(id)) => Expr::Id(id.to_string()),
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(op)), Sexp::List(eb), e] if op == "let" => {
                Expr::Let(parse_bindings(eb), Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), e] if op == "add1" => {
                Expr::UnOp(Op1::Add1, Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), e] if op == "sub1" => {
                Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "+" => Expr::BinOp(
                Op2::Plus,
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
            ),
            [Sexp::Atom(S(op)), e1, e2] if op == "-" => Expr::BinOp(
                Op2::Minus,
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
            ),
            [Sexp::Atom(S(op)), e1, e2] if op == "*" => Expr::BinOp(
                Op2::Times,
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
            ),
            _ => todo!("Error Sexp List Op"),
        },
        _ => todo!("Error Sexp"),
    }
}

fn compile(e: &Expr, instrs: &mut Vec<Instr>, env: &im::HashMap<String, i32>, mut sp: i32) {
    match e {
        Expr::Number(n) => instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n))),
        Expr::Id(id) => {
            if !env.contains_key(id) {
                panic!("Unbound variable identifier {id}")
            }
            instrs.push(Instr::IMov(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, env[id]),
            ))
        }
        Expr::Let(bindings, body) => {
            let mut new_env = env.clone();
            for bind in bindings {
                compile(&bind.1, instrs, &new_env, sp);
                instrs.push(Instr::IMov(
                    Val::RegOffset(Reg::RSP, sp),
                    Val::Reg(Reg::RAX),
                ));
                new_env = new_env.update(bind.0.clone(), sp);
                sp += 1;
            }
            compile(body, instrs, &new_env, sp);
        }
        Expr::UnOp(op, e) => {
            compile(e, instrs, env, sp);
            match op {
                Op1::Add1 => instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1))),
                Op1::Sub1 => instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(1))),
            }
        }
        Expr::BinOp(op, e1, e2) => {
            compile(e2, instrs, env, sp);
            instrs.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, sp),
                Val::Reg(Reg::RAX),
            ));

            compile(e1, instrs, env, sp + 1);
            match op {
                Op2::Plus => instrs.push(Instr::IAdd(
                    Val::Reg(Reg::RAX),
                    Val::RegOffset(Reg::RSP, sp),
                )),
                Op2::Minus => instrs.push(Instr::ISub(
                    Val::Reg(Reg::RAX),
                    Val::RegOffset(Reg::RSP, sp),
                )),
                Op2::Times => instrs.push(Instr::IMul(
                    Val::Reg(Reg::RAX),
                    Val::RegOffset(Reg::RSP, sp),
                )),
            }
        }
    }
}

fn reg_to_asm(r: &Reg) -> String {
    match r {
        Reg::RAX => "rax".to_string(),
        Reg::RSP => "rsp".to_string(),
    }
}

fn val_to_asm(v: &Val) -> String {
    match v {
        Val::Reg(r) => reg_to_asm(r),
        Val::Imm(i) => format!("{}", i),
        Val::RegOffset(r, i) => format!("[{} - 8*{}]", reg_to_asm(&r), i),
    }
}

fn assemble(instrs: &Vec<Instr>) -> String {
    instrs
        .iter()
        .map(|i| match i {
            Instr::IMov(v1, v2) => format!("\tmov {}, {}", val_to_asm(v1), val_to_asm(v2)),
            Instr::IAdd(v1, v2) => format!("\tadd {}, {}", val_to_asm(v1), val_to_asm(v2)),
            Instr::ISub(v1, v2) => format!("\tsub {}, {}", val_to_asm(v1), val_to_asm(v2)),
            Instr::IMul(v1, v2) => format!("\timul {}, {}", val_to_asm(v1), val_to_asm(v2)),
        })
        .collect::<Vec<String>>()
        .join("\n")
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    // You will make result hold the result of actually compiling
    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let expr = parse_expr(&parse(&in_contents).unwrap());
    let mut instrs = Vec::new();
    compile(&expr, &mut instrs, &im::HashMap::new(), 1);
    let result = assemble(&instrs);
    let asm_program = format!(
        "
section .text
global our_code_starts_here
our_code_starts_here:
{}
\tret
",
        result
    );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
