use sexp::Atom::*;
use sexp::*;
use std::env;
use std::fs::File;
use std::io::prelude::*;

#[derive(Debug)]
enum Err {
    Overflow,
    Type,
}

#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i64),
    RegOffset(Reg, i32),
    True,
    False,
    Error(Err),
}

#[derive(Debug)]
enum Reg {
    RAX,
    RSP,
    RDI,
    RCX,
    RDX,
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    // IShl(Val, Val),
    IShr(Val, Val),
    IAnd(Val, Val),
    ITest(Val, Val),
    ICmp(Val, Val),
    ILabel(String),
    IJmp(String),
    IJe(String),
    IJne(String),
    IJb(String),
    IJbe(String),
    IJa(String),
    IJae(String),
    IJz(String),
    IJnz(String),
    IJo,
}

#[derive(Debug)]
enum Op1 {
    Add1,
    Sub1,
    IsNum,
    IsBool,
}

#[derive(Debug)]
enum Op2 {
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
enum Expr {
    Number(i64),
    Boolean(bool),
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    Input,
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Set(String, Box<Expr>),
    Block(Vec<Expr>),
}

fn check_keyword(id: &str) {
    let keywords = [
        "let", "block", "set!", "if", "add1", "sub1", "isnum", "isbool", "loop", "break", "+", "-",
        "*", "<", ">", "<=", ">=", "=",
    ];
    if keywords.contains(&id) {
        panic!("Identifier invalid, matching keyword {id}");
    }
}

fn parse_bindings(bind: &Vec<Sexp>) -> Vec<(String, Expr)> {
    // use set to track duplicate identifiers
    let mut set = im::HashSet::<String>::new();
    bind.iter()
        .map(|b| match b {
            Sexp::List(b) => match &b[..] {
                [Sexp::Atom(S(id)), e] => {
                    check_keyword(id);
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

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => {
            let n = i64::try_from(*n).unwrap();
            if (n > 4611686018427387903) || (n < -4611686018427387904) {
                panic!("Invalid int (overflow after shift)")
            }
            Expr::Number(n)
        },
        Sexp::Atom(S(s)) => match s.as_str() {
            "true" => Expr::Boolean(true),
            "false" => Expr::Boolean(false),
            "input" => Expr::Input,
            _ => Expr::Id(s.to_string()),
        },
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(op)), Sexp::List(eb), e] if op == "let" => {
                Expr::Let(parse_bindings(eb), Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), es @ ..] if op == "block" => {
                Expr::Block(es.iter().map(|e| parse_expr(e)).collect())
            }
            [Sexp::Atom(S(op)), e] => match op.as_str() {
                "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
                "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),
                "isnum" => Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),
                "isbool" => Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),
                "loop" => Expr::Loop(Box::new(parse_expr(e))),
                "break" => Expr::Break(Box::new(parse_expr(e))),
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

fn label_string(label_type: &str, count: &i32) -> String {
    format!("{}_{}", label_type, count)
}

fn rax_check_num_decode(op: &Op2) -> Vec<Instr> {
    match op {
        Op2::Equal => vec![], // check handled in euqal's branch
        _ => {
            let mut instrs = Vec::new();
            // check if oprand is number and throw exception
            instrs.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
            instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Error(Err::Type)));
            instrs.push(Instr::IJnz("label_error".to_string()));
            instrs
        }
    }
}

fn check_overflow() -> Vec<Instr> {
    let mut instrs = Vec::new();
    instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Error(Err::Overflow)));
    instrs.push(Instr::IJo);
    instrs
}

fn compile(
    e: &Expr,
    instrs: &mut Vec<Instr>,
    env: &im::HashMap<String, i32>,
    mut sp: i32,
    count: &mut i32,
    exit_label: &Option<String>,
) {
    match e {
        Expr::Number(n) => instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n << 1))),
        Expr::Boolean(true) => instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))),
        Expr::Boolean(false) => instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))),
        Expr::Input => instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, 1))),
        Expr::Id(id) => {
            if !env.contains_key(id) {
                panic!("Unbound variable identifier {id} in compilation")
            }
            instrs.push(Instr::IMov(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, env[id]),
            ))
        }
        Expr::Let(bindings, body) => {
            let mut new_env = env.clone();
            for bind in bindings {
                compile(&bind.1, instrs, &new_env, sp, count, exit_label);
                instrs.push(Instr::IMov(
                    Val::RegOffset(Reg::RSP, sp),
                    Val::Reg(Reg::RAX),
                ));
                new_env = new_env.update(bind.0.clone(), sp);
                sp += 1;
            }
            compile(body, instrs, &new_env, sp, count, exit_label);
        }
        Expr::UnOp(op, e) => {
            compile(e, instrs, env, sp, count, exit_label);
            match op {
                Op1::Add1 => {
                    instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2)));
                    instrs.extend(check_overflow())
                }
                Op1::Sub1 => {
                    instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)));
                    instrs.extend(check_overflow())
                }
                Op1::IsNum => {
                    instrs.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::True));
                    let label = label_string("isnum_exit", count);
                    *count += 1;
                    instrs.push(Instr::IJz(label.clone()));

                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::False));
                    instrs.push(Instr::ILabel(label));
                }
                Op1::IsBool => {
                    instrs.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::True));
                    let label = label_string("isbool_exit", count);
                    *count += 1;
                    instrs.push(Instr::IJnz(label.clone()));

                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::False));
                    instrs.push(Instr::ILabel(label));
                }
            }
        }
        Expr::BinOp(op, e1, e2) => {
            compile(e2, instrs, env, sp, count, exit_label);
            instrs.extend(rax_check_num_decode(op));
            instrs.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, sp),
                Val::Reg(Reg::RAX),
            ));

            compile(e1, instrs, env, sp + 1, count, exit_label);
            instrs.extend(rax_check_num_decode(op));

            match op {
                Op2::Plus => {
                    instrs.push(Instr::IAdd(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, sp),
                    ));
                    instrs.extend(check_overflow());
                }
                Op2::Minus => {
                    instrs.push(Instr::ISub(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, sp),
                    ));
                    instrs.extend(check_overflow());
                }
                Op2::Times => {
                    instrs.push(Instr::IShr(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::IMul(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, sp),
                    ));
                    instrs.extend(check_overflow());
                }
                Op2::Equal => {
                    // check operands type
                    instrs.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IAnd(Val::Reg(Reg::RCX), Val::Imm(1)));
                    instrs.push(Instr::IMov(
                        Val::Reg(Reg::RDX),
                        Val::RegOffset(Reg::RSP, sp),
                    ));
                    instrs.push(Instr::IAnd(Val::Reg(Reg::RDX), Val::Imm(1)));
                    instrs.push(Instr::ICmp(Val::Reg(Reg::RCX), Val::Reg(Reg::RDX)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Error(Err::Type)));
                    instrs.push(Instr::IJne("label_error".to_string()));
                    // compare operands value
                    instrs.push(Instr::ICmp(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, sp),
                    ));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::False));
                    let label = label_string("equal_exit", count);
                    *count += 1;
                    instrs.push(Instr::IJne(label.clone()));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::True));
                    instrs.push(Instr::ILabel(label));
                }
                Op2::Greater => {
                    instrs.push(Instr::ICmp(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, sp),
                    ));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::True));
                    let label = label_string("greater_exit", count);
                    *count += 1;
                    instrs.push(Instr::IJa(label.clone()));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::False));
                    instrs.push(Instr::ILabel(label));
                }
                Op2::GreaterEqual => {
                    instrs.push(Instr::ICmp(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, sp),
                    ));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::True));
                    let label = label_string("greaterequal_exit", count);
                    *count += 1;
                    instrs.push(Instr::IJae(label.clone()));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::False));
                    instrs.push(Instr::ILabel(label));
                }
                Op2::Less => {
                    instrs.push(Instr::ICmp(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, sp),
                    ));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::True));
                    let label = label_string("less_exit", count);
                    *count += 1;
                    instrs.push(Instr::IJb(label.clone()));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::False));
                    instrs.push(Instr::ILabel(label));
                }
                Op2::LessEqual => {
                    instrs.push(Instr::ICmp(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, sp),
                    ));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::True));
                    let label = label_string("lessequal_exit", count);
                    *count += 1;
                    instrs.push(Instr::IJbe(label.clone()));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::False));
                    instrs.push(Instr::ILabel(label));
                }
            }
        }
        Expr::If(cond, e1, e2) => {
            let label_else = label_string("if_else", count);
            let label_exit = label_string("if_exit", count);
            *count += 1;
            compile(cond, instrs, env, sp, count, exit_label);
            instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::False));
            instrs.push(Instr::IJe(label_else.clone()));
            compile(e1, instrs, env, sp, count, exit_label);
            instrs.push(Instr::IJmp(label_exit.clone()));
            instrs.push(Instr::ILabel(label_else));
            compile(e2, instrs, env, sp, count, exit_label);
            instrs.push(Instr::ILabel(label_exit));
        }
        Expr::Loop(e) => {
            let label_loop = label_string("loop_start", count);
            let label_exit = label_string("loop_exit", count);
            *count += 1;
            instrs.push(Instr::ILabel(label_loop.clone()));
            compile(e, instrs, env, sp, count, &Some(label_exit.clone()));
            instrs.push(Instr::IJmp(label_loop));
            instrs.push(Instr::ILabel(label_exit));
        }
        Expr::Break(e) => {
            if let Some(label_exit) = exit_label {
                compile(e, instrs, env, sp, count, &None);
                instrs.push(Instr::IJmp(label_exit.clone()));
            } else {
                panic!("break outside of loop")
            }
        }
        Expr::Set(name, e) => {
            if !env.contains_key(name) {
                panic!("Unbound variable identifier {name}")
            }
            compile(e, instrs, env, sp, count, exit_label);
            instrs.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, env[name]),
                Val::Reg(Reg::RAX),
            ));
        }
        Expr::Block(eb) => {
            for e in eb {
                compile(e, instrs, env, sp, count, exit_label);
            }
        }
    }
}

fn reg_to_asm(r: &Reg) -> String {
    match r {
        Reg::RAX => "rax".to_string(),
        Reg::RSP => "rsp".to_string(),
        Reg::RDI => "rdi".to_string(),
        Reg::RCX => "rcx".to_string(),
        Reg::RDX => "rdx".to_string(),
    }
}

fn val_to_asm(v: &Val) -> String {
    match v {
        Val::Reg(r) => reg_to_asm(r),
        Val::Imm(i) => format!("{}", i),
        Val::RegOffset(r, i) => format!("[{} - 8*{}]", reg_to_asm(&r), i),
        Val::True => format!("3"),
        Val::False => format!("1"),
        Val::Error(e) => match e {
            Err::Overflow => format!("112233"),
            Err::Type => format!("556677"),
        },
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
            // Instr::IShl(v1, v2) => format!("\tshl {}, {}", val_to_asm(v1), val_to_asm(v2)),
            Instr::IShr(v1, v2) => format!("\tshr {}, {}", val_to_asm(v1), val_to_asm(v2)),
            Instr::IAnd(v1, v2) => format!("\tand {}, {}", val_to_asm(v1), val_to_asm(v2)),
            Instr::ITest(v1, v2) => format!("\ttest {}, {}", val_to_asm(v1), val_to_asm(v2)),
            Instr::ICmp(v1, v2) => format!("\tcmp {}, {}", val_to_asm(v1), val_to_asm(v2)),
            Instr::ILabel(l) => format!("{}:", l),
            Instr::IJmp(l) => format!("\tjmp {}", l),
            Instr::IJe(l) => format!("\tje {}", l),
            Instr::IJne(l) => format!("\tjne {}", l),
            Instr::IJb(l) => format!("\tjb {}", l),
            Instr::IJbe(l) => format!("\tjbe {}", l),
            Instr::IJa(l) => format!("\tja {}", l),
            Instr::IJae(l) => format!("\tjae {}", l),
            Instr::IJz(l) => format!("\tjz {}", l),
            Instr::IJnz(l) => format!("\tjnz {}", l),
            Instr::IJo => format!("\tjo label_error"),
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
    // print!("Sexp: {}\n", parse(&in_contents).unwrap());
    let expr = parse_expr(&parse(&in_contents).unwrap());
    // print!("Parsed: {:?}\n", expr);
    let mut instrs = Vec::new();
    compile(&expr, &mut instrs, &im::HashMap::new(), 2, &mut 0, &None);
    let result = assemble(&instrs);
    let asm_program = format!(
        "section .text
extern snek_error
global our_code_starts_here
label_error:
\tpush rsp
\tcall snek_error
our_code_starts_here:
\tmov [rsp - 8], rdi
{}
\tret
",
        result
    );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;
    // print!("Assembly:\n{}\n", asm_program);
    Ok(())
}
