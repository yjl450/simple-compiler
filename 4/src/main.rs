use std::cmp::max;
use std::env;
use std::fs::File;
use std::io::prelude::*;

use expr::{assemble, parse_program, Defn, Err, Expr, Instr, Op1, Op2, Prog, Reg, Val};
use im::HashMap;
pub mod expr;

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

fn compile_expr(
    e: &Expr,
    instrs: &mut Vec<Instr>,
    env: &im::HashMap<String, i32>,
    mut sp: i32,
    count: &mut i32,
    exit_label: &Option<String>,
    fn_name: &Option<String>,
    fn_count: &HashMap<String, i32>,
    fdef: bool,
    tr: bool,
) {
    match e {
        Expr::Number(n) => instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n << 1))),
        Expr::Boolean(true) => instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))),
        Expr::Boolean(false) => instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))),
        Expr::Input => {
            if fdef {
                match fn_name {
                    Some(f) => panic!("input used in function body {}", f),
                    None => panic!("input used in function body"),
                }
            }
            instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RBP, 1)))
        }
        Expr::Id(id) => {
            if !env.contains_key(id) {
                panic!("Unbound variable identifier {id} in compilation")
            }
            instrs.push(Instr::IMov(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RBP, env[id]),
            ))
        }
        Expr::Let(bindings, body) => {
            let mut new_env = env.clone();
            for bind in bindings {
                compile_expr(
                    &bind.1, instrs, &new_env, sp, count, exit_label, fn_name, fn_count, fdef,
                    false,
                );
                instrs.push(Instr::IMov(
                    Val::RegOffset(Reg::RBP, sp),
                    Val::Reg(Reg::RAX),
                ));
                new_env = new_env.update(bind.0.clone(), sp);
                sp += 1;
            }
            compile_expr(
                body, instrs, &new_env, sp, count, exit_label, fn_name, fn_count, fdef, tr,
            );
        }
        Expr::UnOp(op, e) => {
            compile_expr(
                e, instrs, env, sp, count, exit_label, fn_name, fn_count, fdef, false,
            );
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
            compile_expr(
                e2, instrs, env, sp, count, exit_label, fn_name, fn_count, fdef, false,
            );
            instrs.extend(rax_check_num_decode(op));
            instrs.push(Instr::IMov(
                Val::RegOffset(Reg::RBP, sp),
                Val::Reg(Reg::RAX),
            ));

            compile_expr(
                e1,
                instrs,
                env,
                sp + 1,
                count,
                exit_label,
                fn_name,
                fn_count,
                fdef,
                false,
            );
            instrs.extend(rax_check_num_decode(op));

            match op {
                Op2::Plus => {
                    instrs.push(Instr::IAdd(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RBP, sp),
                    ));
                    instrs.extend(check_overflow());
                }
                Op2::Minus => {
                    instrs.push(Instr::ISub(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RBP, sp),
                    ));
                    instrs.extend(check_overflow());
                }
                Op2::Times => {
                    instrs.push(Instr::ISar(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::IMul(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RBP, sp),
                    ));
                    instrs.extend(check_overflow());
                }
                Op2::Equal => {
                    // check operands type
                    instrs.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IAnd(Val::Reg(Reg::RCX), Val::Imm(1)));
                    instrs.push(Instr::IMov(
                        Val::Reg(Reg::RDX),
                        Val::RegOffset(Reg::RBP, sp),
                    ));
                    instrs.push(Instr::IAnd(Val::Reg(Reg::RDX), Val::Imm(1)));
                    instrs.push(Instr::ICmp(Val::Reg(Reg::RCX), Val::Reg(Reg::RDX)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Error(Err::Type)));
                    instrs.push(Instr::IJne("label_error".to_string()));
                    // compare operands value
                    instrs.push(Instr::ICmp(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RBP, sp),
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
                        Val::RegOffset(Reg::RBP, sp),
                    ));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::True));
                    let label = label_string("greater_exit", count);
                    *count += 1;
                    instrs.push(Instr::IJg(label.clone()));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::False));
                    instrs.push(Instr::ILabel(label));
                }
                Op2::GreaterEqual => {
                    instrs.push(Instr::ICmp(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RBP, sp),
                    ));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::True));
                    let label = label_string("greaterequal_exit", count);
                    *count += 1;
                    instrs.push(Instr::IJge(label.clone()));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::False));
                    instrs.push(Instr::ILabel(label));
                }
                Op2::Less => {
                    instrs.push(Instr::ICmp(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RBP, sp),
                    ));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::True));
                    let label = label_string("less_exit", count);
                    *count += 1;
                    instrs.push(Instr::IJl(label.clone()));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::False));
                    instrs.push(Instr::ILabel(label));
                }
                Op2::LessEqual => {
                    instrs.push(Instr::ICmp(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RBP, sp),
                    ));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::True));
                    let label = label_string("lessequal_exit", count);
                    *count += 1;
                    instrs.push(Instr::IJle(label.clone()));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::False));
                    instrs.push(Instr::ILabel(label));
                }
            }
        }
        Expr::If(cond, e1, e2) => {
            let label_else = label_string("if_else", count);
            let label_exit = label_string("if_exit", count);
            *count += 1;
            compile_expr(
                cond, instrs, env, sp, count, exit_label, fn_name, fn_count, fdef, false,
            );
            instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::False));
            instrs.push(Instr::IJe(label_else.clone()));
            compile_expr(
                e1, instrs, env, sp, count, exit_label, fn_name, fn_count, fdef, tr,
            );
            instrs.push(Instr::IJmp(label_exit.clone()));
            instrs.push(Instr::ILabel(label_else));
            compile_expr(
                e2, instrs, env, sp, count, exit_label, fn_name, fn_count, fdef, tr,
            );
            instrs.push(Instr::ILabel(label_exit));
        }
        Expr::Loop(e) => {
            let label_loop = label_string("loop_start", count);
            let label_exit = label_string("loop_exit", count);
            *count += 1;
            instrs.push(Instr::ILabel(label_loop.clone()));
            compile_expr(
                e,
                instrs,
                env,
                sp,
                count,
                &Some(label_exit.clone()),
                fn_name,
                fn_count,
                fdef,
                false,
            );
            instrs.push(Instr::IJmp(label_loop));
            instrs.push(Instr::ILabel(label_exit));
        }
        Expr::Break(e) => {
            if let Some(label_exit) = exit_label {
                compile_expr(
                    e, instrs, env, sp, count, &None, fn_name, fn_count, fdef, false,
                );
                instrs.push(Instr::IJmp(label_exit.clone()));
            } else {
                panic!("break outside of loop")
            }
        }
        Expr::Set(name, e) => {
            if !env.contains_key(name) {
                panic!("Unbound variable identifier {name}")
            }
            compile_expr(
                e, instrs, env, sp, count, exit_label, fn_name, fn_count, fdef, false,
            );
            instrs.push(Instr::IMov(
                Val::RegOffset(Reg::RBP, env[name]),
                Val::Reg(Reg::RAX),
            ));
        }
        Expr::Block(eb) => {
            for e in eb {
                compile_expr(
                    e, instrs, env, sp, count, exit_label, fn_name, fn_count, fdef, false,
                );
            }
            let _ = eb.iter().enumerate().map(|(i, e)| {
                compile_expr(
                    e,
                    instrs,
                    env,
                    sp,
                    count,
                    exit_label,
                    fn_name,
                    fn_count,
                    fdef,
                    tr && (i == eb.len() - 1),
                )
            });
        }
        Expr::Print(e) => {
            compile_expr(
                e, instrs, env, sp, count, exit_label, fn_name, fn_count, fdef, false,
            );
            instrs.extend(vec![
                Instr::IMov(Val::Reg(Reg::RDI), Val::Reg(Reg::RAX)),
                Instr::ICall("snek_print".to_string()),
            ]);
        }
        Expr::Call(f, e) => {
            let n = e.len() as i32;
            if !fn_count.contains_key(f) {
                panic!("Function {f} undefined")
            }
            if fn_count[f] != n {
                panic!(
                    "Function {f} called with {n} arguments, requiring {}",
                    fn_count[f]
                )
            }
            for arg in e.iter() {
                compile_expr(
                    arg, instrs, env, sp, count, exit_label, fn_name, fn_count, fdef, false,
                );
                instrs.push(Instr::IMov(
                    Val::RegOffset(Reg::RBP, sp),
                    Val::Reg(Reg::RAX),
                ));
                sp += 1;
            }
            if tr && (Some(f.clone()) == *fn_name) {
                for i in 0..n {
                    instrs.push(Instr::IMov(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RBP, sp - n + i),
                    ));
                    instrs.push(Instr::IMov(
                        Val::RegOffset(Reg::RBP, -i - 2),
                        Val::Reg(Reg::RAX),
                    ));
                }

                instrs.push(Instr::IJmp(format!("fun_body_{f}")));
            } else {
                for i in 0..n {
                    instrs.push(Instr::IMov(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RBP, sp - i - 1),
                    ));
                    instrs.push(Instr::IPush(Val::Reg(Reg::RAX)));
                }
                instrs.push(Instr::ICall(format!("fun_start_{f}")));
                instrs.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(n as i64)));
            }
        }
    }
}

fn expr_vars(e: &Expr) -> i32 {
    match e {
        Expr::Number(_) | Expr::Id(_) | Expr::Input | Expr::Boolean(_) => 0,

        Expr::UnOp(_, e) | Expr::Set(_, e) | Expr::Loop(e) | Expr::Break(e) | Expr::Print(e) => {
            expr_vars(e)
        }

        Expr::BinOp(_, e1, e2) => max(expr_vars(e1), 1 + expr_vars(e2)),

        Expr::Let(binding, e) => {
            let len = binding.len();
            let mut max_vars = 0;

            binding.iter().for_each(|(_, be)| {
                max_vars = max(max_vars, expr_vars(be));
            });

            max(max_vars, expr_vars(e) + len as i32)
        }

        Expr::If(e1, e2, e3) => max(expr_vars(e1), max(expr_vars(e2), expr_vars(e3))),
        Expr::Block(es) => es
            .iter()
            .map(|e: &Expr| expr_vars(e))
            .max()
            .expect("Invalid block"),
        Expr::Call(_, es) => match es.as_slice() {
            [] => 0,
            _ => es
                .iter()
                .enumerate()
                .map(|(i, e)| i as i32 + expr_vars(e))
                .max()
                .expect("Invalid call"),
        },
    }
}

fn compile_entry(e: &Expr, sp: i32) -> String {
    let vars = expr_vars(e) + sp;
    format!(
        "\tpush rbp
\tmov rbp, rsp
\tsub rsp, 8*{vars}"
    )
}

fn compile_exit() -> String {
    format!(
        "\tmov rsp, rbp
\tpop rbp
\tret"
    )
}

fn init_env(args: &[String]) -> im::HashMap<String, i32> {
    let mut env = im::HashMap::new();
    for (i, arg) in args.iter().enumerate() {
        env = env.update(arg.to_string(), -(i as i32) - 2);
    }
    env
}

fn compile_def_body(
    args: &[String],
    sp: i32,
    body: &Expr,
    count: &mut i32,
    fn_name: &String,
    fn_count: &HashMap<String, i32>,
) -> String {
    let fun_entry = compile_entry(body, sp);
    let mut body_instrs = Vec::<Instr>::new();
    let env: HashMap<String, i32> = init_env(args);
    compile_expr(
        body,
        &mut body_instrs,
        &env,
        sp,
        count,
        &None,
        &Some(fn_name.clone()),
        fn_count,
        true,
        true,
    );
    let body_code = assemble(&body_instrs);
    let fun_exit = compile_exit();
    format!("{fun_entry}\nfun_body_{fn_name}:\n{body_code}\nfun_exit_{fn_name}:\n{fun_exit}")
}

fn compile_def(def: &Defn, count: &mut i32, fn_count: &HashMap<String, i32>) -> String {
    let (f, args, body) = match def {
        Defn::Fun(f, xs, e) => (f, xs, e),
    };
    // check input in function def
    if args.contains(&"input".to_string()) {
        panic!("input used as a function argument {f}");
    }

    let body = compile_def_body(&args, 1, body, count, f, fn_count);
    format!(
        "fun_start_{f}:
{body}"
    )
}

fn compile_program(prog: &Prog) -> String {
    let mut count = 0;
    let defs_code = prog
        .defs
        .iter()
        .map(|def| compile_def(def, &mut count, &prog.fn_count))
        .collect::<Vec<String>>()
        .join("\n");

    let e_entry = compile_entry(&prog.expr, 1);
    let mut expr_instrs = Vec::<Instr>::new();
    compile_expr(
        &prog.expr,
        &mut expr_instrs,
        &im::HashMap::new(),
        2,
        &mut count,
        &None,
        &None,
        &prog.fn_count,
        false,
        false,
    );
    let e_code = assemble(&expr_instrs);
    let e_exit = compile_exit();
    format!(
        "section .text
global our_code_starts_here
extern snek_error
extern snek_print
label_error:
\tpush rsp
\tcall snek_error
{defs_code}
our_code_starts_here:
{e_entry}
\tmov [rbp - 8], rdi
{e_code}
{e_exit}
our_code_exit:
\tret
"
    )
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    // You will make result hold the result of actually compiling
    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;
    let prog = parse_program(&in_contents);
    // print!("Parsed: {:?}\n", prog);
    let asm_program = compile_program(&prog);

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;
    // print!("Assembly:\n{}\n", asm_program);
    Ok(())
}
