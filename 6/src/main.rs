use std::fs::File;
use std::io::prelude::*;
use std::{cmp::max, env};

use expr::{Defn, Expr};
use im::{hashmap, hashset, HashMap, HashSet};

pub mod expr;

type Stack = HashMap<String, i32>;

fn test_number(code: usize) -> String {
    format!(
        "mov rcx, rax
             and rcx, 1
             cmp rcx, 0
             mov rdi, {code}
             jne label_error"
    )
}

fn label(prefix: String, count: &i32) -> String {
    format!("{prefix}_{count}")
}

const FALSE: usize = 3;
const TRUE: usize = 7;

fn compile_args(
    exprs: &Vec<Expr>,
    env: &Stack,
    sp: usize,
    count: &mut i32,
    brk: &str,
    f: &str,
) -> String {
    let args_code: Vec<String> = exprs
        .iter()
        .enumerate()
        .map(|(i, e)| {
            let e_code = compile_expr(e, env, sp + i, count, brk, false, f);
            let e_pos = sp + i;
            format!(
                "{e_code}
                 mov [rbp - 8*{e_pos}], rax",
            )
        })
        .collect();
    args_code.join("\n")
}

fn push_args(sp: usize, args: usize) -> String {
    let mut res: Vec<String> = vec![];
    for i in (0..args).rev() {
        let i_pos = sp + i;
        let i_code = format!(
            "mov rcx, [rbp - 8*{i_pos}]
             push rcx",
        );
        res.push(i_code);
    }
    res.join("\n")
}

fn lookup_var(env: &Stack, x: &str) -> i32 {
    match env.get(x) {
        None => panic!("Unbound variable {}", x),
        Some(x_pos) => *x_pos,
    }
}

fn compile_var(env: &Stack, x: &str) -> String {
    let x_pos = lookup_var(env, x);
    format!("mov rax, [rbp - 8*{}]", x_pos)
}

fn compile_expr(
    e: &Expr,
    env: &Stack,
    sp: usize,
    count: &mut i32,
    brk: &str,
    tr: bool,
    f: &str,
) -> String {
    match e {
        Expr::Num(n) => format!("mov rax, {}", *n << 1),
        Expr::Add1(subexpr) => {
            compile_expr(subexpr, env, sp, count, brk, false, f) + "\nadd rax, 2"
        }
        Expr::Sub1(subexpr) => {
            compile_expr(subexpr, env, sp, count, brk, false, f) + "\nsub rax, 2"
        }
        Expr::Neg(subexpr) => compile_expr(subexpr, env, sp, count, brk, false, f) + "\nneg rax",
        Expr::Var(x) => compile_var(env, x),
        Expr::Let(x, e1, e2) => {
            let x_pos = sp;
            let mut new_env = env.update(x.to_string(), x_pos as i32);
            match *e1.clone() {
                Expr::Fun(_) => {}
                _ => new_env = env.clone(),
            }
            let e1_code = compile_expr(e1, &new_env, sp, count, brk, false, f);
            let x_save = format!("mov [rbp - 8*{}], rax", x_pos);
            new_env = env.update(x.to_string(), x_pos as i32);
            let e2_code = compile_expr(e2, &new_env, sp + 1, count, brk, tr, f);
            format!("{e1_code:}\n{x_save:}\n{e2_code:}")
        }
        Expr::Plus(e1, e2) => {
            let e1_code = compile_expr(e1, env, sp, count, brk, false, f);
            let e2_code = compile_expr(e2, env, sp + 1, count, brk, false, f);
            let test_code_1 = test_number(99);
            let test_code_2 = test_number(33);

            format!(
                "{e1_code}
                 {test_code_1}
                 mov [rbp - 8*{sp}], rax
                 {e2_code}
                 {test_code_2}
                 add rax, [rbp - 8*{sp}]
                "
            )
        }
        Expr::Mult(e1, e2) => {
            let e1_code = compile_expr(e1, env, sp, count, brk, false, f);
            let e2_code = compile_expr(e2, env, sp + 1, count, brk, false, f);
            let test_code_1 = test_number(99);
            let test_code_2 = test_number(33);
            let off = 8 * sp;
            format!(
                "{e1_code}
                 {test_code_1}
                 mov [rbp - {off}], rax
                 {e2_code}
                 {test_code_2}
                 sar rax, 1
                 imul rax, [rbp - {off}]
                "
            )
        }
        Expr::If(e_cond, e_then, e_else) => {
            *count += 1;
            let e_cond_code = compile_expr(e_cond, env, sp, count, brk, false, f);
            let e_then_code = compile_expr(e_then, env, sp, count, brk, tr, f);
            let e_else_code = compile_expr(e_else, env, sp, count, brk, tr, f);
            format!(
                "{e_cond_code}
                      cmp rax, {FALSE}
                      je label_else_{count}
                      {e_then_code}
                      jmp label_exit_{count}
                    label_else_{count}:
                      {e_else_code}
                    label_exit_{count}:"
            )
        }
        Expr::Input => {
            format!("mov rax, [rbp - 8]")
        }
        Expr::True => {
            format!("mov rax, {TRUE}")
        }
        Expr::False => {
            format!("mov rax, {FALSE}")
        }
        Expr::Eq(e1, e2) => {
            let e1_code = compile_expr(e1, env, sp, count, brk, false, f);
            let e2_code = compile_expr(e2, env, sp + 1, count, brk, false, f);
            *count += 1;
            let exit = label("eq_exit".to_string(), count);
            format!(
                "{e1_code}
                 mov [rbp - 8*{sp}], rax
                 {e2_code}
                 cmp rax, [rbp - 8*{sp}]
                 mov rax, {FALSE}
                 jne {exit}
                 mov rax, {TRUE}
               {exit}:
                "
            )
        }
        Expr::Le(e1, e2) => {
            let e1_code = compile_expr(e1, env, sp, count, brk, false, f);
            let e2_code = compile_expr(e2, env, sp + 1, count, brk, false, f);
            *count += 1;
            let exit = label("eq_exit".to_string(), count);
            format!(
                "{e1_code}
                 mov [rbp - 8*{sp}], rax
                 {e2_code}
                 cmp rax, [rbp - 8*{sp}]
                 mov rax, {FALSE}
                 jl {exit}
                 mov rax, {TRUE}
               {exit}:
                "
            )
        }
        Expr::Set(x, e) => {
            let x_pos = env.get(x).expect(&format!("Unbound variable identifier {x}"));
            let e_code = compile_expr(e, env, sp, count, brk, false, f);
            format!(
                "{e_code}
                     mov [rbp - 8*{}], rax",
                x_pos
            )
        }
        Expr::Block(es) => {
            let n = es.len();
            let e_codes: Vec<String> = es
                .iter()
                .enumerate()
                .map(|(i, e)| compile_expr(e, env, sp, count, brk, tr && i == n - 1, f))
                .collect();
            e_codes.join("\n")
        }
        Expr::Loop(e) => {
            *count += 1;
            let loop_start = label("loop_start".to_string(), count);
            let loop_exit = label("loop_exit".to_string(), count);
            let e_code = compile_expr(e, env, sp, count, &loop_exit, false, f);
            format!(
                "{loop_start}:
                        {e_code}
                        jmp {loop_start}
                     {loop_exit}:"
            )
        }
        Expr::Break(e) => {
            let e_code = compile_expr(e, env, sp, count, brk, false, f);
            format!(
                "{e_code}
                     jmp {brk}"
            )
        }
        Expr::Print(e) => {
            let e_code = compile_expr(e, env, sp, count, brk, false, f);
            format!(
                "{e_code}
                 mov rdi, rax
                 call snek_print"
            )
        }
        Expr::Vec(e1, e2) => {
            let e1_code = compile_expr(e1, env, sp, count, brk, false, f);
            let e2_code = compile_expr(e2, env, sp + 1, count, brk, false, f);
            format!(
                "{e1_code}
                 mov [rbp - 8*{sp}], rax
                 {e2_code}
                 mov rcx, [rbp - 8*{sp}]
                 mov [r11 + 0], rcx
                 mov [r11 + 8], rax
                 mov rax, r11
                 add rax, 0x1
                 add r11, 16"
            )
        }
        Expr::Get(e, idx) => {
            let e_code: String = compile_expr(e, env, sp, count, brk, false, f);
            let idx = match idx {
                expr::Index::Zero => 0,
                expr::Index::One => 1,
            };
            format!(
                "{e_code}
                 ;; CHECK rax is a pointer?
                 sub rax, 0x1 ;; strip the bit
                 mov rax, [rax + 8*{idx}]",
            )
        }
        Expr::Call(f, exprs) => {
            let eval_args = compile_args(exprs, env, sp, count, brk, f);
            let push_args = push_args(sp, exprs.len());
            let eval_f = compile_var(env, f);
            let pop_args = format!("add rsp, 8*{}", exprs.len());
            let arg_len = exprs.len();
            format!(
                "{eval_args}
                 {push_args}
                 {eval_f}
                 ;; check function ptr and arity
                 mov rcx, rax
                 and rcx, 0b111
                 cmp rcx, 0b101
                 mov rdi, 299 ;; callee is not a function
                 jne label_error

                 sub rax, 0b101
                 
                 mov rcx, [rax + 8]
                 cmp rcx, {arg_len}
                 mov rdi, 199 ;; arity mismatch
                 jne label_error

                 call [rax]
                 {pop_args}"
            )
        }
        Expr::Fun(defn) => {
            let f = defn_name(defn, count);
            compile_defn(defn, &f, count, env)
        }
    }
}

fn compile_exit() -> String {
    format!(
        "mov rsp, rbp
             pop rbp
             ret"
    )
}

fn compile_entry(e: &Expr, sp: usize) -> String {
    let free_vars = free_vars(e);
    let vars = expr_vars(e) + sp + free_vars.len() + 100;
    // let vars = expr_vars(e) + sp + 100;
    format!(
        "push rbp
         mov rbp, rsp
         sub rsp, 8*{vars}"
    )
}

fn free_vars(e: &Expr) -> HashSet<String> {
    match e {
        Expr::Num(_) | Expr::Input | Expr::True | Expr::False => {
            hashset! {}
        }

        Expr::Add1(e)
        | Expr::Sub1(e)
        | Expr::Neg(e)
        | Expr::Set(_, e)
        | Expr::Loop(e)
        | Expr::Break(e)
        | Expr::Print(e)
        | Expr::Get(e, _) => free_vars(e),

        Expr::Let(x, e1, e2) => free_vars(e1)
            .union(free_vars(e2))
            .relative_complement(hashset![x.clone()]),

        Expr::Eq(e1, e2)
        | Expr::Le(e1, e2)
        | Expr::Plus(e1, e2)
        | Expr::Mult(e1, e2)
        | Expr::Vec(e1, e2) => free_vars(e1).union(free_vars(e2)),

        Expr::If(e1, e2, e3) => free_vars(e1).union(free_vars(e2)).union(free_vars(e3)),

        Expr::Block(es) => es
            .iter()
            .map(|e| free_vars(e))
            .fold(hashset! {}, |a, b| a.union(b)),

        Expr::Call(x, exprs) => exprs
            .iter()
            .map(|e| free_vars(e))
            .fold(hashset! {}, |a, b| a.union(b))
            .union(hashset![x.clone()]),

        Expr::Var(x) => hashset![x.clone()],
        Expr::Fun(defn) => {
            free_vars(&defn.body).relative_complement(defn.params.iter().cloned().collect())
        }
    }
}

fn expr_vars(e: &Expr) -> usize {
    match e {
        Expr::Num(_) | Expr::Var(_) | Expr::Input | Expr::True | Expr::False | Expr::Fun(_) => 0,
        Expr::Add1(e)
        | Expr::Sub1(e)
        | Expr::Neg(e)
        | Expr::Set(_, e)
        | Expr::Loop(e)
        | Expr::Break(e)
        | Expr::Print(e)
        | Expr::Get(e, _) => expr_vars(e),
        Expr::Let(_, e1, e2)
        | Expr::Eq(e1, e2)
        | Expr::Le(e1, e2)
        | Expr::Plus(e1, e2)
        | Expr::Mult(e1, e2)
        | Expr::Vec(e1, e2) => max(expr_vars(e1), 1 + expr_vars(e2)),
        Expr::If(e1, e2, e3) => max(expr_vars(e1), max(expr_vars(e2), expr_vars(e3))),
        Expr::Block(es) => es.iter().map(|e| expr_vars(e)).max().expect("Invalid"),
        Expr::Call(_, exprs) => exprs
            .iter()
            .enumerate()
            .map(|(i, e)| i + expr_vars(e))
            .max()
            .expect("Invalid"),
    }
}

fn init_env(args: &[String]) -> Stack {
    let mut env = hashmap! {};
    for (i, a) in args.iter().enumerate() {
        env = env.update(a.to_string(), -2 - i as i32);
    }
    env
}

fn defn_name(def: &Defn, count: &mut i32) -> String {
    let old_count = count.clone();
    *count += 1;
    match def.name {
        Some(ref name) => format!("{name}_{old_count}"),
        None => {
            format!("anon_{old_count}")
        }
    }
}

fn compile_defn(defn: &Defn, f: &str, count: &mut i32, scope: &Stack) -> String {
    let mut sp = 1;
    let arity = defn.params.len();
    let fun_entry = compile_entry(&defn.body, sp);
    let exit_label = format!("fun_exit_{f}");
    let mut env = init_env(&defn.params);
    let free_vars = free_vars(&Expr::Fun(defn.clone()));
    let mut load_vars = "".to_string();
    let mut save_vars = "".to_string();
    for (i, v) in free_vars.iter().enumerate() {
        let src = scope
            .get(v)
            .expect(&format!("Unbound free var {} in {} {:?}", v, f, scope));
        let dst = i + 2;
        if defn.name != Some(v.to_string()) {
            save_vars += &format!("mov rcx, [rbp - 8*{src}]\n");
        } else {
            save_vars += &format!("mov rcx, r11\n");
            save_vars += &format!("add rcx, 0b101\n");
        }
        save_vars += &format!("mov [r11 + 8*{dst}], rcx\n");
        load_vars += &format!("mov rcx, [rax + 8*{dst}]\n");
        load_vars += &format!("mov [rbp - (8*{sp})], rcx\n");
        env = env.update(v.to_string(), sp as i32);
        sp += 1;
    }
    let body_code = compile_expr(&defn.body, &env, sp, count, &exit_label, true, f);
    let fun_exit = compile_exit();
    let length = (free_vars.len() + 2) * 8;
    format!(
        "jmp fun_finish_{f}
         fun_start_{f}:
           {fun_entry}
         fun_body_{f}:
         ;; load vars-------------
         {load_vars}
         ;; load vars end---------
           {body_code}
         fun_exit_{f}:
           {fun_exit}
         fun_finish_{f}:
            mov rcx, fun_start_{f} ;; save function pointer
            mov [r11], rcx
            mov rcx, {arity} ;; save arity
            mov [r11 + 8], rcx
            ;; save free vars
            {save_vars}
            mov rax, r11
            add rax, 0b101
            add r11, {length}
            "
    )
}

fn compile_prog(prog: &Expr) -> String {
    let mut count = 0;
    let e_entry = compile_entry(prog, 1);
    let e_code = compile_expr(
        prog,
        &hashmap! {},
        2,
        &mut count,
        "time_to_exit",
        false,
        "main",
    );
    let e_exit = compile_exit();
    format!(
        "section .text
global our_code_starts_here
extern snek_error
extern snek_print
label_error:
  push rsp
  call snek_error
our_code_starts_here:
 {e_entry}
 mov [rbp - 8], rdi
 mov r11, rsi               ;; save start of heap in r11
 {e_code}
 {e_exit}
time_to_exit:
  ret
"
    )
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let prog = expr::parse(&in_contents);
    let mut out_file = File::create(out_name)?;
    let asm_program = compile_prog(&prog);

    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
