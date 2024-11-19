use sexp::Atom::*;
use sexp::*;
use std::fs::File;
use std::io::prelude::*;
use std::{env, vec};

enum Expr {
    Num(i32),
    Add1(Box<Expr>),
    Sub1(Box<Expr>),
    Negate(Box<Expr>),
}
#[derive(Debug)]
pub enum Val {
    Rax,
    Imm(i32),
}

#[derive(Debug)]
pub enum X86 {
    Mov(Val, Val),
    Add(Val, Val),
    Sub(Val, Val),
    Neg(Val),
}

#[cfg(test)]
fn eval(e: &Expr) -> i32 {
    match e {
        Expr::Num(n) => *n,
        Expr::Add1(e1) => eval(e1) + 1,
        Expr::Sub1(e1) => eval(e1) - 1,
        Expr::Negate(e1) => -eval(e1),
    }
}

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => Expr::Num(i32::try_from(*n).unwrap()),
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(op)), e] if op == "add1" => Expr::Add1(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::Sub1(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "negate" => Expr::Negate(Box::new(parse_expr(e))),
            _ => todo!("Error Sexp List Op"),
        },
        _ => todo!("Error Sexp"),
    }
}

fn compile(e: &Expr, x86: &mut Vec<X86>) {
    match e {
        Expr::Num(n) => x86.push(X86::Mov(Val::Rax, Val::Imm(*n))),
        Expr::Add1(e1) => {
            compile(e1, x86);
            x86.push(X86::Add(Val::Rax, Val::Imm(1)))
        }
        Expr::Sub1(e1) => {
            compile(e1, x86);
            x86.push(X86::Sub(Val::Rax, Val::Imm(1)))
        }
        Expr::Negate(e1) => {
            compile(e1, x86);
            x86.push(X86::Neg(Val::Rax))
        }
    }
}

fn compile_expr(e: &Expr) -> String {
    let mut x86 = vec![];
    compile(e, &mut x86);
    let mut result = String::new();
    for instr in x86 {
        match instr {
            X86::Mov(Val::Rax, Val::Imm(n)) => result.push_str(&format!("\tmov rax, {}\n", n)),
            X86::Add(Val::Rax, Val::Imm(n)) => result.push_str(&format!("\tadd rax, {}\n", n)),
            X86::Sub(Val::Rax, Val::Imm(n)) => result.push_str(&format!("\tsub rax, {}\n", n)),
            X86::Neg(Val::Rax) => result.push_str(&format!("\tneg rax\n")),
            _ => panic!("Unsupport instr"),
        }
    }
    result
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let expr = parse_expr(&parse(&in_contents).unwrap());
    let result = compile_expr(&expr);
    let asm_program = format!(
        "section .text
global our_code_starts_here
our_code_starts_here:
{}\tret
",
        result
    );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}

#[cfg(test)]
mod tests {
    #[test]
    fn test1() {
        let expr1 = crate::Expr::Num(10);
        let result = crate::eval(&expr1);
        assert_eq!(result, 10);
    }
}
