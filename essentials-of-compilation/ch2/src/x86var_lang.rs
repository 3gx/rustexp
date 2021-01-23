#[path = "macros.rs"]
mod macros;
#[allow(unused_imports)]
use macros::r#match;

#[path = "cvar_lang.rs"]
pub mod cvar_lang;
pub use cvar_lang::rvar_anf_lang::rvar_lang;

use cvar_lang as CVarLang;

#[allow(non_camel_case_types)]
pub enum Reg {
    rsp,
    rbp,
    rax,
    rbx,
    rcx,
    rdx,
    rsi,
    rdi,
    r8,
    r9,
    r10,
    r11,
    r12,
    r13,
    r14,
    r15,
}

type Int = i64;
pub enum Arg {
    Imm(Int),
    Reg(Reg),
    Deref(Reg, Int),
    Var(String),
}

type Label = String;
pub enum Inst {
    Addq(Arg, Arg),
    Subq(Arg, Arg),
    Movq(Arg, Arg),
    Negq(Arg, Arg),
    Callq(Label, Int),
    Retq,
    Pushq(Arg),
    Popq(Arg),
    Jmp(Label),
}

type Info = Vec<String>;
pub struct Block(Info, Vec<Inst>);

pub fn select_inst_atom(a: &CVarLang::Atom) -> Arg {
    match a {
        CVarLang::Atom::Int(n) => Arg::Imm(*n),
        CVarLang::Atom::Var(x) => Arg::Var(x.clone()),
    }
}

pub fn select_inst_expr(e: &CVarLang::Expr) -> Inst {
    use CVarLang::Expr;
    /*
    match e {
        Expr::Atom(a)
    }
    */
    unimplemented!()
}

pub fn select_inst_stmt(s: &CVarLang::Stmt) -> Inst {
    unimplemented!()
}

pub fn select_inst_tail(t: &CVarLang::Tail, block: &Block) -> Block {
    unimplemented!()
}
