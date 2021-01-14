#![allow(incomplete_features)]
#![feature(if_let_guard)]
#![feature(box_syntax)]

pub mod x86int_lang {
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
    }

    type Label = String;
    pub enum Instr {
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

    type Info = Vec<i64>;
    pub struct Block(Info, Vec<Instr>);
}

pub mod cvar_lang {
    type Int = i64;
    type Label = String;
    type Var = String;
    type Info = Vec<i64>;

    pub enum Atm {
        Int(Int),
        Var(Var),
    }

    pub enum Prim {
        Read,
        Neg(Atm),
        Add(Atm, Atm),
    }

    pub enum Exp {
        Atm,
        Read,
        Prim,
    }

    pub enum Stmt {
        Assign(Var, Exp),
    }

    pub enum Tail {
        Return(Exp),
        Seq(Stmt, Box<Tail>),
    }

    pub enum CVar {
        CProgram(Info, Vec<(Label, Tail)>),
    }
}

fn main() {
    {
        use ch2::rvar_lang::*;
        use ch2::*;

        let p1 = program![r#let!([x <- plus!(12, 20)]  plus!(10, x))];
        println!("p1= {:?} ", p1);
        let v1 = interp_program(&p1);
        println!("v1= {:?} ", v1);

        let p1 = program![r#let!([x <- 2]  plus!(r#let!([x <- 10]  x), x))];
        println!("p1= {:?} ", p1);
        let v1 = interp_program(&p1);
        println!("v1= {:?} ", v1);

        let p1 = program![r#let!(
            [x <- read!()]
            r#let!([y <- read!()]  plus!(x, neg!(y)))
        )];
        println!("p1= {:?} ", p1);
        let t = true;
        let t = if t { false } else { true };
        if t {
            println!("inter 52<enter>, 10<enter>, should get 42");
            let v1 = interp_program(&p1);
            println!("v1= {:?} ", v1);
        }
    }
}
