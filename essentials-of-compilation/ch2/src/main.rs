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

fn main() {
    {
        let v = ch2::rvar_lang::var!("x");
        println!("v= {:?}", v);
    }
    {
        use ch2::rvar_lang::*;

        let p1 = program![r#let!([x add!(12, 20)]  add!(10, x))];
        println!("p1= {:?} ", p1);
        let v1 = interp_program(&p1);
        println!("v1= {:?} ", v1);

        let p1 = program![r#let!([x 2]  add!(r#let!([x 10]  x), x))];
        println!("p1= {:?} ", p1);
        let v1 = interp_program(&p1);
        println!("v1= {:?} ", v1);

        let p1 = program![r#let!(
            [x read!()]
            r#let!([y read!()]  add!(x, neg!(y)))
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

    {
        #[derive(Debug, Clone)]
        enum T {
            A(i32),
            B(String),
            Kaboom,
        }
        let t1a = T::A(42);
        let t1b = T::A(45);
        let t1c = T::A(45);
        let t2a = T::B("42_i32".to_string());
        let t2b = T::B("42_i64".to_string());
        let t2c = T::B("42_f32".to_string());
        let t3 = T::Kaboom;

        fn matchme(t: &T) {
            match t {
                T::A(42) => println!("matched T::A(42)"),
                T::A(45) => println!("matched T::A(45)"),
                T::A(n) => println!("matched T::A(n), n={}", n),
                T::B(s)
                    if {
                        let verify = |s| s == "42_i32";
                        verify(s)
                    } =>
                {
                    println!("matched T::B(\"42_i32\")")
                }
                T::B(s)
                    if {
                        let verify = |s| s == "42_i64";
                        verify(s)
                    } =>
                {
                    println!("matched T::B(\"42_i64\")")
                }
                T::B(s) => println!("matched T::B(s), s= {:?}", s),
                _ => println!("unhandled match "),
            }
        }

        matchme(&t1a);
        matchme(&t1b);
        matchme(&t1c);
        matchme(&t2a);
        matchme(&t2b);
        matchme(&t2c);
        matchme(&t3);
    }
}
