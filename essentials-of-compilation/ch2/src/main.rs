#![allow(incomplete_features)]
#![feature(if_let_guard)]
#![feature(box_syntax)]
#![feature(let_chains)]
#![feature(decl_macro)]
#![feature(box_patterns)]

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
            D(Box<String>),
            C(Box<T>),
            Kaboom,
        }
        let t1a = T::A(42);
        let t1b = T::A(45);
        let t1c = T::A(45);
        let t2a = T::B("42_i32".to_string());
        let t2b = T::B("42_i64".to_string());
        let t2c = T::B("42_f32".to_string());
        let t3 = T::Kaboom;

        /*
        macro case($expr:expr) {
            $expr
        }
        */
        fn matchme(t: &T) {
            println!("++ matching: {:?}", t);
            match t {
                T::A(42) => println!("matched T::A(42)"),
                T::A(45) => println!("matched T::A(45)"),
                /*
                //case![T::A(n)] => println!("matched T::A(n), n={}", n),
                case![T::B(String("42_i32")) => println!("matched T::B(\"42_i32\")")],
                case![T::B(String("42_i64")) => println!("matched T::B(\"42_i64\")")],
                case![T::C(T::B("fun") => println!("matched T::C<T> box")],
                case![T::D(T::B("fun" => println!("matched T::D box")],
                case![T::C(ref T::B(s)) => println!("matched box with s={}", s)],
                */
                T::B(s) => println!("matched T::B(s), s= {:?}", s),
                _ => println!("unhandled match "),
            }

            /*
            T::C(_box) if {
                          }
            */

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
                /*
                T::C(s) if guard!(T::B(s) = &**s, "fun" = &s[..]) =>
                       println!("fun found"),
                rule![|T::B(s) = &**s, "fun" = &s[..] | {println!("fun_found")}],
                 expands to
                T::C(s) if {
                ( || {let T::B(s) = &**s {
                           if let "fun" = &s[..] {
                                   return true;
                               }
                           }
                      return false;})() =>
                       println!("fun found"),
                */
                T::C(s)
                    if {
                        let guard = || {
                            if let T::B(_s1) = &**s {
                                if let "fun" = &_s1[..] {
                                    return true;
                                }
                            }
                            return false;
                        };
                        guard()
                    } =>
                {
                    let rule = || {
                        if let T::B(_s1) = &**s {
                            if let "fun" = &_s1[..] {
                                return {
                                    println!("fun found\n");
                                };
                            }
                        }
                        panic!("unreachable")
                    };
                    rule()
                }
                /*
                T::C(s) if guard!(T::B(s) = &**s) =>
                       rule![|T::B(s) = &**s | {
                       println!("T::C(T::B(s)) with s = {}", s)}];
                expands to what is below
                */
                T::C(s)
                    if {
                        (|| {
                            if let T::B(_s1) = &**s {
                                return true;
                            }
                            return false;
                        })()
                    } =>
                {
                    (|| {
                        if let T::B(s) = &**s {
                            return {
                                println!("T::C(T::B(s)) with s = \"{}\"", s);
                            };
                        }
                        panic!("internal error")
                    })()
                }
                T::B(s) => println!("matched T::B(s), s= {:?}", s),
                T::C(box T::D(box s)) if "hun" == &s[..] => {
                    println!("hun matches with s = {:#?}", s)
                }
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
        let t = T::C(Box::new(T::B("gun".to_string())));
        matchme(&t);
        let t = T::C(Box::new(T::B("fun".to_string())));
        matchme(&t);
        let t = T::C(Box::new(T::D(box String::from("hun"))));
        matchme(&t);
    }
}