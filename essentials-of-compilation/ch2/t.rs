    Checking ch2 v0.1.0 (/home/egaburov/work/rust/rustexpt.git/essentials-of-compilation/ch2)
error: expected identifier, found keyword `let`
   --> ch2/src/main.rs:424:30
    |
424 |                 T::C(s) if @{let T::B(s) = &**s, let "fun" = &s[..]} =>
    |                              ^^^ expected identifier, found keyword
error: expected identifier, found keyword `let`
   --> ch2/src/main.rs:432:31
    |
432 |                 T::C(td) if @{let T::D(s) = &**td, "dun" == &s[..]} => {
    |                               ^^^ expected identifier, found keyword
error: expected identifier, found keyword `let`
   --> ch2/src/main.rs:451:30
    |
451 |                 T::C(s) if @{let T::B(s) = &**s, "fun" == &s[..]} =>
    |                              ^^^ expected identifier, found keyword
error: expected identifier, found keyword `let`
   --> ch2/src/main.rs:459:31
    |
459 |                 T::C(td) if @{let T::D(s) = &**td, "dun" == &s[..]} => {
    |                               ^^^ expected identifier, found keyword
error: could not compile `ch2`
To learn more, run the command again with --verbose.

#![feature(prelude_import)]
#![allow(incomplete_features)]
#![feature(if_let_guard)]
#![feature(box_syntax)]
#![feature(let_chains)]
#![feature(decl_macro)]
#![feature(box_patterns)]
#[prelude_import]
use std::prelude::v1::*;
#[macro_use]
extern crate std;
macro r#match { ((@ cases) (@ obj $ ($ obj : tt) *) (@ rules $ ($ rules : tt) *)) => { match $ ($ obj) * { $ ($ rules) * } } , (@ allow_unused $ ($ tt : tt) *) => { # [allow (unused_variables)] $ ($ tt) * } , (@ carbon_copy $ ($ tt : tt) *) => { $ ($ tt) * } , (@ guard $ cb : ident $ ret : expr , let $ pat : pat = $ expr : expr $ (,) ?) => { r#match ! (@ $ cb if let $ pat = $ expr { return $ ret }) } , (@ guard $ cb : ident $ ret : expr , $ pat : pat = $ expr : expr $ (,) ?) => { r#match ! (@ $ cb if let $ pat = $ expr { return $ ret }) } , (@ guard $ cb : ident $ ret : expr , $ guard : expr $ (,) ?) => { # [allow (unreachable_code)] if $ guard { return $ ret } } , (@ guard $ cb : ident $ ret : expr , let $ pat : pat = $ expr : expr , $ ($ tail : tt) *) => { r#match ! (@ $ cb if let $ pat = $ expr { r#match ! (@ guard $ cb $ ret , $ ($ tail) *) }) } , (@ guard $ cb : ident $ ret : expr , $ guard : expr , $ ($ tail : tt) *) => { if $ guard { r#match ! (@ guard $ cb $ ret , $ ($ tail) *) } } , ((@ cases $ pat : pat $ (if @ { $ ($ guard : tt) * }) ? => $ result : expr , $ ($ tail : tt) *) (@ obj $ ($ obj : tt) *) (@ rules $ ($ rules : tt) *)) => { r#match ! ((@ cases $ ($ tail) *) (@ obj $ ($ obj) *) (@ rules $ ($ rules) * $ pat $ (if (|| { r#match ! (@ guard allow_unused true , $ ($ guard) *) ; return false ; }) ()) ? => (|| { r#match ! (@ guard carbon_copy $ result , true , $ ($ ($ guard) *) ?) ; panic ! ("unreacheable") ; }) () ,)) } , ((@ cases $ pat : pat $ (if @ { $ ($ guard : tt) * }) ? => $ result : expr) (@ obj $ ($ obj : tt) *) (@ rules $ ($ rules : tt) *)) => { r#match ! ((@ cases $ pat $ (if @ { $ ($ guard) * }) ? => $ result ,) (@ obj $ ($ obj) *) (@ rules $ ($ rules) *)) } , ((@ cases $ pat : pat $ (if $ guard : expr) ? => $ result : expr , $ ($ tail : tt) *) (@ obj $ ($ obj : tt) *) (@ rules $ ($ rules : tt) *)) => { r#match ! ((@ cases $ ($ tail) *) (@ obj $ ($ obj) *) (@ rules $ ($ rules) * $ pat $ (if $ guard) ? => $ result ,)) } , ((@ cases $ pat : pat $ (if $ guard : expr) ? => $ result : expr) (@ obj $ ($ obj : tt) *) (@ rules $ ($ rules : tt) *)) => { r#match ! ((@ cases $ pat $ (if $ guard) ? => $ result ,) (@ obj $ ($ obj) *) (@ rules $ ($ rules) *)) } , ([$ obj : expr] $ ($ tail : tt) *) => { r#match ! ((@ cases $ ($ tail) *) (@ obj $ obj) (@ rules)) } }
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
        let v = Term::Var("x".to_string());
        {
            ::std::io::_print(::core::fmt::Arguments::new_v1(
                &["v= ", "\n"],
                &match (&v,) {
                    (arg0,) => [::core::fmt::ArgumentV1::new(arg0, ::core::fmt::Debug::fmt)],
                },
            ));
        };
    }
    {
        use ch2::rvar_lang::*;
        let p1 = Program(
            ::alloc::vec::Vec::new(),
            Term::Let(
                "x".to_owned(),
                Box::new(Term::Add(box 12.into_term(), box 20.into_term()).into_term()),
                Box::new(Term::Add(box 10.into_term(), box Term::Var("x".to_string())).into_term()),
            )
            .into_term(),
        );
        {
            ::std::io::_print(::core::fmt::Arguments::new_v1(
                &["p1= ", " \n"],
                &match (&p1,) {
                    (arg0,) => [::core::fmt::ArgumentV1::new(arg0, ::core::fmt::Debug::fmt)],
                },
            ));
        };
        let v1 = interp_program(&p1);
        {
            ::std::io::_print(::core::fmt::Arguments::new_v1(
                &["v1= ", " \n"],
                &match (&v1,) {
                    (arg0,) => [::core::fmt::ArgumentV1::new(arg0, ::core::fmt::Debug::fmt)],
                },
            ));
        };
        let p1 = Program(
            ::alloc::vec::Vec::new(),
            Term::Let(
                "x".to_owned(),
                Box::new(2.into_term()),
                Box::new(
                    Term::Add(
                        box Term::Let(
                            "x".to_owned(),
                            Box::new(10.into_term()),
                            Box::new(Term::Var("x".to_string())),
                        )
                        .into_term(),
                        box Term::Var("x".to_string()),
                    )
                    .into_term(),
                ),
            )
            .into_term(),
        );
        {
            ::std::io::_print(::core::fmt::Arguments::new_v1(
                &["p1= ", " \n"],
                &match (&p1,) {
                    (arg0,) => [::core::fmt::ArgumentV1::new(arg0, ::core::fmt::Debug::fmt)],
                },
            ));
        };
        let v1 = interp_program(&p1);
        {
            ::std::io::_print(::core::fmt::Arguments::new_v1(
                &["v1= ", " \n"],
                &match (&v1,) {
                    (arg0,) => [::core::fmt::ArgumentV1::new(arg0, ::core::fmt::Debug::fmt)],
                },
            ));
        };
        let p1 = Program(
            ::alloc::vec::Vec::new(),
            Term::Let(
                "x".to_owned(),
                Box::new(Term::Read.into_term()),
                Box::new(
                    Term::Let(
                        "y".to_owned(),
                        Box::new(Term::Read.into_term()),
                        Box::new(
                            Term::Add(
                                box Term::Var("x".to_string()),
                                box Term::Neg(box (box Term::Var("y".to_string())).into_term())
                                    .into_term(),
                            )
                            .into_term(),
                        ),
                    )
                    .into_term(),
                ),
            )
            .into_term(),
        );
        {
            ::std::io::_print(::core::fmt::Arguments::new_v1(
                &["p1= ", " \n"],
                &match (&p1,) {
                    (arg0,) => [::core::fmt::ArgumentV1::new(arg0, ::core::fmt::Debug::fmt)],
                },
            ));
        };
        let t = true;
        let t = if t { false } else { true };
        if t {
            {
                ::std::io::_print(::core::fmt::Arguments::new_v1(
                    &["inter 52<enter>, 10<enter>, should get 42\n"],
                    &match () {
                        () => [],
                    },
                ));
            };
            let v1 = interp_program(&p1);
            {
                ::std::io::_print(::core::fmt::Arguments::new_v1(
                    &["v1= ", " \n"],
                    &match (&v1,) {
                        (arg0,) => [::core::fmt::ArgumentV1::new(arg0, ::core::fmt::Debug::fmt)],
                    },
                ));
            };
        }
    }
    {
        enum T {
            A(i32),
            B(String),
            D(Box<String>),
            C(Box<T>),
            Kaboom,
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::fmt::Debug for T {
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                match (&*self,) {
                    (&T::A(ref __self_0),) => {
                        let mut debug_trait_builder = f.debug_tuple("A");
                        let _ = debug_trait_builder.field(&&(*__self_0));
                        debug_trait_builder.finish()
                    }
                    (&T::B(ref __self_0),) => {
                        let mut debug_trait_builder = f.debug_tuple("B");
                        let _ = debug_trait_builder.field(&&(*__self_0));
                        debug_trait_builder.finish()
                    }
                    (&T::D(ref __self_0),) => {
                        let mut debug_trait_builder = f.debug_tuple("D");
                        let _ = debug_trait_builder.field(&&(*__self_0));
                        debug_trait_builder.finish()
                    }
                    (&T::C(ref __self_0),) => {
                        let mut debug_trait_builder = f.debug_tuple("C");
                        let _ = debug_trait_builder.field(&&(*__self_0));
                        debug_trait_builder.finish()
                    }
                    (&T::Kaboom,) => {
                        let mut debug_trait_builder = f.debug_tuple("Kaboom");
                        debug_trait_builder.finish()
                    }
                }
            }
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::clone::Clone for T {
            #[inline]
            fn clone(&self) -> T {
                match (&*self,) {
                    (&T::A(ref __self_0),) => T::A(::core::clone::Clone::clone(&(*__self_0))),
                    (&T::B(ref __self_0),) => T::B(::core::clone::Clone::clone(&(*__self_0))),
                    (&T::D(ref __self_0),) => T::D(::core::clone::Clone::clone(&(*__self_0))),
                    (&T::C(ref __self_0),) => T::C(::core::clone::Clone::clone(&(*__self_0))),
                    (&T::Kaboom,) => T::Kaboom,
                }
            }
        }
        let t1a = T::A(42);
        let t1b = T::A(45);
        let t1c = T::A(45);
        let t2a = T::B("42_i32".to_string());
        let t2b = T::B("42_i64".to_string());
        let t2c = T::B("42_f32".to_string());
        let t3 = T::Kaboom;
        fn matchme(t: &T) {
            {
                ::std::io::_print(::core::fmt::Arguments::new_v1(
                    &["++ matching: ", "\n"],
                    &match (&t,) {
                        (arg0,) => [::core::fmt::ArgumentV1::new(arg0, ::core::fmt::Debug::fmt)],
                    },
                ));
            };
            match t {
                T::A(42) => {
                    ::std::io::_print(::core::fmt::Arguments::new_v1(
                        &["matched T::A(42)\n"],
                        &match () {
                            () => [],
                        },
                    ));
                }
                T::A(45) => {
                    ::std::io::_print(::core::fmt::Arguments::new_v1(
                        &["matched T::A(45)\n"],
                        &match () {
                            () => [],
                        },
                    ));
                }
                T::B(s) => {
                    ::std::io::_print(::core::fmt::Arguments::new_v1(
                        &["matched T::B(s), s= ", "\n"],
                        &match (&s,) {
                            (arg0,) => {
                                [::core::fmt::ArgumentV1::new(arg0, ::core::fmt::Debug::fmt)]
                            }
                        },
                    ));
                }
                _ => {
                    ::std::io::_print(::core::fmt::Arguments::new_v1(
                        &["unhandled match \n"],
                        &match () {
                            () => [],
                        },
                    ));
                }
            }
            match t {
                T::A(42) => {
                    ::std::io::_print(::core::fmt::Arguments::new_v1(
                        &["matched T::A(42)\n"],
                        &match () {
                            () => [],
                        },
                    ));
                }
                T::A(45) => {
                    ::std::io::_print(::core::fmt::Arguments::new_v1(
                        &["matched T::A(45)\n"],
                        &match () {
                            () => [],
                        },
                    ));
                }
                T::A(n) => {
                    ::std::io::_print(::core::fmt::Arguments::new_v1(
                        &["matched T::A(n), n=", "\n"],
                        &match (&n,) {
                            (arg0,) => [::core::fmt::ArgumentV1::new(
                                arg0,
                                ::core::fmt::Display::fmt,
                            )],
                        },
                    ));
                }
                T::B(s)
                    if {
                        let verify = |s| s == "42_i32";
                        verify(s)
                    } =>
                {
                    ::std::io::_print(::core::fmt::Arguments::new_v1(
                        &["matched T::B(\"42_i32\")\n"],
                        &match () {
                            () => [],
                        },
                    ));
                }
                T::B(s)
                    if {
                        let verify = |s| s == "42_i64";
                        verify(s)
                    } =>
                {
                    ::std::io::_print(::core::fmt::Arguments::new_v1(
                        &["matched T::B(\"42_i64\")\n"],
                        &match () {
                            () => [],
                        },
                    ));
                }
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
                                    {
                                        ::std::io::_print(::core::fmt::Arguments::new_v1(
                                            &["fun found\n\n"],
                                            &match () {
                                                () => [],
                                            },
                                        ));
                                    };
                                };
                            }
                        }
                        {
                            ::std::rt::begin_panic("unreachable")
                        }
                    };
                    rule()
                }
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
                                {
                                    ::std::io::_print(::core::fmt::Arguments::new_v1(
                                        &["T::C(T::B(s)) with s = \"", "\"\n"],
                                        &match (&s,) {
                                            (arg0,) => [::core::fmt::ArgumentV1::new(
                                                arg0,
                                                ::core::fmt::Display::fmt,
                                            )],
                                        },
                                    ));
                                };
                            };
                        }
                        {
                            ::std::rt::begin_panic("internal error")
                        }
                    })()
                }
                T::B(s) => {
                    ::std::io::_print(::core::fmt::Arguments::new_v1(
                        &["matched T::B(s), s= ", "\n"],
                        &match (&s,) {
                            (arg0,) => {
                                [::core::fmt::ArgumentV1::new(arg0, ::core::fmt::Debug::fmt)]
                            }
                        },
                    ));
                }
                T::C(box T::D(box s)) if "hun" == &s[..] => {
                    ::std::io::_print(::core::fmt::Arguments::new_v1_formatted(
                        &["hun matches with s = ", "\n"],
                        &match (&s,) {
                            (arg0,) => {
                                [::core::fmt::ArgumentV1::new(arg0, ::core::fmt::Debug::fmt)]
                            }
                        },
                        &[::core::fmt::rt::v1::Argument {
                            position: 0usize,
                            format: ::core::fmt::rt::v1::FormatSpec {
                                fill: ' ',
                                align: ::core::fmt::rt::v1::Alignment::Unknown,
                                flags: 4u32,
                                precision: ::core::fmt::rt::v1::Count::Implied,
                                width: ::core::fmt::rt::v1::Count::Implied,
                            },
                        }],
                    ));
                }
                _ => {
                    ::std::io::_print(::core::fmt::Arguments::new_v1(
                        &["unhandled match \n"],
                        &match () {
                            () => [],
                        },
                    ));
                }
            }
            match t {
                T::A(42) => (|| {
                    #[allow(unreachable_code)]
                    if true {
                        return {
                            ::std::io::_print(::core::fmt::Arguments::new_v1(
                                &["matched T::A(42)\n"],
                                &match () {
                                    () => [],
                                },
                            ));
                        };
                    };
                    {
                        ::std::rt::begin_panic("unreacheable")
                    };
                })(),
                T::A(45) => (|| {
                    #[allow(unreachable_code)]
                    if true {
                        return {
                            ::std::io::_print(::core::fmt::Arguments::new_v1(
                                &["matched T::A(45)\n"],
                                &match () {
                                    () => [],
                                },
                            ));
                        };
                    };
                    {
                        ::std::rt::begin_panic("unreacheable")
                    };
                })(),
                T::A(n) => (|| {
                    #[allow(unreachable_code)]
                    if true {
                        return {
                            ::std::io::_print(::core::fmt::Arguments::new_v1(
                                &["matched T::A(n), n=", "\n"],
                                &match (&n,) {
                                    (arg0,) => [::core::fmt::ArgumentV1::new(
                                        arg0,
                                        ::core::fmt::Display::fmt,
                                    )],
                                },
                            ));
                        };
                    };
                    {
                        ::std::rt::begin_panic("unreacheable")
                    };
                })(),
                T::B(s) if { s == "42_i32" } => {
                    ::std::io::_print(::core::fmt::Arguments::new_v1(
                        &["matched T::B(\"42_i32\")\n"],
                        &match () {
                            () => [],
                        },
                    ));
                }
                T::B(s) if { s == "42_i64" } => {
                    ::std::io::_print(::core::fmt::Arguments::new_v1(
                        &["matched T::B(\"42_i64\")\n"],
                        &match () {
                            () => [],
                        },
                    ));
                }
                T::C(s)
                    if (|| {
                        #[allow(unused_variables)]
                        if let T::B(s) = &**s {
                            #[allow(unused_variables)]
                            if let "fun" = &s[..] {
                                return true;
                            }
                        };
                        return false;
                    })() =>
                {
                    (|| {
                        if true {
                            if let T::B(s) = &**s {
                                if let "fun" = &s[..] {
                                    return {
                                        ::std::io::_print(::core::fmt::Arguments::new_v1(
                                            &["fun found\n\n"],
                                            &match () {
                                                () => [],
                                            },
                                        ));
                                    };
                                }
                            }
                        };
                        {
                            ::std::rt::begin_panic("unreacheable")
                        };
                    })()
                }
                T::C(s)
                    if (|| {
                        #[allow(unused_variables)]
                        if let T::B(s) = &**s {
                            return true;
                        };
                        return false;
                    })() =>
                {
                    (|| {
                        if true {
                            if let T::B(s) = &**s {
                                return {
                                    ::std::io::_print(::core::fmt::Arguments::new_v1(
                                        &["xT::C(T::B(s)) with s = \"", "\"\n"],
                                        &match (&s,) {
                                            (arg0,) => [::core::fmt::ArgumentV1::new(
                                                arg0,
                                                ::core::fmt::Display::fmt,
                                            )],
                                        },
                                    ));
                                };
                            }
                        };
                        {
                            ::std::rt::begin_panic("unreacheable")
                        };
                    })()
                }
                T::B(s) => (|| {
                    #[allow(unreachable_code)]
                    if true {
                        return {
                            ::std::io::_print(::core::fmt::Arguments::new_v1(
                                &["matched T::B(s), s= ", "\n"],
                                &match (&s,) {
                                    (arg0,) => [::core::fmt::ArgumentV1::new(
                                        arg0,
                                        ::core::fmt::Debug::fmt,
                                    )],
                                },
                            ));
                        };
                    };
                    {
                        ::std::rt::begin_panic("unreacheable")
                    };
                })(),
                T::C(box T::D(box s)) if { "hun" == &s[..] } => {
                    ::std::io::_print(::core::fmt::Arguments::new_v1_formatted(
                        &["hun matches with s = ", "\n"],
                        &match (&s,) {
                            (arg0,) => {
                                [::core::fmt::ArgumentV1::new(arg0, ::core::fmt::Debug::fmt)]
                            }
                        },
                        &[::core::fmt::rt::v1::Argument {
                            position: 0usize,
                            format: ::core::fmt::rt::v1::FormatSpec {
                                fill: ' ',
                                align: ::core::fmt::rt::v1::Alignment::Unknown,
                                flags: 4u32,
                                precision: ::core::fmt::rt::v1::Count::Implied,
                                width: ::core::fmt::rt::v1::Count::Implied,
                            },
                        }],
                    ));
                }
                T::C(td)
                    if (|| {
                        #[allow(unused_variables)]
                        if let T::D(s) = &**td {
                            #[allow(unreachable_code)]
                            if "dun" == &s[..] {
                                return true;
                            }
                        };
                        return false;
                    })() =>
                {
                    (|| {
                        if true {
                            if let T::D(s) = &**td {
                                #[allow(unreachable_code)]
                                if "dun" == &s[..] {
                                    return {
                                        {
                                            :: std :: io :: _print (:: core :: fmt :: Arguments :: new_v1_formatted (& ["dun matches with s = " , "\n"] , & match (& s ,) { (arg0 ,) => [:: core :: fmt :: ArgumentV1 :: new (arg0 , :: core :: fmt :: Debug :: fmt)] , } , & [:: core :: fmt :: rt :: v1 :: Argument { position : 0usize , format : :: core :: fmt :: rt :: v1 :: FormatSpec { fill : ' ' , align : :: core :: fmt :: rt :: v1 :: Alignment :: Unknown , flags : 4u32 , precision : :: core :: fmt :: rt :: v1 :: Count :: Implied , width : :: core :: fmt :: rt :: v1 :: Count :: Implied , } , }])) ;
                                        }
                                    };
                                }
                            }
                        };
                        {
                            ::std::rt::begin_panic("unreacheable")
                        };
                    })()
                }
                T::C(td)
                    if (|| {
                        #[allow(unused_variables)]
                        if let T::D(s) = &**td {
                            return true;
                        };
                        return false;
                    })() =>
                {
                    (|| {
                        if true {
                            if let T::D(s) = &**td {
                                return {
                                    {
                                        ::std::io::_print(
                                            ::core::fmt::Arguments::new_v1_formatted(
                                                &["some matches with s = ", "\n"],
                                                &match (&s,) {
                                                    (arg0,) => [::core::fmt::ArgumentV1::new(
                                                        arg0,
                                                        ::core::fmt::Debug::fmt,
                                                    )],
                                                },
                                                &[::core::fmt::rt::v1::Argument {
                                                    position: 0usize,
                                                    format: ::core::fmt::rt::v1::FormatSpec {
                                                        fill: ' ',
                                                        align:
                                                            ::core::fmt::rt::v1::Alignment::Unknown,
                                                        flags: 4u32,
                                                        precision:
                                                            ::core::fmt::rt::v1::Count::Implied,
                                                        width: ::core::fmt::rt::v1::Count::Implied,
                                                    },
                                                }],
                                            ),
                                        );
                                    }
                                };
                            }
                        };
                        {
                            ::std::rt::begin_panic("unreacheable")
                        };
                    })()
                }
                _ => (|| {
                    #[allow(unreachable_code)]
                    if true {
                        return {
                            ::std::io::_print(::core::fmt::Arguments::new_v1(
                                &["unhandled match \n"],
                                &match () {
                                    () => [],
                                },
                            ));
                        };
                    };
                    {
                        ::std::rt::begin_panic("unreacheable")
                    };
                })(),
            }
            match t {
                T::A(42) => (|| {
                    #[allow(unreachable_code)]
                    if true {
                        return {
                            ::std::io::_print(::core::fmt::Arguments::new_v1(
                                &["matched T::A(42)\n"],
                                &match () {
                                    () => [],
                                },
                            ));
                        };
                    };
                    {
                        ::std::rt::begin_panic("unreacheable")
                    };
                })(),
                T::A(45) => (|| {
                    #[allow(unreachable_code)]
                    if true {
                        return {
                            ::std::io::_print(::core::fmt::Arguments::new_v1(
                                &["matched T::A(45)\n"],
                                &match () {
                                    () => [],
                                },
                            ));
                        };
                    };
                    {
                        ::std::rt::begin_panic("unreacheable")
                    };
                })(),
                T::A(n) => (|| {
                    #[allow(unreachable_code)]
                    if true {
                        return {
                            ::std::io::_print(::core::fmt::Arguments::new_v1(
                                &["matched T::A(n), n=", "\n"],
                                &match (&n,) {
                                    (arg0,) => [::core::fmt::ArgumentV1::new(
                                        arg0,
                                        ::core::fmt::Display::fmt,
                                    )],
                                },
                            ));
                        };
                    };
                    {
                        ::std::rt::begin_panic("unreacheable")
                    };
                })(),
                T::B(s) if { s == "42_i32" } => {
                    ::std::io::_print(::core::fmt::Arguments::new_v1(
                        &["matched T::B(\"42_i32\")\n"],
                        &match () {
                            () => [],
                        },
                    ));
                }
                T::B(s) if { s == "42_i64" } => {
                    ::std::io::_print(::core::fmt::Arguments::new_v1(
                        &["matched T::B(\"42_i64\")\n"],
                        &match () {
                            () => [],
                        },
                    ));
                }
                T::C(s)
                    if (|| {
                        #[allow(unused_variables)]
                        if let T::B(s) = &**s {
                            #[allow(unreachable_code)]
                            if "fun" == &s[..] {
                                return true;
                            }
                        };
                        return false;
                    })() =>
                {
                    (|| {
                        if true {
                            if let T::B(s) = &**s {
                                #[allow(unreachable_code)]
                                if "fun" == &s[..] {
                                    return {
                                        ::std::io::_print(::core::fmt::Arguments::new_v1(
                                            &["fun found\n\n"],
                                            &match () {
                                                () => [],
                                            },
                                        ));
                                    };
                                }
                            }
                        };
                        {
                            ::std::rt::begin_panic("unreacheable")
                        };
                    })()
                }
                T::C(s)
                    if (|| {
                        #[allow(unused_variables)]
                        if let T::B(s) = &**s {
                            return true;
                        };
                        return false;
                    })() =>
                {
                    (|| {
                        if true {
                            if let T::B(s) = &**s {
                                return {
                                    ::std::io::_print(::core::fmt::Arguments::new_v1(
                                        &["xT::C(T::B(s)) with s = \"", "\"\n"],
                                        &match (&s,) {
                                            (arg0,) => [::core::fmt::ArgumentV1::new(
                                                arg0,
                                                ::core::fmt::Display::fmt,
                                            )],
                                        },
                                    ));
                                };
                            }
                        };
                        {
                            ::std::rt::begin_panic("unreacheable")
                        };
                    })()
                }
                T::B(s) => (|| {
                    #[allow(unreachable_code)]
                    if true {
                        return {
                            ::std::io::_print(::core::fmt::Arguments::new_v1(
                                &["matched T::B(s), s= ", "\n"],
                                &match (&s,) {
                                    (arg0,) => [::core::fmt::ArgumentV1::new(
                                        arg0,
                                        ::core::fmt::Debug::fmt,
                                    )],
                                },
                            ));
                        };
                    };
                    {
                        ::std::rt::begin_panic("unreacheable")
                    };
                })(),
                T::C(box T::D(box s)) if { "hun" == &s[..] } => {
                    ::std::io::_print(::core::fmt::Arguments::new_v1_formatted(
                        &["hun matches with s = ", "\n"],
                        &match (&s,) {
                            (arg0,) => {
                                [::core::fmt::ArgumentV1::new(arg0, ::core::fmt::Debug::fmt)]
                            }
                        },
                        &[::core::fmt::rt::v1::Argument {
                            position: 0usize,
                            format: ::core::fmt::rt::v1::FormatSpec {
                                fill: ' ',
                                align: ::core::fmt::rt::v1::Alignment::Unknown,
                                flags: 4u32,
                                precision: ::core::fmt::rt::v1::Count::Implied,
                                width: ::core::fmt::rt::v1::Count::Implied,
                            },
                        }],
                    ));
                }
                T::C(td)
                    if (|| {
                        #[allow(unused_variables)]
                        if let T::D(s) = &**td {
                            #[allow(unreachable_code)]
                            if "dun" == &s[..] {
                                return true;
                            }
                        };
                        return false;
                    })() =>
                {
                    (|| {
                        if true {
                            if let T::D(s) = &**td {
                                #[allow(unreachable_code)]
                                if "dun" == &s[..] {
                                    return {
                                        {
                                            :: std :: io :: _print (:: core :: fmt :: Arguments :: new_v1_formatted (& ["dun matches with s = " , "\n"] , & match (& s ,) { (arg0 ,) => [:: core :: fmt :: ArgumentV1 :: new (arg0 , :: core :: fmt :: Debug :: fmt)] , } , & [:: core :: fmt :: rt :: v1 :: Argument { position : 0usize , format : :: core :: fmt :: rt :: v1 :: FormatSpec { fill : ' ' , align : :: core :: fmt :: rt :: v1 :: Alignment :: Unknown , flags : 4u32 , precision : :: core :: fmt :: rt :: v1 :: Count :: Implied , width : :: core :: fmt :: rt :: v1 :: Count :: Implied , } , }])) ;
                                        }
                                    };
                                }
                            }
                        };
                        {
                            ::std::rt::begin_panic("unreacheable")
                        };
                    })()
                }
                T::C(td)
                    if (|| {
                        #[allow(unused_variables)]
                        if let T::D(s) = &**td {
                            return true;
                        };
                        return false;
                    })() =>
                {
                    (|| {
                        if true {
                            if let T::D(s) = &**td {
                                return {
                                    {
                                        ::std::io::_print(
                                            ::core::fmt::Arguments::new_v1_formatted(
                                                &["some matches with s = ", "\n"],
                                                &match (&s,) {
                                                    (arg0,) => [::core::fmt::ArgumentV1::new(
                                                        arg0,
                                                        ::core::fmt::Debug::fmt,
                                                    )],
                                                },
                                                &[::core::fmt::rt::v1::Argument {
                                                    position: 0usize,
                                                    format: ::core::fmt::rt::v1::FormatSpec {
                                                        fill: ' ',
                                                        align:
                                                            ::core::fmt::rt::v1::Alignment::Unknown,
                                                        flags: 4u32,
                                                        precision:
                                                            ::core::fmt::rt::v1::Count::Implied,
                                                        width: ::core::fmt::rt::v1::Count::Implied,
                                                    },
                                                }],
                                            ),
                                        );
                                    }
                                };
                            }
                        };
                        {
                            ::std::rt::begin_panic("unreacheable")
                        };
                    })()
                }
                _ => (|| {
                    #[allow(unreachable_code)]
                    if true {
                        return {
                            ::std::io::_print(::core::fmt::Arguments::new_v1(
                                &["unhandled match \n"],
                                &match () {
                                    () => [],
                                },
                            ));
                        };
                    };
                    {
                        ::std::rt::begin_panic("unreacheable")
                    };
                })(),
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
        let t = T::C(Box::new(T::D(box String::from("dun"))));
        matchme(&t);
        let t = T::C(Box::new(T::D(box String::from("arun"))));
        matchme(&t);
        {
            enum OpCode {
                Neg,
                Add,
            }
            #[automatically_derived]
            #[allow(unused_qualifications)]
            impl ::core::fmt::Debug for OpCode {
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    match (&*self,) {
                        (&OpCode::Neg,) => {
                            let mut debug_trait_builder = f.debug_tuple("Neg");
                            debug_trait_builder.finish()
                        }
                        (&OpCode::Add,) => {
                            let mut debug_trait_builder = f.debug_tuple("Add");
                            debug_trait_builder.finish()
                        }
                    }
                }
            }
            #[automatically_derived]
            #[allow(unused_qualifications)]
            impl ::core::clone::Clone for OpCode {
                #[inline]
                fn clone(&self) -> OpCode {
                    match (&*self,) {
                        (&OpCode::Neg,) => OpCode::Neg,
                        (&OpCode::Add,) => OpCode::Add,
                    }
                }
            }
            enum Expr {
                Int(i64),
                Prim(OpCode, Vec<Expr>),
            }
            #[automatically_derived]
            #[allow(unused_qualifications)]
            impl ::core::fmt::Debug for Expr {
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    match (&*self,) {
                        (&Expr::Int(ref __self_0),) => {
                            let mut debug_trait_builder = f.debug_tuple("Int");
                            let _ = debug_trait_builder.field(&&(*__self_0));
                            debug_trait_builder.finish()
                        }
                        (&Expr::Prim(ref __self_0, ref __self_1),) => {
                            let mut debug_trait_builder = f.debug_tuple("Prim");
                            let _ = debug_trait_builder.field(&&(*__self_0));
                            let _ = debug_trait_builder.field(&&(*__self_1));
                            debug_trait_builder.finish()
                        }
                    }
                }
            }
            #[automatically_derived]
            #[allow(unused_qualifications)]
            impl ::core::clone::Clone for Expr {
                #[inline]
                fn clone(&self) -> Expr {
                    match (&*self,) {
                        (&Expr::Int(ref __self_0),) => {
                            Expr::Int(::core::clone::Clone::clone(&(*__self_0)))
                        }
                        (&Expr::Prim(ref __self_0, ref __self_1),) => Expr::Prim(
                            ::core::clone::Clone::clone(&(*__self_0)),
                            ::core::clone::Clone::clone(&(*__self_1)),
                        ),
                    }
                }
            }
            use Expr::*;
            use OpCode::*;
            let e0 = Int(32);
            let e1 = Prim(Neg, <[_]>::into_vec(box [Int(32)]));
            let e1a = Prim(
                Neg,
                <[_]>::into_vec(box [Prim(Add, <[_]>::into_vec(box [Int(10), Int(32)]))]),
            );
            let e2 = Prim(
                Add,
                <[_]>::into_vec(box [
                    Prim(
                        Add,
                        <[_]>::into_vec(box [Int(32), Prim(Neg, <[_]>::into_vec(box [Int(44)]))]),
                    ),
                    Int(42),
                ]),
            );
            {
                ::std::io::_print(::core::fmt::Arguments::new_v1(
                    &["e0= ", "\n"],
                    &match (&e0,) {
                        (arg0,) => [::core::fmt::ArgumentV1::new(arg0, ::core::fmt::Debug::fmt)],
                    },
                ));
            };
            {
                ::std::io::_print(::core::fmt::Arguments::new_v1(
                    &["e1= ", "\n"],
                    &match (&e1,) {
                        (arg0,) => [::core::fmt::ArgumentV1::new(arg0, ::core::fmt::Debug::fmt)],
                    },
                ));
            };
            {
                ::std::io::_print(::core::fmt::Arguments::new_v1(
                    &["e1a= ", "\n"],
                    &match (&e1a,) {
                        (arg0,) => [::core::fmt::ArgumentV1::new(arg0, ::core::fmt::Debug::fmt)],
                    },
                ));
            };
            {
                ::std::io::_print(::core::fmt::Arguments::new_v1(
                    &["e2= ", "\n"],
                    &match (&e2,) {
                        (arg0,) => [::core::fmt::ArgumentV1::new(arg0, ::core::fmt::Debug::fmt)],
                    },
                ));
            };
            fn eval(e: &Expr) -> i64 {
                match e {
                    Int(n) => (|| {
                        #[allow(unreachable_code)]
                        if true {
                            return *n;
                        };
                        {
                            ::std::rt::begin_panic("unreacheable")
                        };
                    })(),
                    Prim(Neg, v)
                        if (|| {
                            #[allow(unused_variables)]
                            if let [Int(m)] = v.as_slice() {
                                return true;
                            };
                            return false;
                        })() =>
                    {
                        (|| {
                            if true {
                                if let [Int(m)] = v.as_slice() {
                                    return -*m;
                                }
                            };
                            {
                                ::std::rt::begin_panic("unreacheable")
                            };
                        })()
                    }
                    Prim(Neg, v)
                        if (|| {
                            #[allow(unused_variables)]
                            if let [e] = &v[..] {
                                return true;
                            };
                            return false;
                        })() =>
                    {
                        (|| {
                            if true {
                                if let [e] = &v[..] {
                                    return -eval(e);
                                }
                            };
                            {
                                ::std::rt::begin_panic("unreacheable")
                            };
                        })()
                    }
                    Prim(Add, v)
                        if (|| {
                            #[allow(unused_variables)]
                            if let [e1, e2] = &v[..] {
                                return true;
                            };
                            return false;
                        })() =>
                    {
                        (|| {
                            if true {
                                if let [e1, e2] = &v[..] {
                                    return eval(e1) + eval(e2);
                                }
                            };
                            {
                                ::std::rt::begin_panic("unreacheable")
                            };
                        })()
                    }
                    _ => (|| {
                        #[allow(unreachable_code)]
                        if true {
                            return {
                                ::std::rt::begin_panic_fmt(&::core::fmt::Arguments::new_v1(
                                    &["unhandled "],
                                    &match (&e,) {
                                        (arg0,) => [::core::fmt::ArgumentV1::new(
                                            arg0,
                                            ::core::fmt::Debug::fmt,
                                        )],
                                    },
                                ))
                            };
                        };
                        {
                            ::std::rt::begin_panic("unreacheable")
                        };
                    })(),
                }
            }
            {
                ::std::io::_print(::core::fmt::Arguments::new_v1(
                    &["eval(e0)= ", "\n"],
                    &match (&eval(&e0),) {
                        (arg0,) => [::core::fmt::ArgumentV1::new(
                            arg0,
                            ::core::fmt::Display::fmt,
                        )],
                    },
                ));
            };
            {
                ::std::io::_print(::core::fmt::Arguments::new_v1(
                    &["eval(e1)= ", "\n"],
                    &match (&eval(&e1),) {
                        (arg0,) => [::core::fmt::ArgumentV1::new(
                            arg0,
                            ::core::fmt::Display::fmt,
                        )],
                    },
                ));
            };
            {
                ::std::io::_print(::core::fmt::Arguments::new_v1(
                    &["eval(e1a)= ", "\n"],
                    &match (&eval(&e1a),) {
                        (arg0,) => [::core::fmt::ArgumentV1::new(
                            arg0,
                            ::core::fmt::Display::fmt,
                        )],
                    },
                ));
            };
            {
                ::std::io::_print(::core::fmt::Arguments::new_v1(
                    &["eval(e2)= ", "\n"],
                    &match (&eval(&e2),) {
                        (arg0,) => [::core::fmt::ArgumentV1::new(
                            arg0,
                            ::core::fmt::Display::fmt,
                        )],
                    },
                ));
            };
        }
    }
}
