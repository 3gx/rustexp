#![allow(incomplete_features)]
#![feature(if_let_guard)]

#[derive(Debug, Clone, Copy)]
pub enum OpCode {
    Plus,
    Neg,
    Read,
}

#[derive(Debug, Clone)]
pub enum Term {
    Int(i64),
    Prim(OpCode, Vec<Term>),
}
#[derive(Debug, Clone)]
pub struct Options;
#[derive(Debug, Clone)]
pub struct Rint(Vec<Options>, Term);

fn main() {
    {
        let eight = Term::Int(8);
        println!("eight= {:?}", eight);

        let neg_eight = Term::Prim(OpCode::Neg, vec![eight]);
        println!("neg_eight= {:?}", neg_eight);

        let rd = Term::Prim(OpCode::Read, vec![]);
        println!("rd= {:?}", rd);

        use {OpCode::*, Term::*};
        let ast1_1 = Prim(Plus, vec![rd, neg_eight]);
        println!("ast1_1= {:?}", ast1_1);

        match &ast1_1 {
            Prim(op, v) if let [_,_] = &v[..] => println!("{:?}", op),
            _ => panic!("unhandled {:?}", ast1_1),
        };

        fn is_leaf(arith: &Term) -> bool {
            match arith {
                Int(_) => true,
                Prim(Read, v) if let [] = &v[..] => true,
                Prim(Neg, v) if let [_] = &v[..] => false,
                Prim(Plus, v) if let [_,_] = &v[..] => false,
                _ => panic!("invalid term {:?}", arith),
            }
        }
        let term = Prim(Read, vec![]);
        println!("is_leaf({:?}) = {}", term, is_leaf(&term));
        let term = Prim(Neg, vec![Int(8)]);
        println!("is_leaf({:?}) = {}", term, is_leaf(&term));
        let term = Int(8);
        println!("is_leaf({:?}) = {}", term, is_leaf(&term));

        fn is_exp(ast: &Term) -> bool {
            match ast {
                Int(_) => true,
                Prim(Read, v) if v.len() == 0 => true,
                Prim(Neg, v) if v.len() == 1 => is_exp(&v[0]),
                Prim(Plus, v) if let [e1,e2] = &v[..] => is_exp(e1) && is_exp(e2),
                _ => false,
            }
        }
        fn is_rint(prog: &Rint) -> bool {
            match prog {
                Rint(_, ast) => is_exp(ast),
            }
        }
        macro_rules! is_rint {
            ($arg:expr) => {
                is_rint(&$arg)
            };
        }
        let prog = Rint(vec![], ast1_1.clone());
        println!("prog= {:?}  is_rint= {}", prog, is_rint(&prog));
        let prog = Rint(
            vec![],
            Prim(Neg, vec![Prim(Read, vec![]), Prim(Plus, vec![Int(8)])]),
        );
        println!("prog= {:?}  is_rint= {}", prog, is_rint!(prog));

        type Value = i64;
        fn interp_exp(e: &Term) -> Value {
            match e {
                _ => panic!("unhandled expression {:?}", e),
            }
        }

        fn interp_rint(p: &Rint) -> Value {
            match p {
                Rint(_, e) => interp_exp(e),
            }
        }

        let prog = Rint(vec![], Prim(Plus, vec![Int(10), Int(32)]));
        println!("prog= {:?}", prog);
        println!("exec(prog) = {}", interp_rint(&prog));
    }
    {
        println!("\n--- examples---\n");
        fn fun(slice: &[i32]) {
            println!("slice= {:?}", slice);
            let what = match slice {
                [_, _, 3] => "ends with 3",
                [_a, _b, _c] => "ends with something else",
                _ => "unhandled",
            };
            println!("what= {:?}", what);
        }
        let arr = [1, 2, 3];
        fun(&arr);
        let vec = vec![1, 2, 3];
        fun(&vec);

        assert_eq!(
            "ends with 3",
            match arr {
                [_, _, 3] => "ends with 3",
                [_a, _b, _c] => "ends with something else",
            }
        );
        assert_eq!(
            "ends with 3",
            match vec.as_slice() {
                [_, _, 3] => "ends with 3",
                [_a, _b, _c] => "ends with something else",
                _ => "unhandled",
            }
        );

        enum T1 {
            Vi(Vec<i32>),
            Vf(Vec<f32>),
        }

        let ti = T1::Vi(vec![1, 2, 3]);
        let tf = T1::Vf(vec![1.1, 2.2, 3.3]);

        fn match1(t1: &T1) {
            match t1 {
                T1::Vi(v) if let [_,_,3] = &v[..] => println!("ends with 3"),
                _ => println!("unhandled"),
            }
        }
        fn match2(t1: &T1) {
            match t1 {
                T1::Vi(v) if (v.len() > 2 || v[2] == 3) => println!("ends with 3"),
                T1::Vf(v) if (v.len() > 2 || v[2] == 3.3) => println!("ends with 3.3"),
                _ => println!("unhandled"),
            }
        }
        match1(&ti);
        match2(&tf);
        match1(&tf);
    }
}
