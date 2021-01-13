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

trait IntoTerm {
    fn into_term(&self) -> Term;
}

impl IntoTerm for i64 {
    fn into_term(&self) -> Term {
        Term::Int(*self)
    }
}
impl IntoTerm for Term {
    fn into_term(&self) -> Term {
        self.clone()
    }
}

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
                Int(n) => *n,
                Prim(Read, v) if v.len() == 0 => {
                    let mut input = String::new();
                    std::io::stdin().read_line(&mut input).unwrap();
                    input.trim().parse().unwrap()
                },
                Prim(Neg, v) if let [e] = &v[..] => -interp_exp(e),
                Prim(Plus, v) if let [e1,e2] = &v[..] => interp_exp(e1) + interp_exp(e2),
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

        let prog = Rint(
            vec![],
            Prim(
                Plus,
                vec![Int(10), Prim(Neg, vec![Prim(Plus, vec![Int(12), Int(20)])])],
            ),
        );
        println!("prog= {:?} is_rint={}", prog, is_rint(&prog));
        println!("exec(prog) = {}", interp_rint(&prog));
        let prog = Rint(
            vec![],
            Prim(
                Plus,
                vec![Int(10), Prim(Neg, vec![Prim(Plus, vec![Int(12), Int(20)])])],
            ),
        );
        println!("prog= {:?} is_rint={}", prog, is_rint(&prog));
        println!("exec(prog) = {}", interp_rint(&prog));
        let flag = true;
        let flag = if flag { false } else { true };
        if flag {
            println!("enter number and press enter (get 42 for 50): ");
            println!(
                "exec(prog) = {}",
                interp_rint(&Rint(vec![], ast1_1.clone()))
            );
        }

        fn pe_neg(r: &Term) -> Term {
            match r {
                Int(n) => Int(-n),
                _ => Prim(Neg, vec![r.clone()]),
            }
        }

        fn pe_add(r1: &Term, r2: &Term) -> Term {
            match (r1, r2) {
                (Int(n1), Int(n2)) => Int(n1 + n2),
                _ => Prim(Plus, vec![r1.clone(), r2.clone()]),
            }
        }

        fn pe_exp(e: &Term) -> Term {
            match e {
                Prim(Neg, v) if let [e] = &v[..] => pe_neg(&pe_exp(e)),
                Prim(Plus, v) if let [e1,e2] = &v[..] => pe_add(&pe_exp(e1), &pe_exp(e2)),
                _ => e.clone(),
            }
        }
        fn pe_rint(p: &Rint) -> Rint {
            match p {
                Rint(v, e) => Rint(v.clone(), pe_exp(e)),
            }
        }
        macro_rules! plus {
            ($e1:expr, $e2:expr) => {
                Prim(Plus, vec![$e1.into_term(), $e2.into_term()])
            };
        }
        macro_rules! neg {
            ($e:expr) => {
                Prim(Neg, vec![$e.into_term()])
            };
        }
        macro_rules! read {
            () => {
                Prim(Read, vec![])
            };
        }
        macro_rules! int {
            ($e:expr) => {
                Int($e)
            };
        }
        macro_rules! rint {
            ($e:expr) => {
                Rint(vec![], $e.into_term())
            };
        }
        let prog1 = rint!(plus!(read!(), neg!(plus!(5, 3))));
        println!("prog1= {:?}", prog1);
        let prog2 = pe_rint(&prog1);
        println!("prog2= {:?}", prog2);

        fn test_pe(p: &Rint) {
            assert_eq!(interp_rint(p), interp_rint(&pe_rint(p)));
        }
        test_pe(&rint!(plus!(10, neg!(plus!(5, 3)))));
        test_pe(&rint!(plus!(int!(1), plus!(int!(3), int!(1)))));
        test_pe(&rint!(neg!(plus!(3, neg!(5)))));
    }
}
