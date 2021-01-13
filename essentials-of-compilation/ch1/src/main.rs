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
    }
    {
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
                T1::Vi(v) => match &v[..] {
                    [_, _, 3] => println!("ends with 3"),
                    _ => println!("unhandled"),
                },
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
