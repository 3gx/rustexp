#![feature(box_patterns)]

fn main() {
    //use std::ops::Deref;

    //let x = box 5;

    #[derive(Debug, Clone, PartialEq)]
    enum Expr {
        Lit(i32),
        Add(Box<Expr>, Box<Expr>),
    }
    use Expr::*;

    let e = Box::new(Add(
        Box::new(Lit(7)),
        Box::new(Add(Box::new(Lit(1)), Box::new(Lit(2)))),
    ));
    println!("{:?}", e);

    let e2 = match &*e {
        Add(box Lit(i), box Add(box Lit(j), box Lit(k))) => Lit(i + j + k),
        e => e.clone(),
    };
    println!("{:?}", e2);

    let e3 = match &*e {
        Add(lhs, rhs) => {
            if let (Lit(0), Add(lhs, rhs)) = (&**lhs, &**rhs) {
                if let (Lit(1), Lit(2)) = (&**lhs, &**rhs) {
                    Lit(3)
                } else {
                    (*e).clone()
                }
            } else {
                (*e).clone()
            }
        }
        e => (*e).clone(),
    };
    println!("{:?}", e3);

    let e4 = match &*e {
        Add(lhs, rhs) => match (&**lhs, &**rhs) {
            (Lit(0), Add(lhs, rhs)) => match (&**lhs, &**rhs) {
                (Lit(1), Lit(2)) => Lit(3),
                _ => (*e).clone(),
            },
            _ => (*e).clone(),
        },
        e => e.clone(),
    };

    println!("{:?}", e4);

    let e5 = match &*e {
        Add(lhs, rhs) => match (&**lhs, &**rhs) {
            (Lit(i), Add(lhs, rhs)) => match (&**lhs, &**rhs) {
                (Lit(j), Lit(k)) => Lit(i + j + k),
                _ => (*e).clone(),
            },
            _ => (*e).clone(),
        },
        e => e.clone(),
    };

    println!("{:?}", e5);
}

