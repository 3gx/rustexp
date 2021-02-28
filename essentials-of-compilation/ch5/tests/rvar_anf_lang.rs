#[cfg(test)]
mod rvar_anf_tests {
    #[test]
    fn t1() {
        use ch5::rvar_anf_lang;
        let (p1, v1) = {
            use rvar_anf_lang::rvar_lang::*;
            let p1 = expr! {
                (let [x (add 12 (add (neg 20) (neg (add 10 (neg 15)))))]
                     (add (add 30 (neg 15)) x))
            };
            /*
            let p1 = r#let!([x add!(12, add!(neg!(20), neg!(add!(10,neg!(15)))))]
                    add!(add!(30, neg!(15)), x));
            */
            println!("p1= {:?} ", p1);
            let p1ty = type_expr(&vec![], &p1);
            println!("type= {:?}", p1ty);
            let v1 = interp_exp(&vec![], &p1);
            println!("v1= {:?} ", v1);
            let p1 = typed_expr(&vec![], p1);
            (p1, v1)
        };

        use rvar_anf_lang::{interp_exp, rco_exp};
        let p1anf = rco_exp(&p1);
        println!("p1anf= {:?} ", p1anf);

        let v1anf = interp_exp(&vec![], &p1anf);

        println!("v1anf= {:?}", v1anf);
        assert_eq!(v1, v1anf);
        let v1 = vec![3, 4];
        let v2 = vec![5, 6];
        let mut a = [&v1[..], &v2[..]].concat();
        a.push(42);
        println!("a={:?}", a);
    }

    #[test]
    fn t2() {
        use ch5::rvar_anf_lang;
        let (e1, v1) = {
            use rvar_anf_lang::rvar_lang::*;
            let e1 = expr! {
                (let [x 1]
                     (let [y 101]
                          (if (and (eq (add (add x (neg 1)) 1) 0)
                                   (eq y (read)))
                              (add y 2)
                              (if (or (eq x 1) (eq x 2))
                                  (add y 20)
                                  (add y 30)))))
            };
            /*
            let e1 = let_!([x 1]
                        let_!([y 101]
                          r#if!( and!(eq!(add!(add!(x,neg!(1)), 1),0), eq!(y,read!())), // check short-circuit
                                 add!(y,2),
                                 r#if!( or!(eq!(x,1), eq!(x,2)),
                                        add!(y,20),
                                        add!(y,30)))));
            */

            let v1 = interp_exp(&vec![], &e1);
            let e1 = typed_expr(&vec![], e1);
            (e1, v1)
        };
        println!("e1= {:?} ", e1);
        println!("v1= {:?} ", v1);

        use rvar_anf_lang::{interp_exp, rco_exp};
        let e1anf = rco_exp(&e1);
        println!("e1anf= {:?} ", e1anf);

        let v1anf = interp_exp(&vec![], &e1anf);
        assert_eq!(v1, v1anf);
        println!("v1= {:?} ", v1);
    }
}
