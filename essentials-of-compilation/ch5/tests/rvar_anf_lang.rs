#[cfg(test)]
mod rvar_anf_tests {
    #[test]
    fn t1() {
        use ch5::rvar_anf_lang;
        let (p1, v1) = {
            use rvar_anf_lang::rvar_lang::*;
            let p1 = expr! {
                (let [x 12] //(add 12 (add (neg 20) (neg (add 10 (neg 15)))))]
                     (add (add 30 (neg 15)) x))
            };
            println!("p1= {:?} ", p1);
            let p1ty = type_expr(&vec![], &p1);
            println!("type= {:?}", p1ty);
            let v1 = interp_expr(&p1);
            println!("v1= {:?} ", v1);
            let p1 = typed_expr(p1);
            println!("p1= {:?}", p1);
            (p1, v1)
        };

        use rvar_anf_lang::{interp_exp, rco_exp};
        let p1anf = rco_exp(p1);
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

            let v1 = interp_expr(&e1);
            let e1 = typed_expr(e1);
            (e1, v1)
        };
        println!("e1= {:?} ", e1);
        println!("v1= {:?} ", v1);

        use rvar_anf_lang::{interp_exp, rco_exp};
        let e1anf = rco_exp(e1);
        println!("e1anf= {:?} ", e1anf);

        let v1anf = interp_exp(&vec![], &e1anf);
        assert_eq!(v1, v1anf);
        println!("v1= {:?} ", v1);
    }

    #[test]
    fn t4() {
        use ch5::rvar_anf_lang;
        let (e, v) = {
            use rvar_anf_lang::rvar_lang::*;
            let e = expr! {
            (let [t1 (tuple 3 7)]
                 (let [t2 t1]
                      (let [_ (tupleset! t2 0 42)]
                           (tupleref t1 0))))};
            let v = interp_expr(&e);
            assert_eq!(v, Value::from(42));
            let e = typed_expr(e);
            let v1 = interp_texpr(&e);
            assert_eq!(v, v1);
            (e, v)
        };
        println!("e= {:?}", e);
        println!("v= {:?}", v);

        /*
        use rvar_anf_lang::*;
        let e_anf = rco_exp(e);
        println!("e1anf= {:?} ", e_anf);
        */

        /*
        let v_anf = interp_expr(&e_anf);
        assert_eq!(v, v_anf);
        */
    }

    #[test]
    fn t5() {
        let (e, v) = {
            use ch5::rvar_lang::*;
            let e = expr! {
                (let [v (tuple (tuple 44))]
                     (let [x (let [w (tuple 48)]
                                   (let [_ (tupleset! v 0 w)] (-6)))]
                          (add x (tupleref (tupleref v 0) 0))))
            };
            let v = interp_expr(&e);
            assert_eq!(v, Value::from(42));
            println!("e={:?}", e);
            let e = typed_expr(e);
            let v1 = interp_texpr(&e);
            assert_eq!(v, v1);
            (e, v)
        };
        println!("e= {:?}", e);
        println!("v= {:?}", v);
    }

    #[test]
    fn t6() {
        let (e, v) = {
            use ch5::rvar_lang::*;
            let e = expr! {
                (let [t (tuple 40 true (tuple 2))]
                     (if (tupleref t 1)
                         (add (tupleref t 0)
                              (tupleref (tupleref t 2) 0))
                         44))
            };
            let v = interp_expr(&e);
            assert_eq!(v, Value::from(42));
            println!("e={:?}", e);
            let e = typed_expr(e);
            let v1 = interp_texpr(&e);
            assert_eq!(v, v1);
            (e, v)
        };
        println!("e= {:?}", e);
        println!("v= {:?}", v);
    }
}
