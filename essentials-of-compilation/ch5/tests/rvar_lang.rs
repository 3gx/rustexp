#[cfg(test)]
mod rvar_lang {
    #[test]
    fn t0() {
        use ch5::rvar_lang::*;
        let p1 = expr! {(let [x (add 12 20)] (add 10 x))};
        let tp1 = typed_expr(p1.clone());
        println!("type= {:?}", tp1);

        let p1 = program! {p1};
        let p1ty = type_expr(&vec![], &p1.0);
        println!("type= {:?}", p1ty);
        let v1 = interp_program(&p1);
        println!("v1= {:?} ", v1);

        let p1 = expr! {(let [x 2] (add (let [x 10] x) x))};
        println!("p1= {:?} ", p1);
        let p1 = program! {p1};
        let v1 = interp_program(&p1);
        println!("v1= {:?} ", v1);

        let p1 = expr! {
            (let [x (read)]
                 (let [y (read)] (add x (neg y))))
        };
        let p1 = program! {p1};
        println!("p1= {:?} ", p1);
        let t = true;
        let t = !t;
        if t {
            println!("inter 52<enter>, 10<enter>, should get 42");
            use std::io::{self, Write};
            io::stdout().flush().unwrap();
            let v1 = interp_program(&p1);
            println!("v1= {:?} ", v1);
        }
    }

    #[test]
    fn t1() {
        use ch5::rvar_lang::sym;
        use ch5::rvar_lang::*;
        let e1 = expr! { (let [x 32] (add (let [x 10] x) x)) };
        let e2 = expr! { (let [x1 32] (add (let [x2 10] x2) x1)) };
        println!("e1= {:?}", e1);
        println!("e2= {:?}", e2);
        let e1ty = type_expr(&vec![], &e1);
        let e2ty = type_expr(&vec![], &e2);
        println!("e1ty= {:?}", e1ty);
        println!("e2ty= {:?}", e2ty);
        let e1u = uniquify_expr(&sym![], e1);
        println!("e1u= {:?}", e1u);
        assert_eq!(e2, e1u);
        let e1u_ty = type_expr(&vec![], &e1u);
        assert_eq!(e1ty, e1u_ty);

        gensym_reset();
        let e1 = expr! { (let [x 32] (add (let [y 10] y) x)) };
        let e2 = expr! { (let [x1 32] (add (let [y1 10] y1) x1)) };
        println!("e1= {:?}", e1);
        println!("e2= {:?}", e2);
        let e1u = uniquify_expr(&sym![], e1);
        println!("e1u= {:?}", e1u);
        assert_eq!(e2, e1u);
    }

    #[test]
    fn t2() {
        use ch5::rvar_lang;
        let (p1, v1) = {
            // not the same as
            // use ch2::rvar_lang::*;
            use rvar_lang::*;
            let p = expr! {
                (let [x (add 12 (add (neg 20) (neg (add 10 (neg 15)))))]
                     (add (add 30 (neg 15)) x))
            };
            let v1 = interp_expr(&p);
            (p, v1)
        };
        println!("p1= {:?} ", p1);
        println!("v1= {:?} ", v1);
    }

    #[test]
    fn t3() {
        use ch5::rvar_lang::*;
        let expr = expr! {
            (let [x 0]
                 (let [y 100]
                      (if (if (lt x 1) (eq x 0) (eq x 2))
                          (add y 2)
                          (add y 10))))
        };
        println!("expr= {:?}", expr);
        let ety = type_expr(&vec![], &expr);
        println!("ety= {:?}", ety);
        let prog = program![expr];
        let val = interp_program(&prog);
        println!("val= {:?}", val);
        assert_eq!(val, Value::Int(102));

        gensym_reset();
        let expr = expr! {
        (let [x 1]
             (let [y 100]
                  (if (if (lt x 1) (eq x 0) (eq x 2))
                      (add y 2)
                      (add y 10)))) };
        let ety = type_expr(&vec![], &expr);
        println!("ety= {:?}", ety);
        let uexpr = uniquify_expr(&sym![], expr);
        println!("uexpr= {:?}", uexpr);
        let prog = program![uexpr];
        let uval = interp_program(&prog);
        assert_eq!(uval, Value::Int(110));

        let a = expr! { (add 3 false) };
        println!("a= {:?}", a);
    }

    #[test]
    fn t4() {
        let (e, v) = {
            use ch5::rvar_lang::*;
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
            let x = 40;
            let two = 2;
            let e = expr! {
                (let [t (tuple (unquote x*2 - x) true (tuple (@two)))]
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

    #[test]
    fn t7() {
        let (e, v) = {
            use ch5::rvar_lang::*;
            let e = expr! {
                (tupleref (tupleref (tuple (tuple 42)) 0) 0)
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
