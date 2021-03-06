#[cfg(test)]
mod rvar_lang {
    use ch6::rvar_lang as RVar;

    #[test]
    fn t0() {
        use RVar::*;
        let p1 = expr! {(let [x (add 12 20)] (add 10 x))};
        let tp1 = typed_expr(p1.clone());
        println!("type= {:?}", tp1);

        let p1 = program! {p1};
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
        use RVar::*;
        let e1 = expr! { (let [x 32] (add (let [x 10] x) x)) };
        let e2 = expr! { (let [x1 32] (add (let [x2 10] x2) x1)) };
        println!("e1= {:?}", e1);
        println!("e2= {:?}", e2);
        let Expr(_, e1ty) = typed_expr(e1.clone());
        let Expr(_, e2ty) = typed_expr(e2.clone());
        println!("e1ty= {:?}", e1ty);
        println!("e2ty= {:?}", e2ty);
        let e1u = uniquify_expr(e1);
        println!("e1u= {:?}", e1u);
        assert_eq!(e2, e1u);
        let Expr(_, e1u_ty) = typed_expr(e1u);
        assert_eq!(e1ty, e1u_ty);

        gensym_reset();
        let e1 = expr! { (let [x 32] (add (let [y 10] y) x)) };
        let e2 = expr! { (let [x1 32] (add (let [y1 10] y1) x1)) };
        println!("e1= {:?}", e1);
        println!("e2= {:?}", e2);
        let e1u = uniquify_expr(e1);
        println!("e1u= {:?}", e1u);
        assert_eq!(e2, e1u);
    }

    #[test]
    fn t2() {
        let (p1, v1) = {
            // not the same as
            // use ch2::rvar_lang::*;
            use RVar::*;
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
        use RVar::*;
        let expr = expr! {
            (let [x 0]
                 (let [y 100]
                      (if (if (lt x 1) (eq x 0) (eq x 2))
                          (add y 2)
                          (add y 10))))
        };
        println!("expr= {:?}", expr);
        let Expr(_, ety) = typed_expr(expr.clone());
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
        let Expr(_, ety) = typed_expr(expr.clone());
        println!("ety= {:?}", ety);
        let uexpr = uniquify_expr(expr);
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
            use RVar::*;
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
            use RVar::*;
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
            use RVar::*;
            let x = 40;
            let two = 2;
            let e = expr! {
                (let [t (tuple {x*2 - x} true (tuple {two}))]
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
            use RVar::*;
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

    #[test]
    fn t06a() {
        let (p, v) = {
            use RVar::*;
            let p = program1! {
                (defun (map_tu2 [f: (-> Int Int)]
                                [v: (Tuple Int Int)]) -> (Tuple Int Int)
                        (tuple (f (tupleref v 0)) (f (tupleref v 1))))
                (defun (add1 [x : Int]) -> Int
                        (add x 1))
                (tupleref (map_tu2 add1 (tuple 0 41)) 1)
            };
            let v = interp_program(&p);
            assert_eq!(Value::from(42), v);
            //let p = typed_program(p);
            let v1 = interp_program(&p);
            assert_eq!(v1, v);
            (p, v)
        };
        println!("prog= {:?}", p);
        println!("pval= {:?}", v);
    }
}
