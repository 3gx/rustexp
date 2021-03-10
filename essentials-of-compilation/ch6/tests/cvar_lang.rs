#[cfg(test)]
mod cvar_lang {
    use ch6::cvar_lang as CVar;
    use CVar::{RVar, RVarAnf};

    fn print_cfg(cfg: &CVar::CfgGraph) {
        for idx in cfg.node_indices() {
            println!("\t{:?}", cfg[idx]);
        }
    }

    #[test]
    fn t2() {
        let (p1, v1) = {
            use RVar::*;
            let p1 = expr! {
                (let [x (add 12 (add (neg 20) (neg (add 18 (neg 15)))))]
                     (add (add 30 (neg 15)) x))
            };
            let p1 = uniquify_expr(p1);
            let v1 = interp_expr(&p1);
            let p1 = typed_expr(p1);
            (p1, v1)
        };
        println!("p1= {:?} ", p1);
        println!("v1= {:?} ", v1);

        use RVarAnf::{interp_exp, rco_exp};
        let p1anf = rco_exp(p1);
        println!("p1= {:?} ", p1anf);

        let v1anf = interp_exp(&vec![], &p1anf);

        println!("v1anf= {:?}", v1anf);
        assert_eq!(v1, v1anf);

        let cprog = CVar::explicate_expr(p1anf);
        print_cfg(&cprog.cfg);

        let v1clang = CVar::interp_prog(&cprog);
        println!("v1clang= {:?}", v1clang);
        assert_eq!(v1anf, v1clang);

        let cstr = CVar::print_cprog(&cprog);
        println!("\n{}\n", cstr);
    }

    #[test]
    fn t3() {
        let (e1, v1) = {
            use RVar::*;
            let e1 = expr! {
                (let [x 1]
                     (let [y 101]
                          (if (and (eq x 0) (eq y (read)))
                              (add y 2)
                              (if (or (eq x 1) (eq x 2))
                                  (add y 20)
                                  (add y 30)))))
            };

            println!("interp_exp");
            let v1 = interp_expr(&e1);

            let e1 = uniquify_expr(e1);
            let e1 = typed_expr(e1);
            println!("ety= {:?}", e1);
            let v1a = interp_texpr(&e1);
            assert_eq!(v1, v1a);
            (e1, v1)
        };
        println!("e1= {:?} ", e1);
        println!("v1= {:?} ", v1);

        use RVarAnf::{interp_exp, rco_exp};
        let e1anf = rco_exp(e1);
        println!("e1anf= {:?} ", e1anf);

        let v1anf = interp_exp(&vec![], &e1anf);
        assert_eq!(v1, v1anf);
        println!("v1= {:?} ", v1);

        let cprog = CVar::explicate_expr(e1anf);
        print_cfg(&cprog.cfg);

        println!("interp_prog");
        let v1clang = CVar::interp_prog(&cprog);
        println!("v1clang= {:?}", v1clang);
        assert_eq!(v1anf, v1clang);

        let cstr = CVar::print_cprog(&cprog);
        println!("\n{}\n", cstr);
    }

    #[test]
    fn t4() {
        let (e1, v1) = {
            use RVar::*;
            let e1 = expr! {
                (let [x 1]
                     (let [y 101]
                          (if (and (eq (add (add x (neg 1)) 1) 0) (eq y (read)))
                              (add y 2)
                              (if (or (eq x 1) (eq x 2))
                                  (add y 20)
                                  (add y 30)))))
            };
            let v1 = interp_expr(&e1);
            let e1 = uniquify_expr(e1);
            let e1 = typed_expr(e1);
            let v1a = interp_texpr(&e1);
            assert_eq!(v1, v1a);
            (e1, v1)
        };
        println!("e1= {:?} ", e1);
        println!("v1= {:?} ", v1);

        use RVarAnf::{interp_exp, rco_exp};
        let e1anf = rco_exp(e1);
        println!("e1anf= {:?} ", e1anf);

        let v1anf = interp_exp(&vec![], &e1anf);
        assert_eq!(v1, v1anf);
        println!("v1= {:?} ", v1);

        let cprog = CVar::explicate_expr(e1anf);
        print_cfg(&cprog.cfg);

        let v1clang = CVar::interp_prog(&cprog);
        println!("v1clang= {:?}", v1clang);
        assert_eq!(v1anf, v1clang);

        let cstr = CVar::print_cprog(&cprog);
        println!("\n{}\n", cstr);
    }

    #[test]
    fn t5() {
        let (e1, v1) = {
            use RVar::*;
            let e1 = expr! {
                (let [x 1]
                     (let [y 101]
                          (if (if (or (eq x 1) (lt (read) 5))
                                  (eq y 101)
                                  (eq y 102))
                              (add y 20)
                              (add y 30))))
            };
            let v1 = interp_expr(&e1);
            let e1 = uniquify_expr(e1);
            let e1 = typed_expr(e1);
            let v1a = interp_texpr(&e1);
            assert_eq!(v1, v1a);
            (e1, v1)
        };
        println!("e1= {:?} ", e1);
        println!("v1= {:?} ", v1);

        use RVarAnf::{interp_exp, rco_exp};
        let e1anf = rco_exp(e1);
        println!("e1anf= {:?} ", e1anf);

        let v1anf = interp_exp(&vec![], &e1anf);
        assert_eq!(v1, v1anf);
        println!("v1= {:?} ", v1);

        let cprog = CVar::explicate_expr(e1anf);
        print_cfg(&cprog.cfg);

        let v1clang = CVar::interp_prog(&cprog);
        println!("v1clang= {:?}", v1clang);
        assert_eq!(v1anf, v1clang);

        let cstr = CVar::print_cprog(&cprog);
        println!("\n{}\n", cstr);
    }

    #[test]
    fn t6() {
        let (e1, v1) = {
            use RVar::*;
            let e1 = expr! {
                (let [x 1]
                     (let [y 101]
                          (if (let [x1 (if (or (eq x 1) (lt (read) 5))
                                          (eq y 101)
                                          (eq y 102))]
                                   (not x1))
                              (add y 20)
                              (add y 30))))
            };
            let Expr(_, ety) = typed_expr(e1.clone());
            println!("ety= {:?}", ety);

            let v1 = interp_expr(&e1);
            let e1 = uniquify_expr(e1);
            let e1 = typed_expr(e1);
            let v1a = interp_texpr(&e1);
            assert_eq!(v1, v1a);
            (e1, v1)
        };
        println!("e1= {:?} ", e1);
        println!("v1= {:?} ", v1);

        use RVarAnf::{interp_exp, rco_exp};
        let e1anf = rco_exp(e1);
        println!("e1anf= {:?} ", e1anf);

        let v1anf = interp_exp(&vec![], &e1anf);
        assert_eq!(v1, v1anf);
        println!("v1= {:?} ", v1);

        let cprog = CVar::explicate_expr(e1anf);
        print_cfg(&cprog.cfg);

        let v1clang = CVar::interp_prog(&cprog);
        println!("v1clang= {:?}", v1clang);
        assert_eq!(v1anf, v1clang);

        let cstr = CVar::print_cprog(&cprog);
        println!("\n{}\n", cstr);
    }

    #[test]
    fn t7() {
        let (e, v) = {
            use RVar::*;
            let e = expr! {
            (let [t1 (tuple 3 7)]
                 (let [t2 t1]
                      (let [_ (tupleset! t2 0 42)]
                           (tupleref t1 0))))};
            let v = interp_expr(&e);
            assert_eq!(v, Value::from(42));
            println!("\nuntyped= {:?}", e);
            let e = uniquify_expr(e);
            let e = typed_expr(e);
            let v1 = interp_texpr(&e);
            assert_eq!(v, v1);
            (e, v)
        };
        println!("\ntyped= {:?}", e);
        println!("\nval= {:?}", v);

        let e_anf = RVarAnf::rco_exp(e);
        println!("\ne_anf= {:?} ", e_anf);

        let v_anf = RVarAnf::interp_expr(&e_anf);
        assert_eq!(v, v_anf);

        let cprog = CVar::explicate_expr(e_anf);
        print_cfg(&cprog.cfg);

        let v1clang = CVar::interp_prog(&cprog);
        println!("v1clang= {:?}", v1clang);
        assert_eq!(v_anf, v1clang);

        /*
        let cstr = CVar::print_cprog(&cprog);
        println!("\n{}\\nn", cstr);
        */
    }

    #[test]
    fn t8() {
        let (e, v) = {
            use RVar::*;
            let e = expr! {
                (let [v (tuple (tuple 44))]
                     (let [x (let [w (tuple 48)]
                                   (let [_ (tupleset! v 0 w)] (- 6)))]
                          (add x (tupleref (tupleref v 0) 0))))
            };
            let v = interp_expr(&e);
            assert_eq!(v, Value::from(42));
            println!("\nuntyped= {:?}", e);
            let e = uniquify_expr(e);
            let e = typed_expr(e);
            let v1 = interp_texpr(&e);
            assert_eq!(v, v1);
            (e, v)
        };
        println!("\ntyped= {:?}", e);
        println!("\nval= {:?}", v);

        let e_anf = RVarAnf::rco_exp(e);
        println!("\ne_anf= {:?} ", e_anf);

        let v_anf = RVarAnf::interp_expr(&e_anf);
        assert_eq!(v, v_anf);

        let cprog = CVar::explicate_expr(e_anf);
        print_cfg(&cprog.cfg);

        let v1clang = CVar::interp_prog(&cprog);
        println!("v1clang= {:?}", v1clang);
        assert_eq!(v_anf, v1clang);

        /*
        let cstr = CVar::print_cprog(&cprog);
        println!("\n{}\\nn", cstr);
        */
    }

    #[test]
    fn t9() {
        use ch6::cvar_lang as CVar;
        use CVar::{RVar, RVarAnf};
        let (e, v) = {
            use RVar::*;
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
            let e = uniquify_expr(e);
            let e = typed_expr(e);
            let v1 = interp_texpr(&e);
            assert_eq!(v, v1);
            (e, v)
        };
        println!("\ntyped= {:?}", e);
        println!("\nval= {:?}", v);

        let e_anf = RVarAnf::rco_exp(e);
        println!("\ne_anf= {:?} ", e_anf);

        let v_anf = RVarAnf::interp_expr(&e_anf);
        assert_eq!(v, v_anf);

        let cprog = CVar::explicate_expr(e_anf);
        print_cfg(&cprog.cfg);

        let v1clang = CVar::interp_prog(&cprog);
        println!("v1clang= {:?}", v1clang);
        assert_eq!(v_anf, v1clang);

        /*
        let cstr = CVar::print_cprog(&cprog);
        println!("\n{}\\nn", cstr);
        */
    }

    #[test]
    fn t10() {
        use ch6::cvar_lang as CVar;
        use CVar::{RVar, RVarAnf};
        let (e, v) = {
            use RVar::*;
            let e = expr! {
                (tupleref (tupleref (tuple (tuple 42)) 0) 0)
            };
            let v = interp_expr(&e);
            assert_eq!(v, Value::from(42));
            println!("\nuntyped= {:?}", e);
            let e = uniquify_expr(e);
            let e = typed_expr(e);
            let v1 = interp_texpr(&e);
            assert_eq!(v, v1);
            (e, v)
        };
        println!("\ntyped= {:?}", e);
        println!("\nval= {:?}", v);

        let e_anf = RVarAnf::rco_exp(e);
        println!("\ne_anf= {:?} ", e_anf);

        let v_anf = RVarAnf::interp_expr(&e_anf);
        assert_eq!(v, v_anf);

        let cprog = CVar::explicate_expr(e_anf);
        print_cfg(&cprog.cfg);

        let v1clang = CVar::interp_prog(&cprog);
        println!("v1clang= {:?}", v1clang);
        assert_eq!(v_anf, v1clang);

        /*
        let cstr = CVar::print_cprog(&cprog);
        println!("\n{}\\nn", cstr);
        */
    }
}
