#[cfg(test)]
mod cvar_lang {
    #[test]
    fn t1() {
        use ch5::cvar_lang;
        use cvar_lang::*;
        let v = int!(42);
        println!("v= {:?}", v);
        let v = var!(x);
        println!("v= {:?}", v);

        let exp = Expr::BinaryOp(BinaryOpKind::Add, int!(10), int!(32));
        println!("expr= {:?}", exp);
        println!("res= {:?}", interp_expr(&vec![], &exp));

        let prog = CProgram(vec![BasicBlock("start".to_string(), Tail::Return(exp))]);
        println!("prog= {:?}", prog);
        println!("eval= {:?}", interp_prog(&prog));
    }

    #[test]
    fn t2() {
        use ch5::cvar_lang;
        use cvar_lang::{RVar, RVarAnf};
        let (p1, v1) = {
            use RVar::*;
            let p1 = expr! {
                (let [x (add 12 (add (neg 20) (neg (add 18 (neg 15)))))]
                     (add (add 30 (neg 15)) x))
            };
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

        let cprog = cvar_lang::explicate_expr(p1anf);
        let cvar_lang::CProgram(tail) = &cprog;
        println!("tail= {:?}", tail);

        let v1clang = cvar_lang::interp_prog(&cprog);
        println!("v1clang= {:?}", v1clang);
        assert_eq!(v1anf, v1clang);
    }

    #[test]
    fn t3() {
        use ch5::cvar_lang;
        use cvar_lang::{RVar, RVarAnf};
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

        let cprog = cvar_lang::explicate_expr(e1anf);
        let cvar_lang::CProgram(tail) = &cprog;
        println!("tail= {:?}", tail);

        println!("interp_prog");
        let v1clang = cvar_lang::interp_prog(&cprog);
        println!("v1clang= {:?}", v1clang);
        assert_eq!(v1anf, v1clang);
    }

    #[test]
    fn t4() {
        use ch5::cvar_lang;
        use cvar_lang::{RVar, RVarAnf};
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

        let cprog = cvar_lang::explicate_expr(e1anf);
        let cvar_lang::CProgram(tail) = &cprog;
        println!("tail= {:?}", tail);

        let v1clang = cvar_lang::interp_prog(&cprog);
        println!("v1clang= {:?}", v1clang);
        assert_eq!(v1anf, v1clang);
        /*
        for cvar_lang::BasicBlock(name, bb) in cprog.0 {
            println!("{}:", name);
            println!("{:#?}", bb);
        }
        */
    }

    #[test]
    fn t5() {
        use ch5::cvar_lang;
        use cvar_lang::{RVar, RVarAnf};
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

        let cprog = cvar_lang::explicate_expr(e1anf);
        let cvar_lang::CProgram(tail) = &cprog;
        println!("tail= {:?}", tail);

        let v1clang = cvar_lang::interp_prog(&cprog);
        println!("v1clang= {:?}", v1clang);
        assert_eq!(v1anf, v1clang);
    }

    #[test]
    fn t6() {
        use ch5::cvar_lang;
        use cvar_lang::{RVar, RVarAnf};
        let (e1, v1) = {
            use RVar::*;
            let e1 = expr! {
                (let [x 1]
                     (let [y 101]
                          (if (let [x (if (or (eq x 1) (lt (read) 5))
                                          (eq y 101)
                                          (eq y 102))]
                                   (not x))
                              (add y 20)
                              (add y 30))))
            };
            let TypedExpr(_, ety) = typed_expr(e1.clone());
            println!("ety= {:?}", ety);

            let v1 = interp_expr(&e1);
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

        let cprog = cvar_lang::explicate_expr(e1anf);
        let cvar_lang::CProgram(tail) = &cprog;
        println!("tail= {:?}", tail);

        let v1clang = cvar_lang::interp_prog(&cprog);
        println!("v1clang= {:?}", v1clang);
        assert_eq!(v1anf, v1clang);
    }
}
