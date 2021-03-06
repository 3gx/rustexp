#[cfg(test)]
mod x86var_lang {
    use ch5x86::x86var_lang as X86Var;
    use CVar::{RVar, RVarAnf};
    use X86Var::cvar_lang as CVar;

    fn print_cfg(cfg: &X86Var::CfgGraph) {
        for idx in cfg.node_indices() {
            println!("\t{:?}", cfg[idx]);
        }
    }

    #[test]
    fn t1() {
        let (p1, v1) = {
            use RVar::*;
            let p1 = expr! {
                (let [x (add (add (neg 20) (neg (add 10 (neg 15)))) 12)]
                     (add (add 30 (neg 15)) x))
            };
            let v1 = interp_expr(&p1);
            let p1 = typed_expr(p1);
            let v1a = interp_texpr(&p1);
            assert_eq!(v1, v1a);
            (p1, v1)
        };
        //println!("p1= {:?} ", p1);

        use RVarAnf::{interp_exp, rco_exp};
        let p1anf = rco_exp(p1);
        //println!("p1= {:?} ", p1anf);

        let v1anf = interp_exp(&vec![], &p1anf);

        //println!("v1anf= {}", v1anf);
        assert_eq!(v1, v1anf);

        let cprog = CVar::explicate_expr(p1anf);

        let v1clang = CVar::interp_prog(&cprog);
        assert_eq!(v1anf, v1clang);

        let v1clang = CVar::interp_prog(&cprog);
        println!("v1clang= {:?}", v1clang);
        assert_eq!(v1anf, v1clang);

        use X86Var::*;
        let x86cfg = select_inst(cprog);
        print_cfg(&x86cfg.cfg);
        let x86val = interp_prog(&x86cfg);
        assert_eq!(X86Var::Value::from(v1.clone()), x86val);

        let x86cfg = liveness_analysis(x86cfg);
        println!("x86cfg= {:?}", x86cfg);

        let ginterfere = interference_graph(&x86cfg);
        let gbias = move_bias(&x86cfg);
        let regs = reg_alloc(&ginterfere, &gbias);
        let x86cfg = x86cfg.regs(regs);

        let x86homes = X86Var::assign_homes(x86cfg);
        println!("***assgned_homes***:");
        print_cfg(&x86homes.cfg);

        let x86val = X86Var::interp_prog(&x86homes);
        assert_eq!(X86Var::Value::from(v1.clone()), x86val);

        let x86patched = X86Var::patch_prog(x86homes);
        println!("***patched_cfg***:");
        print_cfg(&x86patched.cfg);
        let x86val = X86Var::interp_prog(&x86patched);
        assert_eq!(X86Var::Value::from(v1.clone()), x86val);

        let asmstr = X86Var::print_x86prog(&x86patched);
        println!("\n{}", asmstr);
        println!("result={:?}", v1);
    }

    #[test]
    fn t2() {
        {
            #[derive(Debug)]
            struct SymPair(usize, usize);

            use std::hash::{Hash, Hasher};

            impl Eq for SymPair {}
            impl PartialEq for SymPair {
                fn eq(&self, other: &Self) -> bool {
                    self.0 == other.0 && self.1 == other.1 || self.0 == other.1 && self.1 == other.0
                }
            }

            // This will make it easier to find the right connections
            impl PartialEq<usize> for SymPair {
                fn eq(&self, other: &usize) -> bool {
                    self.0 == *other || self.1 == *other
                }
            }

            impl Hash for SymPair {
                fn hash<H: Hasher>(&self, _: &mut H) {}
            }

            use std::collections::HashSet;
            type HS = HashSet<SymPair>;
            let mut h: HS = HashSet::new();
            h.insert(SymPair(1, 2));
            h.insert(SymPair(2, 1));
            println!("{:?}", h);
        }
        {
            #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
            enum V {
                Int(usize),
                Str(String),
            }
            #[derive(Debug, Clone)]
            struct SymPair(V, V);

            impl Eq for SymPair {}
            impl PartialEq for SymPair {
                fn eq(&self, other: &Self) -> bool {
                    self.0 == other.0 && self.1 == other.1 || self.0 == other.1 && self.1 == other.0
                }
            }

            use std::cmp::Ordering;
            impl Ord for SymPair {
                fn cmp(&self, other: &Self) -> Ordering {
                    use std::cmp::{max, min};
                    let v0 = (min(&self.0, &self.1), max(&self.0, &self.1));
                    let v1 = (min(&other.0, &other.1), max(&other.0, &other.1));
                    v0.cmp(&v1)
                }
            }
            impl PartialOrd for SymPair {
                fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                    Some(self.cmp(other))
                }
            }

            use std::collections::BTreeSet;
            let mut h = BTreeSet::new();
            use V::{Int, Str};
            h.insert(SymPair(Int(3), Int(2)));
            h.insert(SymPair(Int(1), Str("2".to_string())));
            h.insert(SymPair(Str("2".to_string()), Int(1)));
            h.insert(SymPair(Int(2), Int(3)));
            println!("\n-\n{:?}", h);
        }
    }

    #[test]
    fn t3() {
        let (p1, v1) = {
            use RVar::*;
            let p1 = expr! {
                (let [v 1]
                     (let [w 42]
                          (let [x (add v 7)]
                               (let [y x]
                                    (let [z (add x w)]
                                         (add z (neg y)))))))
            };
            let v1 = interp_expr(&p1);
            let p1 = typed_expr(p1);
            let v1a = interp_texpr(&p1);
            assert_eq!(v1, v1a);
            (p1, v1)
        };
        //println!("p1= {:?} ", p1);

        use RVarAnf::{interp_exp, rco_exp};
        let p1anf = rco_exp(p1);
        //println!("p1= {:?} ", p1anf);

        let v1anf = interp_exp(&vec![], &p1anf);

        //println!("v1anf= {}", v1anf);
        assert_eq!(v1, v1anf);

        let cprog = CVar::explicate_expr(p1anf);

        let v1clang = CVar::interp_prog(&cprog);
        assert_eq!(v1anf, v1clang);

        let x86cfg = X86Var::select_inst(cprog);
        print_cfg(&x86cfg.cfg);
        let x86val = X86Var::interp_prog(&x86cfg);
        assert_eq!(X86Var::Value::from(v1.clone()), x86val);

        use X86Var::*;
        let x86cfg = liveness_analysis(x86cfg);
        let ginterfere = interference_graph(&x86cfg);
        let gbias = move_bias(&x86cfg);
        let regs = reg_alloc(&ginterfere, &gbias);
        let x86cfg = x86cfg.regs(regs);
        let x86homes = assign_homes(x86cfg);
        let x86val = X86Var::interp_prog(&x86homes);
        assert_eq!(X86Var::Value::from(v1.clone()), x86val);
        let x86patched = X86Var::patch_prog(x86homes);
        let x86val = X86Var::interp_prog(&x86patched);
        assert_eq!(X86Var::Value::from(v1.clone()), x86val);
        let asmstr = X86Var::print_x86prog(&x86patched);
        println!("\n{}", asmstr);
        println!("result={:?}", v1);
    }

    #[test]
    fn t4() {
        let (e1, v1) = {
            use RVar::*;
            let e1 = expr! {
                (let [x 1]
                     (let [y 1]
                          (if (if (or (eq x 1) (lt (read) 5))
                                  (eq y 101) (eq y 102))
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

        let cprog = CVar::explicate_expr(e1anf);
        let CVar::CProgram(tail) = &cprog;
        println!("tail= {:?}", tail);

        let v1clang = CVar::interp_prog(&cprog);
        println!("v1clang= {:?}", v1clang);
        assert_eq!(v1anf, v1clang);

        let x86cfg = X86Var::select_inst(cprog);
        print_cfg(&x86cfg.cfg);

        let x86val = X86Var::interp_prog(&x86cfg);
        assert_eq!(X86Var::Value::from(v1.clone()), x86val);

        use X86Var::*;
        let x86cfg = liveness_analysis(x86cfg);
        let ginterfere = interference_graph(&x86cfg);
        let gbias = move_bias(&x86cfg);
        let regs = reg_alloc(&ginterfere, &gbias);
        let x86cfg = x86cfg.regs(regs);
        let x86val = X86Var::interp_prog(&x86cfg);
        assert_eq!(X86Var::Value::from(v1.clone()), x86val);
        let x86homes = assign_homes(x86cfg);
        let x86val = X86Var::interp_prog(&x86homes);
        assert_eq!(X86Var::Value::from(v1.clone()), x86val);
        let x86patched = X86Var::patch_prog(x86homes);
        let x86val = X86Var::interp_prog(&x86patched);
        assert_eq!(X86Var::Value::from(v1.clone()), x86val);
        let asmstr = X86Var::print_x86prog(&x86patched);
        println!("\n{}", asmstr);
        println!("result={:?}", v1);
    }

    #[test]
    fn t5() {
        let (e1, v1) = {
            use RVar::*;
            let e1 = expr! {
                (let [x 1]
                     (let [y 101]
                          (if (let [x (if (or (eq x 1) (lt (read) 5))
                                          (eq y 101)
                                          (eq y 102))]
                                   (not (not x)))
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

        let cprog = CVar::explicate_expr(e1anf);
        let CVar::CProgram(tail) = &cprog;
        println!("tail= {:?}", tail);

        let v1clang = CVar::interp_prog(&cprog);
        println!("v1clang= {:?}", v1clang);
        assert_eq!(v1anf, v1clang);

        use X86Var::*;
        let x86cfg = select_inst(cprog);
        print_cfg(&x86cfg.cfg);

        let x86cfg = liveness_analysis(x86cfg);
        let ginterfere = interference_graph(&x86cfg);
        let gbias = move_bias(&x86cfg);
        let regs = reg_alloc(&ginterfere, &gbias);
        let x86cfg = x86cfg.regs(regs);
        let x86val = X86Var::interp_prog(&x86cfg);
        assert_eq!(X86Var::Value::from(v1.clone()), x86val);
        let x86homes = assign_homes(x86cfg);

        let x86val = X86Var::interp_prog(&x86homes);
        assert_eq!(X86Var::Value::from(v1.clone()), x86val);
        let x86patched = X86Var::patch_prog(x86homes);
        let x86val = X86Var::interp_prog(&x86patched);
        assert_eq!(X86Var::Value::from(v1.clone()), x86val);
        let asmstr = X86Var::print_x86prog(&x86patched);
        println!("\n{}", asmstr);
        println!("result={:?}", v1);
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
        let CVar::CProgram(tail) = &cprog;
        println!("tail= {:?}", tail);

        let v1clang = CVar::interp_prog(&cprog);
        println!("v1clang= {:?}", v1clang);
        assert_eq!(v_anf, v1clang);

        use X86Var::*;
        let x86cfg = select_inst(cprog);
        print_cfg(&x86cfg.cfg);

        /*
        let x86val = X86Var::interp_prog(&x86cfg);
        assert_eq!(X86Var::Value::from(v.clone()), x86val);

        let x86homes = assign_homes(x86cfg);
        let x86val = X86Var::interp_prog(&x86homes);
        assert_eq!(X86Var::Value::from(v.clone()), x86val);
         */
    }

    #[test]
    fn t8() {
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
            println!("\nuntyped= {:?}", e);
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
        let CVar::CProgram(tail) = &cprog;
        println!("tail= {:?}", tail);

        let v1clang = CVar::interp_prog(&cprog);
        println!("v1clang= {:?}", v1clang);
        assert_eq!(v_anf, v1clang);

        use X86Var::*;
        let x86cfg = select_inst(cprog);
        print_cfg(&x86cfg.cfg);
    }

    #[test]
    fn t9() {
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
        let CVar::CProgram(tail) = &cprog;
        println!("tail= {:?}", tail);

        let v1clang = CVar::interp_prog(&cprog);
        println!("v1clang= {:?}", v1clang);
        assert_eq!(v_anf, v1clang);

        use X86Var::*;
        let x86cfg = select_inst(cprog);
        print_cfg(&x86cfg.cfg);
    }

    #[test]
    fn t10() {
        let (e, v) = {
            use RVar::*;
            let e = expr! {
                (tupleref (tupleref (tuple (tuple 42)) 0) 0)
            };
            let v = interp_expr(&e);
            assert_eq!(v, Value::from(42));
            println!("\nuntyped= {:?}", e);
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
        let CVar::CProgram(tail) = &cprog;
        println!("tail= {:?}", tail);

        let v1clang = CVar::interp_prog(&cprog);
        println!("v1clang= {:?}", v1clang);
        assert_eq!(v_anf, v1clang);

        use X86Var::*;
        let x86cfg = select_inst(cprog);
        print_cfg(&x86cfg.cfg);
    }
}
