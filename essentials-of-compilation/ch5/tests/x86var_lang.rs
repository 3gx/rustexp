#[cfg(test)]
mod x86var_lang {
    use ch5::x86var_lang as X86Var;
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
            let p1 = r#let!([x add!(add!(neg!(20), neg!(add!(10,neg!(15)))),12)]
                    add!(add!(30, neg!(15)), x));
            let v1 = interp_exp(&vec![], &p1);
            (p1, v1)
        };
        //println!("p1= {:?} ", p1);

        use RVarAnf::{interp_exp, rco_exp};
        let p1anf = rco_exp(&p1);
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
        assert_eq!(X86Var::Value::from(v1), x86val);

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
        assert_eq!(X86Var::Value::from(v1), x86val);

        let x86patched = X86Var::patch_prog(x86homes);
        println!("***patched_cfg***:");
        print_cfg(&x86patched.cfg);
        let x86val = X86Var::interp_prog(&x86patched);
        assert_eq!(X86Var::Value::from(v1), x86val);

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
            let p1 = r#let!([v 1]
                       r#let!([w 42]
                        r#let!([x add!(v,7)]
                         r#let!([y x]
                          r#let!([z add!(x,w)] add!(z, neg!(y)))))));
            let v1 = interp_exp(&vec![], &p1);
            (p1, v1)
        };
        //println!("p1= {:?} ", p1);

        use RVarAnf::{interp_exp, rco_exp};
        let p1anf = rco_exp(&p1);
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
        assert_eq!(X86Var::Value::from(v1), x86val);

        use X86Var::*;
        let x86cfg = liveness_analysis(x86cfg);
        let ginterfere = interference_graph(&x86cfg);
        let gbias = move_bias(&x86cfg);
        let regs = reg_alloc(&ginterfere, &gbias);
        let x86cfg = x86cfg.regs(regs);
        let x86homes = assign_homes(x86cfg);
        let x86val = X86Var::interp_prog(&x86homes);
        assert_eq!(X86Var::Value::from(v1), x86val);
        let x86patched = X86Var::patch_prog(x86homes);
        let x86val = X86Var::interp_prog(&x86patched);
        assert_eq!(X86Var::Value::from(v1), x86val);
        let asmstr = X86Var::print_x86prog(&x86patched);
        println!("\n{}", asmstr);
        println!("result={:?}", v1);
    }

    #[test]
    fn t4() {
        let (e1, v1) = {
            use RVar::*;
            let e1 = let_!([x 1]
                        let_!([y 101]
                          r#if!( r#if!(or!(eq!(x,1), lt!(read!(),5)),
                                       eq!(y,101), eq!(y,102)) ,
                                        add!(y,20),
                                        add!(y,30))));

            let v1 = interp_exp(&vec![], &e1);
            (e1, v1)
        };
        println!("e1= {:?} ", e1);
        println!("v1= {:?} ", v1);

        use RVarAnf::{interp_exp, rco_exp};
        let e1anf = rco_exp(&e1);
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
        assert_eq!(X86Var::Value::from(v1), x86val);

        use X86Var::*;
        let x86cfg = liveness_analysis(x86cfg);
        let ginterfere = interference_graph(&x86cfg);
        let gbias = move_bias(&x86cfg);
        let regs = reg_alloc(&ginterfere, &gbias);
        let x86cfg = x86cfg.regs(regs);
        let x86val = X86Var::interp_prog(&x86cfg);
        assert_eq!(X86Var::Value::from(v1), x86val);
        let x86homes = assign_homes(x86cfg);
        let x86val = X86Var::interp_prog(&x86homes);
        assert_eq!(X86Var::Value::from(v1), x86val);
        let x86patched = X86Var::patch_prog(x86homes);
        let x86val = X86Var::interp_prog(&x86patched);
        assert_eq!(X86Var::Value::from(v1), x86val);
        let asmstr = X86Var::print_x86prog(&x86patched);
        println!("\n{}", asmstr);
        println!("result={:?}", v1);
    }

    #[test]
    fn t5() {
        let (e1, v1) = {
            use RVar::*;
            let e1 = let_!([x 1]
                        let_!([y 101]
                          r#if!( let_!([x r#if!(or!(eq!(x,1), lt!(read!(),5)),
                                       eq!(y,101), eq!(y,102))] not!(not!(x))),
                                        add!(y,20),
                                        add!(y,30))));

            let v1 = interp_exp(&vec![], &e1);
            (e1, v1)
        };
        println!("e1= {:?} ", e1);
        println!("v1= {:?} ", v1);

        use RVarAnf::{interp_exp, rco_exp};
        let e1anf = rco_exp(&e1);
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
        assert_eq!(X86Var::Value::from(v1), x86val);
        let x86homes = assign_homes(x86cfg);

        let x86val = X86Var::interp_prog(&x86homes);
        assert_eq!(X86Var::Value::from(v1), x86val);
        let x86patched = X86Var::patch_prog(x86homes);
        let x86val = X86Var::interp_prog(&x86patched);
        assert_eq!(X86Var::Value::from(v1), x86val);
        let asmstr = X86Var::print_x86prog(&x86patched);
        println!("\n{}", asmstr);
        println!("result={:?}", v1);
    }
}
