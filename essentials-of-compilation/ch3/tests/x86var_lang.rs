#[cfg(test)]
mod x86var_lang {
    #[test]
    fn t0() {
        use ch3::x86var_lang;
        use cvar_lang::rvar_anf_lang;
        use x86var_lang::cvar_lang;
        use x86var_lang::rvar_lang;
        let (p1, v1) = {
            use rvar_lang::*;
            let p1 = r#let!([x add!(12, add!(neg!(20), neg!(add!(10,neg!(15)))))]
                    add!(add!(30, neg!(15)), x));
            let v1 = interp_exp(&vec![], &p1);
            (p1, v1)
        };
        println!("p1= {:#?} ", p1);

        use rvar_anf_lang::{interp_exp, rco_exp};
        let p1anf = rco_exp(&p1);
        println!("p1= {:#?} ", p1anf);

        let v1anf = interp_exp(&vec![], &p1anf);

        println!("v1anf= {}", v1anf);
        assert_eq!(v1, v1anf);

        let (tail, vars) = cvar_lang::explicate_tail(&p1anf);
        println!("vars= {:?}", vars);
        println!("tail= {:#?}", tail);

        let cprog = cvar_lang::CProgram(vars.clone(), vec![("start".to_string(), tail.clone())]);

        let v1clang = cvar_lang::interp_prog(&cprog);
        println!("v1clang= {}", v1clang);
        assert_eq!(v1anf, v1clang);

        let x86var = x86var_lang::select_inst_tail(&tail, x86var_lang::Block::with_vars(vars));
        println!("x86var= {:?}", x86var);
        let val_x86var = x86var_lang::interp_block(&x86var);
        println!("eval(x86var)= {}", val_x86var);
        assert_eq!(v1, val_x86var);

        fn print_vec<T: std::fmt::Debug>(list: &Vec<T>) {
            for el in list {
                println!("\t{:?}", el);
            }
        }
        print_vec(&x86var.1);

        let x86var_lang::Block(vars, _) = &x86var;
        println!("vars= {:?}", vars);

        let x86var_home = x86var_lang::assign_homes(&x86var);
        println!("assgned_homes= {:?}", x86var_home);
        print_vec(&x86var_home.1);

        let val_x86var_stack = x86var_lang::interp_block_stack(&x86var_home);
        println!("eval(x86var_home)= {}", val_x86var_stack);
        assert_eq!(v1, val_x86var_stack);

        let x86var_patched = x86var_lang::patch_x86(&x86var_home);
        println!("x86var_patched= {:?}", x86var_patched);
        let val_x86var_patched = x86var_lang::interp_block_stack(&x86var_patched);
        assert_eq!(v1, val_x86var_patched);
        print_vec(&x86var_patched.1);

        println!("\n{}", x86var_lang::print_x86(&x86var_patched).as_str());
    }
}
