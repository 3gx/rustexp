#[cfg(test)]
mod x86var_lang {
    #[test]
    fn t0() {
        use ch2::x86var_lang;
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

        let cprog = cvar_lang::CProgram(vars, vec![("start".to_string(), tail)]);

        let v1clang = cvar_lang::interp_prog(&cprog);
        println!("v1clang= {}", v1clang);
        assert_eq!(v1anf, v1clang);
    }
}
