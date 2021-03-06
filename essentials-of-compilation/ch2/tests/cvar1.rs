#![feature(box_syntax)]

#[cfg(test)]
mod cvar_lang {
    #[test]
    fn t1() {
        use ch2::cvar_lang;
        use cvar_lang::*;
        let v = int!(42);
        println!("v= {:?}", v);
        let v = var!(x);
        println!("v= {:?}", v);

        let exp = add!(10, 32);
        println!("expr= {:?}", exp);
        println!("res= {}", interp_expr(&cvar_lang::env![], &exp));

        let prog = CProgram(vec![], vec![("start".to_string(), Tail::Return(exp))]);
        println!("prog= {:?}", prog);
        println!("eval= {:?}", interp_prog(&prog));
    }

    #[test]
    fn t2() {
        use ch2::cvar_lang;
        use cvar_lang::{rvar_anf_lang, rvar_lang};
        //        use ch2::rvar_anf_lang::{interp_exp, rco_exp};
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
