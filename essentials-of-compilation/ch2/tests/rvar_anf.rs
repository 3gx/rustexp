#![feature(box_patterns)]
#![feature(box_syntax)]

#[cfg(test)]
mod rvar_anf_tests {
    #[test]
    fn t1() {
        use ch2::rvar_anf_lang::{interp_exp, rco_exp};
        let (p1, v1) = {
            // not the same as
            // use ch2::rvar_lang::*;
            use ch2::rvar_anf_lang::rvar_lang::*;
            let p1 = r#let!([x add!(12, add!(neg!(20), neg!(add!(10,neg!(15)))))]
                    add!(add!(30, neg!(15)), x));
            let v1 = interp_exp(&vec![], &p1);
            (p1, v1)
        };
        println!("p1= {:#?} ", p1);

        let p1anf = rco_exp(&p1);
        println!("p1= {:#?} ", p1anf);

        let v1anf = interp_exp(&vec![], &p1anf);

        println!("v1anf= {}", v1anf);
        assert_eq!(v1, v1anf);
    }
}
