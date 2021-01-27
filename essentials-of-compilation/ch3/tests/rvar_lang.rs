#[cfg(test)]
mod rvar_lang {
    #[test]
    fn t0() {
        use ch3::rvar_lang::*;
        let p1 = program![r#let!([x add!(12, 20)]  add!(10, x))];
        println!("p1= {:?} ", p1);
        let v1 = interp_program(&p1);
        println!("v1= {:?} ", v1);

        let p1 = program![r#let!([x 2]  add!(r#let!([x 10]  x), x))];
        println!("p1= {:?} ", p1);
        let v1 = interp_program(&p1);
        println!("v1= {:?} ", v1);

        let p1 = program![r#let!(
            [x read!()]
            r#let!([y read!()]  add!(x, neg!(y)))
        )];
        println!("p1= {:?} ", p1);
        let t = true;
        let t = !t;
        if t {
            println!("inter 52<enter>, 10<enter>, should get 42");
            let v1 = interp_program(&p1);
            println!("v1= {:?} ", v1);
        }
    }

    #[test]
    fn t1() {
        use ch3::rvar_lang::sym;
        use ch3::rvar_lang::*;
        let e1 = r#let!([x 32]  add!(r#let!([x 10] x), x));
        let e2 = r#let!([x1 32]  add!(r#let!([x2 10] x2), x1));
        println!("e1= {:?}", e1);
        println!("e2= {:?}", e2);
        let e1u = uniquify_expr(&sym![], &e1);
        println!("e1u= {:?}", e1u);
        assert_eq!(e2, e1u);

        gensym_reset();
        let e1 = r#let!([x 32]  add!(r#let!([y 10] y), x));
        let e2 = r#let!([x1 32]  add!(r#let!([y1 10] y1), x1));
        println!("e1= {:?}", e1);
        println!("e2= {:?}", e2);
        let e1u = uniquify_expr(&sym![], &e1);
        println!("e1u= {:?}", e1u);
        assert_eq!(e2, e1u);
    }

    #[test]
    fn t2() {
        use ch3::rvar_lang;
        let (p1, v1) = {
            // not the same as
            // use ch2::rvar_lang::*;
            use rvar_lang::*;
            let p1 = r#let!([x add!(12, add!(neg!(20), neg!(add!(10,neg!(15)))))]
                    add!(add!(30, neg!(15)), x));
            let v1 = interp_exp(&vec![], &p1);
            (p1, v1)
        };
        println!("p1= {:?} ", p1);
        println!("v1= {:?} ", v1);
    }
}
