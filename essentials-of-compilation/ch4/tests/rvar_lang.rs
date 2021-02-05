#[cfg(test)]
mod rvar_lang {
    #[test]
    fn t0() {
        use ch4::rvar_lang::*;
        let p1 = program![r#let!([x add!(12, 20)]  add!(10, x))];
        println!("p1= {:?} ", p1);
        let p1ty = type_check(&vec![], &p1.0);
        println!("type= {:?}", p1ty);
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
        use ch4::rvar_lang::sym;
        use ch4::rvar_lang::*;
        let e1 = r#let!([x 32]  add!(r#let!([x 10] x), x));
        let e2 = r#let!([x1 32]  add!(r#let!([x2 10] x2), x1));
        println!("e1= {:?}", e1);
        println!("e2= {:?}", e2);
        let e1ty = type_check(&vec![], &e1);
        let e2ty = type_check(&vec![], &e2);
        println!("e1ty= {:?}", e1ty);
        println!("e2ty= {:?}", e2ty);
        let e1u = uniquify_expr(&sym![], &e1);
        println!("e1u= {:?}", e1u);
        assert_eq!(e2, e1u);
        let e1u_ty = type_check(&vec![], &e1u);
        assert_eq!(e1ty, e1u_ty);

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
        use ch4::rvar_lang;
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

    #[test]
    fn t3() {
        use ch4::rvar_lang::*;
        let expr = let_! {[x 0]
        let_!{[y  100]
          if_!{if_!{lt!(x,1), eq!(x,0), eq!(x,2)},
               add!(y,2),
               add!(y,10)}}};
        println!("expr= {:?}", expr);
        let ety = type_check(&vec![], &expr);
        println!("ety= {:?}", ety);
        let prog = program![expr];
        let val = interp_program(&prog);
        println!("val= {:?}", val);
        assert_eq!(val, Value::Int(102));

        gensym_reset();
        let expr = let_! {[x 1]
        let_!{[y  100]
          r#if!{if_!{lt!(x,1), eq!(x,0), eq!(x,2)},
               add!(y,2),
               add!(y,10)}}};
        let ety = type_check(&vec![], &expr);
        println!("ety= {:?}", ety);
        let uexpr = uniquify_expr(&sym![], &expr);
        println!("uexpr= {:?}", uexpr);
        let prog = program![uexpr];
        let uval = interp_program(&prog);
        assert_eq!(uval, Value::Int(110));
    }
}
