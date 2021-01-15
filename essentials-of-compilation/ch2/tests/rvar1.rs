#![feature(box_patterns)]
#![feature(box_syntax)]

#[cfg(test)]
mod rvar_lang {
    #[test]
    fn t1() {
        use ch2::rvar_lang::*;
        let v = var!("x");
        println!("v= {:?}", v);
    }

    #[test]
    fn t2() {
        use ch2::rvar_lang::*;

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
        let t = if t { false } else { true };
        if t {
            println!("inter 52<enter>, 10<enter>, should get 42");
            let v1 = interp_program(&p1);
            println!("v1= {:?} ", v1);
        }
    }

    #[test]
    fn t3() {
        use ch2::rvar_lang::sym;
        use ch2::rvar_lang::*;
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
    /*
    fn is_lit(&self) -> bool {
        match self {
            Self::Lit(..) => return true,
            Self::Unary(expr_unary) => {
                if let ast::ExprUnary {
                    op: ast::UnOp::Neg,
                    expr,
                    ..
                } = &**expr_unary
                {
                    if let Self::Lit(expr) = expr {
                        return matches!(
                            &**expr,
                            ast::ExprLit {
                                lit: ast::Lit::Number(..),
                                ..
                            }
                        );
                    }
                }
            }
            _ => (),
        }

        false
    }
    fn is_lit1(&self) -> bool {
        match self {
            Self::Lit(..) => return true,
            Self::Unary(box ast::ExprUnary {
                op: ast::UnOp::Neg,
                expr: Self::Lit(expr),
                ..
            }) => {
                return matches!(
                    &**expr,
                    ast::ExprLit {
                        lit: ast::Lit::Number(..),
                        ..
                    }
                )
            }
            _ => (),
        }
        false
    }
    fn is_lit1(&self) -> bool {
        mymatch! {|self|
            Self::Lit(..) => return true,
            Self::Unary(ref ast::ExprUnary {
                op: ast::UnOp::Neg,
                expr: ref Self::Lit(expr),
                ..
            }) =>
                return matches!(
                    &**expr,
                    ast::ExprLit {
                        lit: ast::Lit::Number(..),
                        ..
                    }
                ),
            _ => (),
        }
        false
    }
    */
}
