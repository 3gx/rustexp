#[cfg(test)]
mod tests {
    #[test]
    fn t1() {
        use ch2::rvar_lang::*;
        let v = var!("x");
        println!("v= {:?}", v);
    }

    #[test]
    fn t2() {
        use ch2::rvar_lang::*;

        let p1 = program![r#let!([x <- plus!(12, 20)]  plus!(10, x))];
        println!("p1= {:?} ", p1);
        let v1 = interp_program(&p1);
        println!("v1= {:?} ", v1);

        let p1 = program![r#let!([x <- 2]  plus!(r#let!([x <- 10]  x), x))];
        println!("p1= {:?} ", p1);
        let v1 = interp_program(&p1);
        println!("v1= {:?} ", v1);

        let p1 = program![r#let!(
            [x <- read!()]
            r#let!([y <- read!()]  plus!(x, neg!(y)))
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
}
