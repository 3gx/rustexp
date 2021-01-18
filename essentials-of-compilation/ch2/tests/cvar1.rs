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

        let exp = Exp::Prim(add!(10, 32));
        println!("expr= {:?}", exp);
        println!("res= {}", interp_exp(&cvar_lang::env![], &exp));
    }
}
