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
        println!("eval= {:?}", inter_prog(&prog));
    }
}
