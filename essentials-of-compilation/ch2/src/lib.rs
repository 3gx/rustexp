#![allow(incomplete_features)]
#![feature(if_let_guard)]

#[macro_use]
pub mod rvar_lang;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
