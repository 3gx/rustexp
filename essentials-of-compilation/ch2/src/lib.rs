#![allow(incomplete_features)]
#![feature(if_let_guard)]
#![feature(let_chains)]
#![feature(decl_macro)]
#![feature(associated_type_defaults)]
#![feature(box_patterns)]
#![feature(box_syntax)]

pub mod cvar_lang;
pub mod macros;
pub mod rvar_anf_lang;
pub mod rvar_lang;
pub mod rvar_lang_legacy;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
