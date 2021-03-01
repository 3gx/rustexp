// https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=d0b550a03e69e2de20d4a3ba27d0d53a
#![allow(dead_code)]
#![allow(unused_variables)]

enum E<T> {
    E0,
    E1(Box<T>),
    E2(Box<T>, Box<T>),
}

#[repr(transparent)]
struct S(E<S>);

fn common_s(S(e): &S) {
    // .. use e
}
fn fun_s1(S(e): &S) {
    let p_s: &S = unsafe { core::mem::transmute(e as *const E<_> as *const S) };
    common_s(p_s);
}
