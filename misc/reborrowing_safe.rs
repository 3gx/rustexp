#![allow(dead_code)]
#![allow(unused_variables)]
#![feature(bindings_after_at)]

enum E<T> {
    E0,
    E1(Box<T>),
    E2(Box<T>, Box<T>),
}
enum U {
    U0,
    U1,
    U2,
}
struct S(E<S>);
struct T(E<T>, U);

fn common_s(S(e) : &S) {
  // .. use e
}
fn fun_s1(s : &S) {  // this works
    let S(e) = s; 
    //let _ : () = e; // typeof(e) = &E<S>
    common_s(s);
}
fn fun_s2(s@S(e) : &S) {  // but prefer this syntax, if possible
    // let _ : () = e; // typeof(e) = &E<S>
    // let _ : () = s; // typeof(s) = &S<E<S>>
    common_s( s );
}


fn common_t(T(e,u) : &T) {
  // .. use t & e
}
fn fun_t1(t : &T) {
    let T(e,u) = t; 
    //let _ : () = (e,u); // typeof(e) = &E<T>, typeof(u) = &U
    common_t(t);
}
fn fun_t2(t@T(e,u) : &T) {
    // let _ : () = (e,u); // typeof(e) = &E<T>, typeof(u) = &U
    // let _ : () = t; // typeof(t) = &T(E<T>,U)
    common_t( t ) // .. Q: is it possible to construct &T(E<T>,U) from &E<T>, &U ?
}


