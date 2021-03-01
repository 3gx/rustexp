#![allow(dead_code)]
#![allow(unused_variables)]

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
struct RefS<'a>(&'a E<S>);
struct RefT<'a>(&'a E<T>, &'a U);

impl<'a> From<&'a S> for RefS<'a> {
    fn from(item: &'a S) -> Self {
        RefS(&item.0)
    }
}
impl<'a> From<&'a T> for RefT<'a> {
    fn from(item: &'a T) -> Self {
        RefT(&item.0, &item.1)
    }
}
fn common_s(RefS(e) : RefS) {
  // .. use e
}
fn fun_s1(s : &S) {  // this works
    let S(e) = s; 
    //let _ : () = e; // typeof(e) = &E<S>
    common_s(s.into());
}
fn fun_s2(S(e) : &S) {  // but prefer this syntax, if possible
    //let _ : () = e; // typeof(e) = &E<S>
    common_s( RefS(e) ) // .. Q: is it possible to construct &S(E<S>) from &E<S> ?
}


fn common_t(RefT(e,u) : RefT) {
  // .. use t & e
}
fn fun_t1(t : &T) {
    let T(e,u) = t; 
    //let _ : () = (e,u); // typeof(e) = &E<T>, typeof(u) = &U
    common_t(t.into());
}
fn fun_t2(T(e,u) : &T) {
    // let _ : () = (e,u); // typeof(e) = &E<T>, typeof(u) = &U
    common_t( RefT(e,u) ) // .. Q: is it possible to construct &T(E<T>,U) from &E<T>, &U ?
}

