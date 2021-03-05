
fn gf_random() -> f64 { 0.2 /*asked gf for random number betwee 0 and 1 */ }

fn random_choice<T>(a: T, b: T) -> T {
    if gf_random() < 0.5 { a } else { b }
}

// This is what we want to implement with Rank-2 types:
/*
fn higher_random_choice<T, U>(a: T, b: T, f: U) -> T
    where U: for<X> Fn(X, X) -> X
{
    if (f(true, false)) {
        f(a, b)
    } else {
        f(b, a)
    }
}
*/

// Instead of using a Rank-2 type, we just constrain a type with a trait for
// the types we want to use:
fn higher_random_choice<T, F: RandomChoice>(a: T, b: T, f: F) -> T
{
    if f.random_choice(true, false) {
        f.random_choice(a, b)
    } else {
        f.random_choice(b, a)
    }
}

// The trait just lifts the function into a trait:
trait RandomChoice {
    fn random_choice<T>(&self, a: T, b: T) -> T {
        random_choice(a, b)
    }
}

// A blanket impl then suffices:
struct RC;
impl RandomChoice for RC {}

fn main() {
    let _ = higher_random_choice(0., 1., RC);
     let _ = higher_random_choice("a", "b", RC);
}
