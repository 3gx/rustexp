// https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=7d046df70e773a9c461b81505f6dc389

fn fix<T, R, F: Fn(&dyn Fn(T) -> R, T) -> R>(f: &F, t: T) -> R {
    f(&|t| fix(f, t), t)
}

fn main() {
    let init = 1;
    println!(
        "{}",
        fix(&|f, n| if n == 1 { init } else { n * f(n - 1) }, 5)
    )
}
