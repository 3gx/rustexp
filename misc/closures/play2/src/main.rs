fn vec_with_closure<'a, T: 'a>(mut f: Box<dyn FnMut(T) + 'a>) -> Vec<Box<dyn FnMut(T) + 'a>>
{
    let mut v = Vec::<Box<dyn FnMut(T)>>::new();
    v.push(Box::new(move |t: T| {
        f(t);
    }));
    v
}

fn main() {
    let mut v = vec_with_closure(Box::new(|t: usize| {
        println!("{}", t);
    }));
    for c in v.iter_mut() {
        c(10);
    }
}
