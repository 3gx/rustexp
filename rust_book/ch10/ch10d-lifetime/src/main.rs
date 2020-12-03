fn main() {
    println!("Hello, world!");

    let r;

    {
        let x = 5;
        r = &x;
        println!("r: {}", r);
    }

//    println!("r: {}", r); // `rustc --explain E0597`
}
