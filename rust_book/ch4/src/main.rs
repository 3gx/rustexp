fn main() {
    println!("Hello, world!");
    let s1 = String::from("hello");
    let s2 = s1.clone();
    println!("s1= {}", s1);
    println!("s2= {}", s2);

    let mut s = String::from("hello");
    {
        let r1 = &mut s;
        println !("r1= {}", r1);
    }
    let r2 = &mut s;
    println!("r2= {}", r2);
    let r3= &s;
    println!("r3= {}", r3);

    let s = String::from("hello world");

    let hello = &s[0..5];
    let world = &s[6..11];

    println!("hello= {}", hello);
    println!("world= {}", world);

    let mut s = String::from("hello world");
    s.push_str(" yay!");
    let word = first_word(&s);
    fn first_word(s: &String) -> &str {
        let bytes = s.as_bytes();
        for (i, &item) in bytes.iter().enumerate() {
            if item == b' ' {
                return &s[0..i];
            }
        }
        &s[..]
    }
    println!("the first word is: {}", word);
}
