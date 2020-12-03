fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() {
        x
    } else {
        y
    }
}

use std::fmt::Display;

fn longest_with_an_announcement<'a, T>(
    x: &'a str,
    y: &'a str,
    ann: T,
) -> &'a str
where
    T: Display,
{
    println!("Announcement! {}", ann);
    if x.len() > y.len() {
        x
    } else {
        y
    }
}

#[derive(Debug)]
struct ImportantExcerpt<'a> {
        part: &'a str,
}

#[derive(Debug)]
struct ImportantExcerpt1 {
        part: &'static str,
}

impl<'a> ImportantExcerpt<'a> {
    fn level(&self) -> i32 {
        3
    }
}

impl<'a> ImportantExcerpt<'a> {
    fn do1<'b>(&'a self, announcement: &'b str) -> &'b str {
        println!("Attention please: {}", announcement);
        announcement
    }
    fn do2(&self, announcement: & str) -> & str {
        println!("Attention2 please: {}", announcement);
        self.part
    }
}


fn main() {
    println!("Hello, world!");

    let r;

    {
        let x = 5;
        r = &x;
        println!("r: {}", r);
    }

    //    println!("r: {}", r); // `rustc --explain E0597`

    let string1 = String::from("abcd");
    let string2 = "xyz";

    let result = longest(string1.as_str(), string2);
    println!("The longest string is '{}'", result);
    
    let result = longest_with_an_announcement(string1.as_str(), string2, "wow");
    println!("The longest_ann string is '{}'", result);

    let string1 = String::from("long string is long");

    let _result1;
    {
        let string2 = String::from("xyz");
        let result = longest(string1.as_str(), string2.as_str());
        _result1 = longest(string1.as_str(), string2.as_str());
        println!("The longest string is '{}'", result);
    }
    //        println!("The longest string is '{}'", _result1); //  `rustc --explain E0597`.

    let novel = String::from("Call me Ishmael. Some years ago...");
    let first_sentence = novel.split('.').next().expect("Could not find a '.'");
    let i = ImportantExcerpt {
        part: first_sentence,
    };
    println!("i= {:?} level={}", i, i.level());
    println!("== {} = ", i.do1("test"));
    println!("== {} = ", i.do2("test"));
//    let i = ImportantExcerpt1 {        part: first_sentence,     }; // fails
//    println!("i= {:?}", i);
}
