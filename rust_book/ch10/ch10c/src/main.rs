use ch10c::{NewsArticle, Summary, Summary1, Tweet, notify, notify1};

pub struct Tweet1 {
    pub username: String,
    pub content: String,
    pub reply: bool,
    pub retweet: bool,
}

impl Summary for Tweet1 {
    fn summarize(&self) -> String {
        format!("{}: {}", self.username, self.content)
    }
}

struct MyTy<T>(T);
impl<T: Summary> Summary1 for MyTy<T> {}
// impl<T: Summary> Summary1 for T {} // `rustc --explain E0210`

use std::fmt::Display;

struct Pair<T> {
    x: T,
    y: T,
}

impl<T> Pair<T> {
    fn new(x: T, y: T) -> Self {
        Self { x, y }
    }
}

impl<T: Display + PartialOrd> Pair<T> {
    fn cmp_display(&self) {
        if self.x >= self.y {
            println!("The largest member is x = {}", self.x);
        } else {
            println!("The largest member is y = {}", self.y);
        }
    }
}

fn main() {
    let tweet = Tweet {
        username: String::from("horse_ebooks"),
        content: String::from(
            "of course, as you probably already know, people",
        ),
        reply: false,
        retweet: false,
    };

    println!("1 new tweet: {}", tweet.summarize());

    let tweet = Tweet1 {
        username: String::from("horse_ebooks"),
        content: String::from(
            "of course, as you probably already know, people",
        ),
        reply: false,
        retweet: false,
    };

    println!("1 new tweet1: {}", tweet.summarize());

        let article = NewsArticle {
        headline: String::from("Penguins win the Stanley Cup Championship!"),
        location: String::from("Pittsburgh, PA, USA"),
        author: String::from("Iceburgh"),
        content: String::from(
            "The Pittsburgh Penguins once again are the best \
             hockey team in the NHL.",
        ),
    };

    println!("New article available! {}", article.summarize());

    notify(&article);
    notify(&tweet);

    notify1(&article);
    notify1(&tweet);

    let pair = Pair::new(3,4);
    pair.cmp_display();
    let pair = Pair::new(6,4);
    pair.cmp_display();

    let s = 3.to_string();
    println!("s is {}", s);
}
