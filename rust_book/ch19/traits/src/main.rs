use std::ops::Add;

#[derive(Debug, PartialEq)]
struct Point {
    x: i32,
    y: i32,
}

impl Add for Point {
    type Output = Point;

    fn add(self, other: Point) -> Point {
        Point {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

trait MyAdd<Rhs = Self> {
    type Output;

    fn myadd(self, rhs: Rhs) -> Self::Output;
}

impl MyAdd for Point {
    type Output = Point;

    fn myadd(self, other: Point) -> Point {
        Point {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

#[derive(Debug)]
struct Millimeters(f32);
#[derive(Debug)]
struct Meters(f32);

impl Add<Meters> for Millimeters {
    type Output = Millimeters;

    fn add(self, other: Meters) -> Millimeters {
        Millimeters(self.0 + (other.0 * 1000.0))
    }
}
impl Add<Millimeters> for Meters {
    type Output = Meters;

    fn add(self, other: Millimeters) -> Meters {
        Meters(self.0 + (other.0 / 1000.0))
    }
}

trait Pilot {
    fn fly(&self);
}
trait Wizard {
    fn fly(&self);
}

struct Human;

impl Pilot for Human {
    fn fly(&self) {
        println!("This is your captain speaking.");
    }
}

impl Wizard for Human {
    fn fly(&self) {
        println!("Up!")
    }
}

impl Human {
    fn fly(&self) {
        println!("*waving arms furiously*");
    }
}

// w/o self
trait Animal {
    fn baby_name() -> String;
}

struct Dog;

impl Dog {
    fn baby_name() -> String {
        String::from("Spot")
    }
}

impl Animal for Dog {
    fn baby_name() -> String {
        String::from("puppy")
    }
}

use std::fmt;

trait OutlinePrint: fmt::Display {
    fn outline_print(&self) {
        let output = self.to_string();
        let len = output.len();
        println!("{}", "*".repeat(len + 4));
        println!("*{}*", " ".repeat(len + 2));
        println!("* {} *", output);
        println!("*{}*", " ".repeat(len + 2));
        println!("{}", "*".repeat(len + 4));
    }
}

struct Point1 {
    x: i32,
    y: i32,
}

impl OutlinePrint for Point1 {}

impl fmt::Display for Point1 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}

struct Wrapper(Vec<String>);

impl fmt::Display for Wrapper {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}]", self.0.join(", "))
    }
}

fn main() {
    assert_eq!(
        Point { x: 1, y: 0 } + Point { x: 2, y: 3 },
        Point { x: 3, y: 3 }
    );

    let x = Point { x: 1, y: 0 };
    println!("x= {:?}", x);
    let y = x + Point { x: 2, y: 3 };
    // println!("x= {:?}", x); // borrow of moved value x
    println!("x= {:?}", y);

    let x = Point { x: 1, y: 0 };
    let y = x.myadd(Point { x: 2, y: 3 });
    println!("x= {:?}", y);

    let meter = Meters(1.0);
    let mmeter = Millimeters(32.);
    let result = mmeter + meter;
    println!("{:?}", result);

    let meter = Meters(1.);
    let mmeter = Millimeters(32.);
    let result = meter + mmeter;
    println!("{:?}", result);

    let person = Human;
    Pilot::fly(&person);
    Wizard::fly(&person);
    person.fly();

    println!("A baby dog is called a {}", Dog::baby_name());
    println!("A baby dog is called a {}", <Dog as Animal>::baby_name());

    let point = Point1 { x: 1, y: 3 };
    println!("point= {}", point);
    point.outline_print();

    let w = Wrapper(vec![String::from("hello"), String::from("world")]);
    println!("w = {}", w);
}
