fn largest_i32(list: &[i32]) -> &i32 {
    let mut largest = &list[0];

    for item in list {
        if item > largest {
            largest = item;
        }
    }

    largest
}

fn largest_char(list: &[char]) -> &char {
    let mut largest = &list[0];

    for item in list {
        if item > largest {
            largest = item;
        }
    }

    largest
}

fn largest<T: std::cmp::PartialOrd>(list: &[T]) -> &T {
    let mut largest: &T = &list[0];

    for item in list {
        if item > largest {
            largest = item;
        }
    }

    largest
}

fn largest1<T: PartialOrd + Copy>(list: &[T]) -> T {
    let mut largest: T = list[0];

    for &item in list {
        if item > largest {
            largest = item;
        }
    }

    largest
}

fn largest2<T: PartialOrd + Clone>(list: &[T]) -> T {
    let mut largest: T = list[0].clone();

    for item in list {
        if item.clone() > largest {
            largest = item.clone()
        }
    }

    largest
}

#[derive(Debug)]
struct Point<T> {
    x: T,
    y: T,
}

#[derive(Debug)]
struct Point1<T, U> {
    x: T,
    y: U,
}

impl<T> Point<T> {
    fn x(&self) -> &T {
        &self.x
    }
}
impl Point<f32> {
    fn xx(&self) -> f32 {
        2.1 * self.x
    }
}

#[derive(Debug, Clone, Copy)]
struct Point2<T, U> {
    x: T,
    y: U,
}

impl<T, U> Point2<T, U> {
    fn mixup<V, W>(self, other: Point2<V, W>) -> Point2<T, W> {
        Point2 {
            x: self.x,
            y: other.y,
        }
    }
}

fn main() {
    let number_list = vec![34, 50, 25, 100, 65];

    let result = largest_i32(&number_list);
    println!("The largest number is {}", result);
    let result = largest(&number_list);
    println!("The largest number is {}", result);

    let char_list = vec!['a', 'm', 'y', 'q'];

    let result = largest_char(&char_list);
    println!("The largest char is {}", result);
    let result = largest(&char_list);
    println!("The largest char is {} : {:?}", result, char_list);
    let result = largest1(&char_list);
    println!("The largest1 char is {}", result);
    let result = largest2(&char_list);
    println!("The largest2 char is {}", result);

    let integer = Point { x: 5, y: 10 };
    let float = Point { x: 1.0, y: 4.0 };
    //    let mixed = Point { x: 1, y: 4.0 };  // fails
    println!("integer= {:?}", integer);
    println!("integer1= {:?}", integer.x());
    println!("float= {:#?}", float);
    println!("float1= {:#?}", float.xx());

    let mixed = Point1 { x: 1, y: 4.0 };
    println!("integer= {:?}", mixed);

    let p1 = Point2 { x: 5, y: 10.4 };
    let p2 = Point2 { x: "Hello", y: 'c' };

    println!("p1= {:?}  p2= {:?}", p1, p2);

    let p3 = p1.mixup(p2);
    let p4 = p2.mixup(p1);

    println!("p3.x = {}, p3.y = {}", p3.x, p3.y);
    println!("p4.x = {}, p4.y = {}", p4.x, p4.y);
}
