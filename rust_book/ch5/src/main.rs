#[derive(Debug)]
struct Rect {
    width: u32,
    height: u32,
}

impl Rect {
    fn mut_area(&mut self) -> u32 {
        self.width * self.height
    }
    fn area(&self) -> u32 {
        self.width * self.height
    }
    fn can_hold(&self, other: &Rect) -> bool {
        self.width > other.width && self.height > other.height
    }
    fn square(size: u32) -> Rect {
        Rect { width: size, height: size }
    }
}
fn main() {
    let width1 = 30;
    let height1 = 50;

    println!("The area of rectangis is {}", area(width1, height1));

    let rect1 = (30, 50);
    println!("the area1 is {}", area1(rect1));

    let mut rect2 = Rect {
        width: 30,
        height: 30,
    };
    println!("the area2 of {:#?} is {}", rect2, area2(&rect2));
    let a2 = rect2.mut_area();
    println!("the area of {:#?} is {}/{}", rect2, a2, rect2.area());

    let rect1 = Rect { width : 30, height : 50,};
    let rect2 = Rect { width : 10, height : 40,};
    let rect3 = Rect { width : 60, height : 45,};
    println!("rect1 holds rect2? {}", rect1.can_hold(&rect2));
    println!("rect1 holds rect3? {}", rect1.can_hold(&rect3));

    println!("square {:#?}", Rect::square(32));
}

fn area(width: u32, height: u32) -> u32 {
    width * height
}
fn area1(dims: (u32, u32)) -> u32 {
    dims.0 * dims.1
}
fn area2(rect: &Rect) -> u32 {
    rect.width * rect.height
}
