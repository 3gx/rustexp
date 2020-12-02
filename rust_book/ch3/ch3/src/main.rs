fn main() {
    println!("Hello, world!");
    let mut x = 5;
    println!("The value of x is: {}", x);
    x = 6; // without mut in line 3 it will fail to build
    println!("The value of x is: {}", x);
    let x = 7;
    println!("The value of x is: {}", x);
    let x = 8;
    println!("The value of x is: {}", x);
    let spaces = "   ";
    let spaces = spaces.len(); // w/o let it will fail to build
    println!("spaces= {}", spaces);
    let (x, y, z) = (32, 43.0, "abc");
    println!("x= {}  y= {}  z= {}", x, y, z);

    let x = 5;
    let y = {
        let x = 3;
        x + 1
    };
    println!("x is {}, y  is {}", x, y);

    let z = if x == 5 {
        42
    } else {
        println!("42");
        43
    };
    println!("z is {}", z);

    let mut counter = 0;
    let result = loop {
        counter += 1;
        if counter == 10 {
            break counter * 2;
        }
    };
    println!("result is {}", result);

    let mut number = 3;
    while number != 0 {
        println!("{}!", number);
        number -= 1;
    }
    println!("LIFTOFF!!!");

    let a = [10, 20, 30, 40, 50];
    for el in a.iter() {
        println!("the value is: {}", el);
    }

    let guess = "252".parse::<u8>().expect("Not a number");
    let guess = guess + 42; // fails with overflow in debug mode
    println!("guess is {}", guess);
}
