#[derive(Debug)]
enum IpAddrKind {
    V4,
    V6,
}

#[derive(Debug)]
struct IpAddr {
    kind: IpAddrKind,
    address: String,
}

#[derive(Debug)]
enum IpAddr1 {
    V4(String),
    V6(String),
}

#[derive(Debug)]
enum Message {
    Quit,
    Move { x: i32, y: i32 },
    Write(String),
    ChangeColor(i32, i32, i32),
}

impl Message {
    fn call(&self) {}
}

#[derive(Debug)]
enum UsState {
    Alabama,
    Alaska,
}

#[derive(Debug)]
enum Coin {
    Penny,
    Nickel,
    Dime,
    Quarter(UsState),
    Other,
}

fn value_in_cent(coin: Coin) -> u8 {
    match coin {
        Coin::Penny => {
            println!("Lucky penny!");
            1
        }
        Coin::Nickel => 5,
        Coin::Dime => 10,
        Coin::Quarter(state) => {
            println!("State quarter {:?}!", state);
            25
        }
        _ => {
            println!("Unhandled coin {:?}", coin);
            0
        }
    }
}

fn main() {
    let home = IpAddr {
        kind: IpAddrKind::V4,
        address: String::from("127.0.0.1"),
    };
    let loopback = IpAddr {
        kind: IpAddrKind::V6,
        address: String::from("::1"),
    };
    println!("home= {:#?}", home);
    println!("loopback= {:#?}", loopback);

    let home = IpAddr1::V4(String::from("127.0.0.1"));
    let lb = IpAddr1::V6(String::from("::1"));
    println!("home= {:#?}", home);
    println!("lb= {:#?}", lb);

    let _quit = Message::Quit;
    let _move = Message::Move { x: 32, y: 64 };
    let _write = Message::Write(String::from("abc"));
    let _col = Message::ChangeColor(1, 2, 3);
    println!("quit= {:#?}", _quit);
    println!("move= {:#?}", _move);
    println!("write= {:#?}", _write);
    println!("col= {:#?}", _col);

    let m = Message::Write(String::from("hello"));
    m.call();

    let some_number = Some(5);
    let some_string = Some("a string");
    let absent_number: Option<i32> = None;

    println!("{:?} {:?} {:?}", some_number, some_string, absent_number);

    let x: i8 = 5;
    let y: Option<i8> = Some(10);
    let sum = x + y.unwrap();
    println!("sum= {}", sum);

    println!(
        "{:?} {:?} {:?} {:?}",
        Coin::Penny,
        Coin::Nickel,
        Coin::Dime,
        Coin::Quarter(UsState::Alaska)
    );
    println!("{}", value_in_cent(Coin::Penny));
    println!("{}", value_in_cent(Coin::Quarter(UsState::Alabama)));
    println!("{}", value_in_cent(Coin::Other));

    let some3 = Some(3);
    if let Some(3) = some3 {
        println!("some 3")
    }
}
