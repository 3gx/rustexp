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
    Move { x : i32 , y : i32 },
    Write(String),
    ChangeColor(i32,i32,i32),
}

impl Message {
    fn call(&self) {
    }
}

fn main() {
  let home = IpAddr{
    kind : IpAddrKind::V4,
    address : String::from("127.0.0.1"),
  };
  let loopback = IpAddr {
      kind:  IpAddrKind::V6,
      address: String::from("::1"),
  };
  println!("home= {:#?}", home);
  println!("loopback= {:#?}", loopback);

  let home = IpAddr1::V4(String::from("127.0.0.1"));
  let lb = IpAddr1::V6(String::from("::1"));
  println!("home= {:#?}", home);
  println!("lb= {:#?}", lb);

  let _quit = Message::Quit;
  let _move = Message::Move{x : 32, y: 64};
  let _write =Message::Write(String::from("abc"));
  let _col = Message::ChangeColor(1,2,3);
  println!("quit= {:#?}", _quit);
  println!("move= {:#?}", _move);
  println!("write= {:#?}", _write);
  println!("col= {:#?}", _col);

  let m = Message::Write(String::from("hello"));
  m.call()
}
