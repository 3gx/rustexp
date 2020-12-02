fn main() {
    let mut v: Vec<i32> = Vec::new();

    v.push(5);
    v.push(6);
    v.push(7);
    v.push(8);

    println!("Hello, world! {:?}", v);

    let v = vec![3,4,5,6];
    println!("v=  {:?}", v);

    let third = v[2];
    println!("third= {}", third);

    let nth = v.get(2);
    println!("nth= {:?}", nth);

    let nth = v.get(20);
    println!("nth= {:?}", nth);

    let mut v = vec![1,2,3,4,5];
    let first = &v[0];

    println!("first element is {}", first); // can't put after v.push..
    v.push(6);

    for i in &mut v {
        *i += 50;
    }
    println!("v is now {:?}", v);

}
