use std::collections::HashMap;

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

    let s: String;
    let data = "initial contents";
    s = data.to_string();
    println!("s is {}", s);

    let s = String::from("initial contents 2");
    println!("s is {}", s);

    let mut s = String::from("foo");
    s.push_str("bar");
    println!("s is {}", s);

    s.push('l');
    println!("s is {}", s);

    let s2 = String::from(" oops");
    let s3 = s + &s2;
    println!("s3 is {}", s3);


    //    let hello = "helloworld";
    let hello = "здраствуйте";
    let s = &hello[0..4];
    println!("s is {}" ,s);
    for (i,c) in hello.chars().enumerate() {
        println!("{}: {}", i, c);
    }
    for (i,c) in hello.bytes().enumerate() {
        println!("{}: {}", i, c);
    }

    let mut scores = HashMap::new();

    scores.insert(String::from("Blue"), 10);
    scores.insert(String::from("Yellow"), 50);
    println!("scores is {:#?}", scores);

    let teams = vec![String::from("Blue"), String::from("Yellow")];
    let initial_scores = vec![10, 50];

    let scores: HashMap<_, _> =
        teams.into_iter().zip(initial_scores.into_iter()).collect();
    println!("scores is {:#?}", scores);

    let field_name = String::from("Favorite color");
    let field_value = String::from("Blue");

    let mut map = HashMap::new();
    map.insert(field_name, field_value);
    println!("map is {:#?}", map);
    // field_name and field_value are invalid at this point, try using them and
    //println!("{:?}", field_name);

    let team_name = String::from("Blue");
    let score = scores.get(&team_name);
    println!("score is {:?}" ,score);
    let score = scores.get("Green");
    println!("score is {:?}" ,score);
    let score = scores.get("Yellow");
    println!("score is {:?}" ,score);

    for (key, value) in &scores {
        println!("{}: {}", key, value);
    }

    let mut scores = scores;
    scores.insert(String::from("Blue"), 25);
    println!("scores is {:#?}", scores);

    let score = scores.entry(String::from("Blue")).or_insert(50);
    println!("score is {}", score);
    let score = scores.entry(String::from("Green")).or_insert(70);
    println!("score is {}", score);
    println!("scores is {:#?}", scores);

    let text = "hello world wonderful world";
    println!("text is {}", text);

    let mut map = HashMap::new();

    for word in text.split_whitespace() {
        let count = map.entry(word).or_insert(0);
        *count += 1;
    }

    println!("{:?}", map);

    let mut map = HashMap::new();

    for (i, word) in text.split_whitespace().enumerate() {
         map.entry(word).or_insert(i);
    }

    println!("{:?}", map)

}
