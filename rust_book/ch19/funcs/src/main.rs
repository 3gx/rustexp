fn add_one(x: i32) -> i32 {
    x + 1
}
fn do_twice(f: fn(i32) -> i32, arg: i32) -> i32 {
    f(arg) + f(arg)
}
fn do_twice1(f: &dyn Fn(i32) -> i32, arg: i32) -> i32 {
    f(arg) + f(arg)
}

fn main() {
    let answer = do_twice(add_one, 5);
    println!("The answer is: {}", answer);

    let answer = do_twice1(&add_one, 5);
    println!("The answer is: {}", answer);

    let list_of_numbers = vec![1, 2, 3];
    let list_of_strings: Vec<String> = list_of_numbers.iter().map(|i| i.to_string()).collect();
    println!("ret= {:?}", list_of_strings);

    let list_of_strings: Vec<String> = list_of_numbers.iter().map(ToString::to_string).collect();
    println!("ret= {:?}", list_of_strings);

    #[derive(Debug)]
    enum Status {
        Value(u32),
        _Stop,
    }

    let list_of_statuses: Vec<Status> = (0u32..5).map(Status::Value).collect();
    println!("statuses= {:?}", list_of_statuses);

    fn return_closure() -> Box<dyn Fn(i32) -> i32> {
        Box::new(|x| x + 1)
    }
    let closure = return_closure();
    let answer = do_twice1(&closure, 5);
    println!("answer= {}", answer);
}
