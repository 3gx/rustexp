use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("{:?}", args);

    let querry = &args[1];
    let filename = &args[2];
    println!("Searching for '{}'", querry);
    println!("In file '{}'", filename);
}

