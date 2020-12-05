use add_one;
use rand;
use rand::Rng;

fn main() {
    let num = rand::thread_rng().gen_range(1, 101);
    println!(
        "Hello, world! {} plus one is {}!",
        num,
        add_one::add_one(num)
    );
}
