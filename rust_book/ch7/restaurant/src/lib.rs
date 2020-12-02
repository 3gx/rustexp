#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}

mod front_of_house;
pub use front_of_house::hosting;

pub fn eat_at_restaurant() {
    // absolut path
    crate::front_of_house::hosting::add_to_waitlist();

    // relative path, idiomatic use
    hosting::add_to_waitlist();
}

use rand::Rng;

pub fn do_something() {
    let secret_number = rand::thread_rng().gen_range(1, 101);
    println!("secret_number= {}", secret_number);
}
