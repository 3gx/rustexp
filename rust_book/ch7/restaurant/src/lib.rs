#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}

fn serve_order() {}

mod front_of_house {
    pub mod hosting {
        pub fn add_to_waitlist() {
            super::super::serve_order();
        }
    }
}

pub fn eat_at_restaurant() {
    // absolut path
    crate::front_of_house::hosting::add_to_waitlist();

    // relative path
    front_of_house::hosting::add_to_waitlist();
}
