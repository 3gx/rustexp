#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }

    use super::*;
    #[test]
    fn addone() {
        assert_eq!(3, add_one(2));
    }
}

pub fn add_one(x: i32) -> i32 {
    x + 1
}
