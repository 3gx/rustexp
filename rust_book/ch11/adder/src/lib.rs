#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
        assert!(3 == 4 - 1);
    }

    #[test]
    fn crash() {
        panic!("oops");
    }

    #[test]
    #[should_panic(expected = "panic here")]
    fn must_fail() {
        panic!("panic here");
    }

    #[test]
    #[should_panic]
    fn exploration() {
        assert_eq!(2 + 2, 5);
    }

    #[test]
    fn greeting_contains_name() {
        let result = String::from("Carol");
        assert!(false, "failed here  @'{}'", result);
    }

    #[test]
    fn it_works1() -> Result<(), String> {
        if 2 + 2 == 4 {
            Ok(())
        } else {
            Err(String::from("two plus two does not equal four"))
        }
    }
}
