#[cfg(test)]
mod tests {
    use add_one;

    #[test]
    fn adder_addone() {
        assert_eq!(3, add_one::add_one(2));
    }
}
