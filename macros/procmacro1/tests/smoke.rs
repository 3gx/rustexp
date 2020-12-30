#[procmacro1::hello]
fn wrapped_function() {}

#[test]
fn works() {
    let res = wrapped_function();
    println!("res= {}" , res);
    assert_eq!(res, 42);
}
