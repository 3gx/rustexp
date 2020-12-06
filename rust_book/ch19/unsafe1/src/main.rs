/*
 * Unsafe five Superpowers
 *
 *  - Dereference a raw pointer
 *  - Call an unsafe function or method
 *  - Access or modify a mutable static variable
 *  - Implement an unsafe trait
 *  - Access fields of unions
 */

unsafe fn dangerous() {}

use std::slice;

fn split_at_mut(slice: &mut [i32], mid: usize) -> (&mut [i32], &mut [i32]) {
    let len = slice.len();
    let ptr = slice.as_mut_ptr();

    assert!(mid <= len);

    unsafe {
        (
            slice::from_raw_parts_mut(ptr, mid),
            slice::from_raw_parts_mut(ptr.add(mid), len - mid),
        )
    }
}

extern "C" {
    fn abs(input: i32) -> i32;
}

#[no_mangle]
pub extern "C" fn call_from_c(num: i32) -> i32 {
    println!("Just calling a Rust from C!");
    if num < 0 {
        -num
    } else {
        num
    }
}

static mut COUNTER: u32 = 0;

fn add_to_count(inc: u32) {
    unsafe {
        COUNTER += inc;
    }
}

unsafe trait Foo {}

unsafe impl Foo for i32 {}

fn main() {
    let mut num = 5;
    let r1 = &num as *const i32;
    let r2 = &mut num as *mut i32;
    // dangerous(); // call to unsafe function,  `rustc --explain E0133`.
    unsafe {
        println!("r1 is: {}", *r1);
        println!("r2 is: {}", *r2);
        dangerous();
        *r2 = 42;
    }
    println!("num is: {}", num);

    let mut v = vec![1, 2, 3, 4, 5, 6];
    let r = &mut v[..];

    let (a, b) = r.split_at_mut(3);
    assert_eq!(a, &mut [1, 2, 3]);
    assert_eq!(b, &mut [4, 5, 6]);

    let (a, b) = split_at_mut(r, 3);
    assert_eq!(a, &mut [1, 2, 3]);
    assert_eq!(b, &mut [4, 5, 6]);

    //  v.push(60);
    println!("v= {:?}", v); // borrowed here

    // println!("b= {:?}", a); // can't use here,  `rustc --explain E0502`.

    let address = 0x01234usize;
    let r = address as *mut i32;

    let _slice: &[i32] = unsafe { slice::from_raw_parts_mut(r, 10000) };
    // println!("slice= {:?}", _slice); // will segfault ..

    let num = -3;
    unsafe {
        println!("Abs of '{}' is '{}'", num, abs(num));
    }

    println!("fromc: Abs of '{}' is '{}'", num, call_from_c(num));

    add_to_count(3);

    unsafe {
        println!("COUNTER: {}", COUNTER);
    }
    add_to_count(2);

    unsafe {
        println!("COUNTER: {}", COUNTER);
    }
}
