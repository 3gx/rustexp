use std::thread;
use std::time::Duration;

fn simulated_expensive_calc(intensity: u32) -> u32 {
    println!("calculating slowly..");
    //    thread::sleep(Duration::from_secs(2));
    intensity
}

struct Cacher<T>
where
    T: Fn(u32) -> u32,
{
    calculation: T,
    value: Option<u32>,
}

impl<T> Cacher<T>
where
    T: Fn(u32) -> u32,
{
    fn new(calculation: T) -> Cacher<T> {
        Cacher {
            calculation,
            value: None,
        }
    }
    fn value(&mut self, arg: u32) -> u32 {
        match self.value {
            Some(v) => v,
            None => {
                let v = (self.calculation)(arg);
                self.value = Some(v);
                v
            }
        }
    }
}

#[test]
fn call_with_different_values() {
    let mut c = Cacher::new(|a| a);

    let v1 = c.value(1);
    let v2 = c.value(2);

    assert_eq!(v1, 1);
    assert_ne!(v2, 2);
}

fn main() {
    println!("Hello, world!");
    let expensive_closure = |num| {
        println!("calculating slowly..");
        thread::sleep(Duration::from_secs(1));
        num
    };
    let mut calc = Cacher::new(expensive_closure);
    println!("run1");
    calc.value(42);
    println!("run2");
    calc.value(42);
    println!("run3");
    simulated_expensive_calc(42);
    println!("done");

    let x = 4;
    let equal_to_x = |z| z == x;
    let y = 4;
    assert!(equal_to_x(y));

    let x = vec![1, 2, 3];
    let equal_to_x = move |z| z == x;
    //    println!("can't use x here: {:?}", x);
    let y = vec![1, 2, 3];
    assert!(equal_to_x(y));
}
