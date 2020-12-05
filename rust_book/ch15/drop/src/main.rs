struct CSP {
    data: String,
}

impl Drop for CSP {
    fn drop(&mut self) {
        println!("Dropping CSP with data `{}`!", self.data);
    }
}
fn main() {
    let _c = CSP {
        data: String::from("my stuff"),
    };
    {
        let _d = CSP {
            data: String::from("other stuff"),
        };
    }
    let _e = CSP {
        data: String::from("my stuff e"),
    };
    drop(_c);
    println!("CustomSmartPointers created.");
}
