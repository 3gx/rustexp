#[derive(Debug)]
struct SelectBox {
    width: u32,
    height: u32,
    options: Vec<String>,
}

use traits::{Button, Draw, Screen};

impl Draw for SelectBox {
    fn draw(&self) {
        println!("draw selectBox");
    }
}

fn main() {
    {
        let screen = Screen {
            components: vec![
                Box::new(SelectBox {
                    width: 75,
                    height: 10,
                    options: vec![
                        String::from("Yes"),
                        String::from("Maybe"),
                        String::from("No"),
                    ],
                }),
                Box::new(Button {
                    width: 50,
                    height: 10,
                    label: String::from("OK"),
                }),
            ],
        };

        screen.run();
    }

    /*
     {
         let screen = Screen {
             components: vec![Box::new(String::from("Hi"))],
    //                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ the trait `Draw` is not implemented
         };

         screen.run();
     } // `rustc --explain E0277`.
     */
}
