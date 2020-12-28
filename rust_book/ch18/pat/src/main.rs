fn main() {
    // -- matching literals
    {
        let x = 1;
        match x {
            1 => println!("one"),
            2 => println!("two"),
            3 => println!("three"),
            _ => println!("anything"),
        }
    }

    // -- matching named variables
    {
        let x = Some(5);
        let y = 10;

        match x {
            Some(50) => println!("Got 50"),
            Some(y) => println!("Matched, y = {:?}", y),
            _ => println!("Default case, x = {:?}", x),
        }

        println!("at the end: x = {:?}, y = {:?}", x, y);
    }

    // -- matching patterns
    {
        let x = 1;

        match x {
            1 | 2 => println!("one or two"),
            3 => println!("three"),
            _ => println!("anything"),
        }

        let x = 2;

        match x {
            1 | 2 => println!("one or two"),
            3 => println!("three"),
            _ => println!("anything"),
        }
    }

    // -- matching ranges of values with ..=
    {
        let x = 5;

        match x {
            1..=5 => println!("one through five"),
            _ => println!("something else"),
        }

        let x = 'c';

        match x {
            'a'..='j' => println!("early ASCII letter"),
            'k'..='z' => println!("late ASCII letter"),
            _ => println!("something else"),
        }
    }

    // Destructuring structs
    {
        struct Point {
            x: i32,
            y: i32,
        }

        let p = Point { x: 0, y: 7 };

        let Point { x: a, y: b } = p;
        assert_eq!(0, a);
        assert_eq!(7, b);

        let Point { x, y } = p;
        assert_eq!(0, x);
        assert_eq!(7, y);

        match p {
            Point { x, y: 0 } => println!("On the x axis at {}", x),
            Point { x: 0, y } => println!("On the y axis at {}", y),
            Point { x, y } => println!("On neither axis: ({}, {})", x, y),
        }
    }

    // -- Destructing enums
    {
        enum Message {
            _Quit,
            _Move { x: i32, y: i32 },
            _Write(String),
            ChangeColor(i32, i32, i32),
        }
        let msg = Message::ChangeColor(0, 160, 255);

        match msg {
            Message::_Quit => {
                println!("The Quit variant has no data to destructure.")
            }
            Message::_Move { x, y } => {
                println!("Move in the x direction {} and in the y direction {}", x, y);
            }
            Message::_Write(text) => println!("Text message: {}", text),
            Message::ChangeColor(r, g, b) => {
                println!("Change the color to red {}, green {}, and blue {}", r, g, b)
            }
        }
    }

    // -- Destructuring nested structs and enums
    {
        enum Color {
            _Rgb(i32, i32, i32),
            Hsv(i32, i32, i32),
        }

        enum Message {
            _Quit,
            _Move { x: i32, y: i32 },
            _Write(String),
            ChangeColor(Color),
        }
        let msg = Message::ChangeColor(Color::Hsv(0, 160, 255));

        match msg {
            Message::ChangeColor(Color::_Rgb(r, g, b)) => {
                println!("Change the color to red {}, green {}, and blue {}", r, g, b)
            }
            Message::ChangeColor(Color::Hsv(h, s, v)) => println!(
                "Change the color to hue {}, saturation {}, and value {}",
                h, s, v
            ),
            _ => (),
        }
    }

    // -- Destructuring structs & tuples
    {
        struct Point {
            x: i32,
            y: i32,
        }
        let ((feet, inches), Point { x, y }) = ((3, 10), Point { x: 3, y: -10 });
        println!("feet= {}, inches= {}", feet, inches);
        println!("x= {}, y= {}", x, y);
    }

    // -- Ignoring values with _
    {
        fn foo(_: i32, y: i32) {
            println!("This code only uses the y parameter: {}", y);
        }
        foo(3, 4);
    }

    // -- Ignoring parts of a value with a nested _
    {
        let mut setting_value = Some(5);
        let new_setting_value = Some(10);

        match (setting_value, new_setting_value) {
            (Some(_), Some(_)) => {
                println!("Can't overwrite an existing customized value");
            }
            _ => {
                setting_value = new_setting_value;
            }
        }

        println!("setting is {:?}", setting_value);

        let numbers = (2, 4, 8, 16, 32);

        match numbers {
            (first, _, third, _, fifth) => {
                println!("Some numbers: {}, {}, {}", first, third, fifth)
            }
        }
    }

    // - ignoring an usunsed variable by staring its name with _
    {
        let s = Some(String::from("Hello!"));

        if let Some(_) = s {
            // can't use _s instead of _, `rustc --explain E0382`.
            println!("found a string");
        }

        println!("{:?}", s);
    }

    // -- ignoring remaining parts of a value with ...
    {
        struct Point {
            x: i32,
            _y: i32,
            _z: i32,
        }

        let origin = Point { x: 0, _y: 0, _z: 0 };

        match origin {
            Point { x, .. } => println!("x is {}", x),
        }

        let numbers = (2, 4, 8, 16, 32);

        match numbers {
            (first, .., last) => {
                println!("Some numbers: {}, {}", first, last);
            }
        }
    }

    // -- extra condition with match guard
    {
        let num = Some(4);

        match num {
            Some(x) if x < 5 => println!("less than five: {}", x),
            Some(x) => println!("{}", x),
            None => (),
        }

        let num = Some(4);
        match num {
            _  if num == Some(3) => println!("is three: {:?}", num),
            _  if num == Some(4) => println!("is four: {:?}", num),
            Some(x) => println!("{}", x),
            None => (),
        }

        let x = Some(5);
        let y = 10;

        match x {
            Some(50) => println!("Got 50"),
            Some(n) if n == y => println!("Matched, n = {}", n),
            _ => println!("Default case, x = {:?}", x),
        }

        let x = Some(10);
        match x {
            Some(50) => println!("Got 50"),
            Some(n) if n == y => println!("Matched, n = {}", n),
            _ => println!("Default case, x = {:?}", x),
        }

        println!("at the end: x = {:?}, y = {}", x, y);

        let x = 4;
        let y = false;

        match x {
            4 | 5 | 6 if y => println!("yes"),
            _ => println!("no"),
        }
    }

    // -- @bindings
    {
        enum Message {
            Hello { id: i32 },
        }

        let msg = Message::Hello { id: 5 };

        match msg {
            Message::Hello { id: idval @ 3..=7 } => println!("Found an id in range: {}", idval),
            Message::Hello { id: 10..=12 } => {
                println!("Found an id in another range")
            }
            Message::Hello { id } => println!("Found some other id: {}", id),
        }
    }
}
