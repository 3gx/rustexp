use std::sync::mpsc;
use std::thread;

fn main() {
    {
        let (tx, rx) = mpsc::channel::<String>();

        thread::spawn(move || {
            let val = String::from("hi");
            tx.send(val).unwrap();
            // println!("val is {}", val); // won't build,  `rustc --explain E0382`
        });

        let received = rx.recv().unwrap();
        println!("Got: {}", received);
    }
    {
        use std::time::Duration;
        let (tx, rx) = mpsc::channel();

        thread::spawn(move || {
            let vals = vec![
                String::from("hi"),
                String::from("from"),
                String::from("the"),
                String::from("thread"),
            ];
            for val in vals {
                tx.send(val).unwrap();
                thread::sleep(Duration::from_secs(1));
            }
        });

        for received in rx {
            println!("Got: {}", received);
        }
    }

    {
        use std::time::Duration;
        let (tx, rx) = mpsc::channel();
        let tx1 = mpsc::Sender::clone(&tx);
        thread::spawn(move || {
            let vals = vec![
                String::from("hi"),
                String::from("from"),
                String::from("the"),
                String::from("thread"),
            ];

            for val in vals {
                tx1.send(("1",val)).unwrap();
                thread::sleep(Duration::from_secs(1));
            }
        });

        thread::spawn(move || {
            let vals = vec![
                String::from("more"),
                String::from("messages"),
                String::from("for"),
                String::from("you"),
            ];

            for val in vals {
                tx.send(("2",val)).unwrap();
                thread::sleep(Duration::from_secs(1));
            }
        });

        for (tid, received) in rx {
            println!("Got: {}-{}", tid, received);
        }
    }
}
