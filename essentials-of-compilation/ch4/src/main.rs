//#![cfg_attr(feature = "nightly", feature(specialization))]

fn main() {
    {
        use gc::{Finalize, Gc, Trace};
        let x = Gc::new(1_u8);
        let y = Gc::new(Box::new(Gc::new(1_u8)));

        #[derive(Trace, Finalize, Debug, Clone)]
        struct Foo {
            a: Gc<u8>,
            b: u8,
        }

        let z = Gc::new(Foo { a: x.clone(), b: 1 });
        println!("x= {:?}", x);
        println!("y= {:?}", y);
        println!("z= {:?}", z);
    }

    {
        use gc::{Finalize, Gc, GcCell, Trace};
        #[derive(Trace, Finalize, Debug)]
        struct Foo {
            cyclic: GcCell<Option<Gc<Foo>>>,
            data: u8,
        }

        let foo1 = Gc::new(Foo {
            cyclic: GcCell::new(None),
            data: 1,
        });
        let foo2 = Gc::new(Foo {
            cyclic: GcCell::new(Some(foo1.clone())),
            data: 2,
        });
        let foo3 = Gc::new(Foo {
            cyclic: GcCell::new(Some(foo2.clone())),
            data: 3,
        });
        println!("foo3= {:?}", foo3);
        println!("foo1= {:?}", foo1);
        *foo1.cyclic.borrow_mut() = Some(foo3.clone());
    }
    {
        use gc::{Gc, GcCell};
        let x = Gc::new(GcCell::new(20));
        let y = Gc::clone(&x);
        println!("x= {:?}", x);
        *x.borrow_mut() = 42;
        println!("x= {:?}", y);
    }
    println!("Hello, world!");
}
