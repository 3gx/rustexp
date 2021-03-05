//#![feature(min_specialization)]
#![feature(specialization)]
#![allow(incomplete_features)]

#[derive(Clone, Debug)]
pub struct Company(Vec<Department>);
#[derive(Clone, Debug)]
pub struct Department(Name, Manager, Vec<SubUnit>);
#[derive(Clone, Debug)]
pub enum SubUnit {
    Person(Employee),
    Department(Box<Department>),
}
#[derive(Clone, Debug)]
pub struct Employee(Person, Salary);
#[derive(Clone, Debug)]
pub struct Person(Name, Address);
#[derive(Clone, Debug)]
pub struct Salary(f64);

pub type Manager = Employee;
pub type Name = &'static str;
pub type Address = &'static str;

pub mod v1 {
    use super::*;
    pub trait Increase: Sized {
        fn increase(self, k: f64) -> Self;
    }

    impl Increase for Company {
        fn increase(self, k: f64) -> Self {
            Company(self.0.into_iter().map(|d| d.increase(k)).collect())
        }
    }

    impl Increase for Department {
        fn increase(self, k: f64) -> Self {
            Department(
                self.0,
                self.1.increase(k),
                self.2.into_iter().map(|s| s.increase(k)).collect(),
            )
        }
    }

    impl Increase for SubUnit {
        fn increase(self, k: f64) -> Self {
            match self {
                SubUnit::Person(e) => SubUnit::Person(e.increase(k)),
                SubUnit::Department(d) => SubUnit::Department(Box::new(d.increase(k))),
            }
        }
    }

    impl Increase for Employee {
        fn increase(self, k: f64) -> Employee {
            Employee(self.0, self.1.increase(k))
        }
    }

    impl Increase for Salary {
        fn increase(self, k: f64) -> Salary {
            Salary(self.0 * (1.0 + k))
        }
    }
}

mod v2 {
    use super::*;
    pub trait GenericTransform {
        fn transform<T: Term>(&mut self, t: T) -> T;
    }

    // Implementing Cast
    // -----------------

    pub trait Cast<T>: Sized {
        fn cast(self) -> Result<T, Self>;
    }

    impl<T, U> Cast<T> for U {
        default fn cast(self) -> Result<T, Self> {
            Err(self)
        }
    }
    /*
    // requires 'specialization' feature, doesn't work with `min_specialization` feature
    impl Cast<bool> for bool {
        fn cast(self) -> Result<bool, Self> {
            Ok(self)
        }
    }
    */
    impl<T> Cast<T> for T {
        fn cast(self) -> Result<T, Self> {
            Ok(self)
        }
    }

    // Implementing Transformation
    // ---------------------------

    use std::marker::PhantomData;
    pub struct Transformation<F: FnMut(U) -> U, U>(F, PhantomData<fn(U) -> U>);
    impl<F: FnMut(U) -> U, U> Transformation<F, U> {
        // Construct a new `Transformation` from the given function.
        pub fn new(f: F) -> Transformation<F, U> {
            Transformation(f, PhantomData)
        }
    }

    impl<F: FnMut(U) -> U, U> GenericTransform for Transformation<F, U> {
        fn transform<T>(&mut self, t: T) -> T {
            // try to cast from the T into a U
            match Cast::<U>::cast(t) {
                // call transformation function and then cast resulting U back into a T
                Ok(u) => match Cast::<T>::cast((self.0)(u)) {
                    Ok(t) => t,
                    Err(_) => unreachable!("If T=U, then U=T"),
                    // Err(_) => unreachable!("If T=U, then U=T, t={:?}, u={:?}", t, u),
                },
                Err(t) => t,
            }
        }
    }

    // Implementing Term
    // -----------------

    pub trait Term: Sized {
        fn map_one_transform<F: GenericTransform>(self, f: &mut F) -> Self;
    }

    impl Term for Employee {
        fn map_one_transform<F: GenericTransform>(self, f: &mut F) -> Self {
            Employee(f.transform(self.0), f.transform(self.1))
        }
    }

    impl Term for Person {
        fn map_one_transform<F: GenericTransform>(self, f: &mut F) -> Self {
            Person(f.transform(self.0), f.transform(self.1))
        }
    }

    impl Term for Company {
        fn map_one_transform<F: GenericTransform>(self, f: &mut F) -> Self {
            Company(self.0.into_iter().map(|x| f.transform(x)).collect())
        }
    }

    impl Term for Department {
        fn map_one_transform<F: GenericTransform>(self, f: &mut F) -> Self {
            Department(
                f.transform(self.0),
                f.transform(self.1),
                self.2.into_iter().map(|x| f.transform(x)).collect(),
            )
        }
    }

    impl Term for Salary {
        fn map_one_transform<F: GenericTransform>(self, f: &mut F) -> Self {
            Salary(f.transform(self.0))
        }
    }

    impl Term for SubUnit {
        fn map_one_transform<F: GenericTransform>(self, f: &mut F) -> Self {
            match self {
                SubUnit::Person(e) => SubUnit::Person(f.transform(e)),
                SubUnit::Department(d) => SubUnit::Department(f.transform(d)),
            }
        }
    }

    impl Term for f64 {
        fn map_one_transform<F: GenericTransform>(self, _: &mut F) -> Self {
            self
        }
    }

    impl Term for bool {
        fn map_one_transform<F: GenericTransform>(self, _: &mut F) -> Self {
            self
        }
    }
    impl Term for &'static str {
        fn map_one_transform<F: GenericTransform>(self, _: &mut F) -> Self {
            self
        }
    }
    impl<T: Term> Term for Box<T> {
        fn map_one_transform<F: GenericTransform>(self, f: &mut F) -> Box<T> {
            Box::new(f.transform(*self))
        }
    }

    // Implementing Everywhere
    // -----------------------

    pub struct Everywhere<F: GenericTransform> {
        f: F,
    }

    impl<F: GenericTransform> Everywhere<F> {
        pub fn new(f: F) -> Everywhere<F> {
            Everywhere { f }
        }
    }

    impl<F: GenericTransform> GenericTransform for Everywhere<F> {
        fn transform<T: Term>(&mut self, t: T) -> T {
            let t = t.map_one_transform(self);
            self.f.transform(t)
        }
    }
}

fn main() {
    let ralf = Employee(Person("Ralf", "Amsterdam"), Salary(8000.0));
    let joost = Employee(Person("Joost", "Amsterdam"), Salary(1000.0));
    let marlow = Employee(Person("Marlow", "Cambridge"), Salary(2000.0));
    let blair = Employee(Person("Blair", "Cambridge"), Salary(100000.0));
    let com = Company(vec![
        Department(
            "Research",
            ralf.clone(),
            vec![
                SubUnit::Person(joost.clone()),
                SubUnit::Person(marlow.clone()),
            ],
        ),
        Department("Strategy", blair.clone(), vec![]),
    ]);
    println!("com={:?}", com);
    let com_v1 = {
        use crate::v1::Increase;
        com.clone().increase(0.2)
    };
    println!("com={:?}", com_v1);

    {
        use crate::v2::*;
        assert_eq!(Cast::<bool>::cast(1), Err(1));
        assert_eq!(Cast::<bool>::cast(true), Ok(true));

        let mut not = Transformation::new(|b: bool| !b);
        assert_eq!(not.transform(true), false);
        assert_eq!(not.transform("str"), "str");
    }

    let com_v2 = {
        use crate::v2::*;
        // Definition
        let rename = |p: Person| Person("Juan Offus", p.1);
        let mut rename = Everywhere::new(Transformation::new(rename));
        // Usage
        rename.transform(com.clone())
    };
    println!("com={:?}", com_v2);

    let com_v3 = {
        use crate::v2::*;
        // Definition
        let rename = |s: Salary| Salary(s.0 * (1.0 + 0.2));
        let mut rename = Everywhere::new(Transformation::new(rename));
        // Usage
        rename.transform(com)
    };
    println!("com={:?}", com_v3);
}
