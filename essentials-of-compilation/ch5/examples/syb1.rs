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
    pub trait GenericTransform {
        fn transform<U>(&mut self, t: U) -> U;
    }
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

    use std::marker::PhantomData;
    pub struct Transformation<F, U>
    where
        F: FnMut(U) -> U,
    {
        f: F,
        phantom: PhantomData<fn(U) -> U>,
    }
    impl<F, U> Transformation<F, U>
    where
        F: FnMut(U) -> U,
    {
        /// Construct a new `Transformation` from the given function.
        #[inline]
        pub fn new(f: F) -> Transformation<F, U> {
            Transformation {
                f,
                phantom: ::std::marker::PhantomData,
            }
        }
    }

    impl<F, U: std::fmt::Debug> GenericTransform for Transformation<F, U>
    where
        F: FnMut(U) -> U,
    {
        fn transform<T>(&mut self, t: T) -> T {
            // try to cast from the T into a U
            match Cast::<U>::cast(t) {
                // call transformation function and then cast resulting U back into a T
                Ok(u) => match Cast::<T>::cast((self.f)(u)) {
                    Ok(t) => t,
                    Err(_) => unreachable!("If T=U, then U=T"), //                    Err(_) => unreachable!("If T=U, then U=T, t={:?}, u={:?}", t, u),
                },
                Err(t) => t,
            }
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

    {
        use crate::v2::*;
        assert_eq!(Cast::<bool>::cast(1), Err(1));
        assert_eq!(Cast::<bool>::cast(true), Ok(true));

        let mut not = Transformation::new(|b: bool| !b);
        assert_eq!(not.transform(true), false);
        assert_eq!(not.transform("str"), "str");
    }
    println!("com={:?}", com_v1);
}
