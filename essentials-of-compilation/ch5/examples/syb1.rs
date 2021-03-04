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
    println!("com={:#?}", com);
    let com_v1 = {
        use crate::v1::Increase;
        com.clone().increase(0.2)
    };
    println!("com={:#?}", com_v1);
}
