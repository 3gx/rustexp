pub struct Company(Vec<Department>);
pub struct Department(Name, Manager, Vec<SubUnit>);
pub enum SubUnit {
    Person(Employee),
    Department(Box<Department>),
}
pub struct Employee(Person, Salary);
pub struct Person(Name, Address);
pub struct Salary(f64);

pub type Manager = Employee;
pub type Name = &'static str;
pub type Address = &'static str;

fn main() {
    println!("syb main");
}
