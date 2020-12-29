
use std::cell::RefCell;
use std::rc::Rc;

fn vec_with_closure<'a, T: 'a>(f: Box<dyn Fn(T) + 'a>) -> Rc<RefCell<Vec<Box<dyn Fn(T) + 'a>>>>
{
    let v = Rc::new(RefCell::new(Vec::<Box<dyn Fn(T)>>::new()));
    v.borrow_mut().push(Box::new(move |t: T| {
        f(t);
    }));
    v
}

fn fun<'a>() -> Rc<RefCell<Vec<Box<dyn Fn(usize) + 'a>>>> {
    let fun = "sbv".to_string();
    //let &rfun = fun;
    let v = vec_with_closure(Box::new(move |t: usize| {
        println!("{:?}, {}", fun, t);
    }));
    for c in v.borrow_mut().iter_mut() {
        c(10);
    }
  //  println!("fun, rfun = {:?}, {:?}", rfun, rfun);
    v
}

fn main() {
  let v = fun();
  v.borrow()[0](10);
}
