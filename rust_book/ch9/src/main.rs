use std::error::Error;
use std::fs::File;

fn main() -> Result<(), Box<dyn Error>> {
    let _f = File::open("hello.txt")?;

    Ok(())
}
