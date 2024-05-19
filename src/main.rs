use std::io;

use lambda::{NameGenerator, REPL};

fn main() -> Result<(), io::Error> {
    let mut gen = NameGenerator::new();
    let mut repl = REPL::new(&mut gen);
    repl.run()
}
