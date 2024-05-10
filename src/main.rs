// Here we implement a basic lambda calculus interpreter in Rust.

use lambda::{Evaluator, Parser, Term};

fn main() {
    println!("Hello, world!");
    // fix = λf. (λx. f (λv. x x v)) (λx. f (λv. x x v))
    let fix = Parser::new("λf. (λx. f (λv. x x v)) (λx. f (λv. x x v))").parse();
    // plus = λm. λn. if (iszero m) n (plus (pred m) (succ n))
    // plus = fix (λf. λm. λn. if iszero m then n else (f (pred m) (succ n)))
    let plus = Term::App(
        Box::new(fix),
        Box::new(Parser::new("λf. λm. λn. if iszero m then n else (f (pred m) (succ n))").parse()),
    );
    let t = Term::App(
        Box::new(Term::App(Box::new(plus), Box::new(Term::Nat(3)))),
        Box::new(Term::Nat(2)),
    );
    let evaluator = Evaluator::new();
    let v = evaluator.eval(t);
    println!("{:?}", *v);
}
