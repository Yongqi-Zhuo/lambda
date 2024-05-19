use std::rc::Rc;

use crate::core::Term;
use crate::intrinsic;
use crate::value::{Captures, Value, ValueAbs};

pub fn eval<'a>(captures: &Captures<'a>, t: &'a Term) -> Rc<Value<'a>> {
    match t {
        Term::Var(x) => captures.lookup(x),
        Term::Bool(b) => Rc::new(Value::VBool(*b)),
        Term::Nat(n) => Rc::new(Value::VNat(*n)),
        Term::Abs(abs) => Rc::new(ValueAbs::new(abs, captures.clone()).into()),
        Term::App(t1, t2) => {
            let v1 = eval(captures, t1);
            match &*v1 {
                Value::VBool(_) => panic!("Cannot apply a boolean"),
                Value::VNat(_) => panic!("Cannot apply a number"),
                Value::VAbs(abs) => {
                    // Call-by-value
                    let v2 = eval(captures, t2);
                    let captures = abs.captures.extended(&abs.term.x, v2);
                    eval(&captures, &abs.term.t)
                }
                Value::VIntrinsic(intrinsic) => {
                    let op = intrinsic::lookup(*intrinsic);
                    let v2 = eval(captures, t2);
                    Rc::new(op.call(captures, &*v2))
                },
            }
        }
        Term::Intrinsic(intrinsic) => Rc::new(Value::VIntrinsic(*intrinsic)),
        Term::If(t1, t2, t3) => {
            let v1 = eval(captures, t1);
            if let Value::VBool(b) = &*v1 {
                if *b {
                    eval(captures, t2)
                } else {
                    eval(captures, t3)
                }
            } else {
                panic!("if expects a boolean");
            }
        }
    }
}
