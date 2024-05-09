use std::rc::Rc;

use super::core::{Intrinsic, Term, Value};
use super::intrinsic::{INTRINSICS, IntrinsicOp1};

pub struct Evaluator {}

impl Evaluator {
    pub fn new() -> Self {
        Self {}
    }

    fn lookup_intrinsic(&self, intrinsic: Intrinsic) -> &IntrinsicOp1 {
        &INTRINSICS[intrinsic as usize]
    }

    pub fn eval(&self, t: Term) -> Rc<Value> {
        match t {
            Term::Val(v) => v,
            Term::Var(x) => panic!("Unbound variable: {}", x),
            Term::Bool(b) => Rc::new(Value::VBool(b)),
            Term::Nat(n) => Rc::new(Value::VNat(n)),
            Term::Abs(x, t) => Rc::new(Value::VAbs(x, t)),
            Term::App(t1, t2) => {
                let v1 = self.eval(*t1);
                match &*v1 {
                    Value::VBool(_) => panic!("Cannot apply a boolean"),
                    Value::VNat(_) => panic!("Cannot apply a number"),
                    Value::VAbs(x, t) => {
                        // Call-by-value
                        let v2 = self.eval(*t2);
                        let app = t.subst(x, Term::Val(v2));
                        self.eval(app)
                    }
                    Value::VIntrinsic(intrinsic) => {
                        let op = self.lookup_intrinsic(*intrinsic);
                        let v2 = self.eval(*t2);
                        Rc::new((op.func)(&*v2))
                    },
                }
            }
            Term::Intrinsic(intrinsic) => Rc::new(Value::VIntrinsic(intrinsic)),
            Term::If(t1, t2, t3) => {
                let v1 = self.eval(*t1);
                if let Value::VBool(b) = &*v1 {
                    if *b {
                        self.eval(*t2)
                    } else {
                        self.eval(*t3)
                    }
                } else {
                    panic!("if expects a boolean");
                }
            }
        }
    }
}
