// Here we implement a basic lambda calculus interpreter in Rust.

use std::{collections::HashMap, rc::Rc};

#[derive(Debug)]
enum Value {
    VBool(bool),
    VNat(u32),
    VAbs(String, Box<Term>),
    VIntrinsic(Intrinsic),
}

#[derive(Clone, Debug)]
enum Term {
    Val(Rc<Value>),
    Var(String),
    Nat(u32),
    Abs(String, Box<Term>),
    App(Box<Term>, Box<Term>),
    If(Box<Term>, Box<Term>, Box<Term>),
}

#[derive(Debug)]
enum Intrinsic {
    IsZero,
    Succ,
    Pred,
}

#[derive(Debug)]
struct Context {
    bindings: HashMap<String, Rc<Value>>,
}

impl Term {
    fn subst(&self, x: &str, v: Term) -> Term {
        match self {
            Term::Val(v) => Term::Val(v.clone()),
            Term::Var(y) => {
                if x == y {
                    v
                } else {
                    Term::Var(y.clone())
                }
            }
            Term::Nat(n) => Term::Nat(*n),
            Term::Abs(y, t) => {
                if x == y {
                    Term::Abs(y.clone(), t.clone())
                } else {
                    Term::Abs(y.clone(), Box::new(t.subst(x, v)))
                }
            }
            Term::App(t1, t2) => Term::App(
                Box::new(t1.subst(x, v.clone())),
                Box::new(t2.subst(x, v.clone())),
            ),
            Term::If(t1, t2, t3) => Term::If(
                Box::new(t1.subst(x, v.clone())),
                Box::new(t2.subst(x, v.clone())),
                Box::new(t3.subst(x, v.clone())),
            ),
        }
    }
}

impl Context {
    fn new() -> Context {
        Context {
            bindings: HashMap::from([
                (
                    "iszero".to_string(),
                    Rc::new(Value::VIntrinsic(Intrinsic::IsZero)),
                ),
                (
                    "succ".to_string(),
                    Rc::new(Value::VIntrinsic(Intrinsic::Succ)),
                ),
                (
                    "pred".to_string(),
                    Rc::new(Value::VIntrinsic(Intrinsic::Pred)),
                ),
            ]),
        }
    }

    fn lookup(&self, x: &str) -> Rc<Value> {
        match self.bindings.get(x) {
            Some(v) => v.clone(),
            None => panic!("Variable not in scope"),
        }
    }

    fn eval(&self, t: Term) -> Rc<Value> {
        match t {
            Term::Val(v) => v,
            Term::Var(x) => self.lookup(&x),
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
                    Value::VIntrinsic(intrinsic) => match intrinsic {
                        Intrinsic::IsZero => {
                            let v2 = self.eval(*t2);
                            match &*v2 {
                                Value::VNat(n) => Rc::new(Value::VBool(*n == 0)),
                                _ => panic!("iszero expects a number"),
                            }
                        }
                        Intrinsic::Succ => {
                            let v2 = self.eval(*t2);
                            match &*v2 {
                                Value::VNat(n) => Rc::new(Value::VNat(n + 1)),
                                _ => panic!("succ expects a number"),
                            }
                        }
                        Intrinsic::Pred => {
                            let v2 = self.eval(*t2);
                            match &*v2 {
                                Value::VNat(n) => {
                                    if *n == 0 {
                                        panic!("pred expects a non-zero number");
                                    }
                                    Rc::new(Value::VNat(n - 1))
                                }
                                _ => panic!("pred expects a number"),
                            }
                        }
                    },
                }
            }
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

fn main() {
    println!("Hello, world!");
    // fix = λf. (λx. λv. f (x x) v) (λx. λv. f (x x) v)
    let fix: Term = Term::Abs(
        "f".to_string(),
        Box::new(Term::App(
            Box::new(Term::Abs(
                "x".to_string(),
                Box::new(Term::Abs(
                    "v".to_string(),
                    Box::new(Term::App(
                        Box::new(Term::App(
                            Box::new(Term::Var("f".to_string())),
                            Box::new(Term::App(
                                Box::new(Term::Var("x".to_string())),
                                Box::new(Term::Var("x".to_string())),
                            )),
                        )),
                        Box::new(Term::Var("v".to_string())),
                    )),
                )),
            )),
            Box::new(Term::Abs(
                "x".to_string(),
                Box::new(Term::Abs(
                    "v".to_string(),
                    Box::new(Term::App(
                        Box::new(Term::App(
                            Box::new(Term::Var("f".to_string())),
                            Box::new(Term::App(
                                Box::new(Term::Var("x".to_string())),
                                Box::new(Term::Var("x".to_string())),
                            )),
                        )),
                        Box::new(Term::Var("v".to_string())),
                    )),
                )),
            )),
        )),
    );
    // plus = λm. λn. if (iszero m) n (plus (pred m) (succ n))
    // plus = fix (λf. λm. λn. if iszero m then n else (f (pred m) (succ n)))
    let plus = Term::App(
        Box::new(fix),
        Box::new(Term::Abs(
            "f".to_string(),
            Box::new(Term::Abs(
                "m".to_string(),
                Box::new(Term::Abs(
                    "n".to_string(),
                    Box::new(Term::If(
                        Box::new(Term::App(
                            Box::new(Term::Var("iszero".to_string())),
                            Box::new(Term::Var("m".to_string())),
                        )),
                        Box::new(Term::Var("n".to_string())),
                        Box::new(Term::App(
                            Box::new(Term::App(
                                Box::new(Term::Var("f".to_string())),
                                Box::new(Term::App(
                                    Box::new(Term::Var("pred".to_string())),
                                    Box::new(Term::Var("m".to_string())),
                                )),
                            )),
                            Box::new(Term::App(
                                Box::new(Term::Var("succ".to_string())),
                                Box::new(Term::Var("n".to_string())),
                            )),
                        )),
                    )),
                )),
            )),
        )),
    );
    let t = Term::App(
        Box::new(Term::App(
            Box::new(plus),
            Box::new(Term::Nat(3)),
        )),
        Box::new(Term::Nat(2)),
    );
    let ctx = Context::new();
    let v = ctx.eval(t);
    println!("{:?}", *v);
}
