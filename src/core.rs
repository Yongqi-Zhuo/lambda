use std::rc::Rc;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Intrinsic {
    IsZero,
    Succ,
    Pred,
}

#[derive(Debug, PartialEq)]
pub enum Value {
    VBool(bool),
    VNat(u32),
    VAbs(String, Box<Term>),
    VIntrinsic(Intrinsic),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Term {
    Val(Rc<Value>),
    Var(String),
    Bool(bool),
    Nat(u32),
    Abs(String, Box<Term>),
    Intrinsic(Intrinsic),
    App(Box<Term>, Box<Term>),
    If(Box<Term>, Box<Term>, Box<Term>),
}

impl Term {
    pub fn subst(&self, x: &str, v: Term) -> Term {
        match self {
            Term::Val(v) => Term::Val(v.clone()),
            Term::Var(y) => {
                if x == y {
                    v
                } else {
                    Term::Var(y.clone())
                }
            }
            Term::Bool(b) => Term::Bool(*b),
            Term::Nat(n) => Term::Nat(*n),
            Term::Abs(y, t) => {
                if x == y {
                    // Protected from variable capture
                    Term::Abs(y.clone(), t.clone())
                } else {
                    Term::Abs(y.clone(), Box::new(t.subst(x, v)))
                }
            }
            Term::App(t1, t2) => Term::App(
                Box::new(t1.subst(x, v.clone())),
                Box::new(t2.subst(x, v.clone())),
            ),
            Term::Intrinsic(intrinsic) => Term::Intrinsic(*intrinsic),
            Term::If(t1, t2, t3) => Term::If(
                Box::new(t1.subst(x, v.clone())),
                Box::new(t2.subst(x, v.clone())),
                Box::new(t3.subst(x, v.clone())),
            ),
        }
    }
}
