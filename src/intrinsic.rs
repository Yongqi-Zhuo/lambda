use crate::core::{Intrinsic, NameGenerator, Term, TermAbs, Type};
use crate::value::{Captures, Value};

// Function pointers cannot be generic over lifetimes
type Eval1Func = fn(&Captures<'static>, &Value<'static>) -> Value<'static>;

pub struct IntrinsicOp1 {
    pub op: Intrinsic,
    pub display_name: &'static str,
    pub name: &'static str,
    eval_func: Eval1Func,
    typing_func: fn(&mut NameGenerator) -> Type,
}

impl IntrinsicOp1 {
    pub fn call<'a>(&self, captures: &Captures<'a>, v: &Value<'a>) -> Value<'a> {
        // We need to bypass the lifetime checker
        // SAFETY: The return value will live as long as the input values
        unsafe { (self.eval_func)(std::mem::transmute(captures), std::mem::transmute(v)) }
    }
    pub fn typing(&self, gen: &mut NameGenerator) -> Type {
        (self.typing_func)(gen)
    }
}

pub fn fix_combinator() -> Term {
    Term::Abs(TermAbs::new(
        "f".to_string(),
        None,
        Box::new(Term::App(
            Box::new(Term::Abs(TermAbs::new(
                "x".to_string(), None,
                Box::new(Term::App(
                    Box::new(Term::Var("f".to_string())),
                    Box::new(Term::Abs(TermAbs::new(
                        "v".to_string(), None,
                        Box::new(Term::App(
                            Box::new(Term::App(
                                Box::new(Term::Var("x".to_string())),
                                Box::new(Term::Var("x".to_string())),
                            )),
                            Box::new(Term::Var("v".to_string())),
                        )),
                    ))),
                ))),
            )),
            Box::new(Term::Abs(TermAbs::new(
                "x".to_string(), None,
                Box::new(Term::App(
                    Box::new(Term::Var("f".to_string())),
                    Box::new(Term::Abs(TermAbs::new(
                        "v".to_string(), None,
                        Box::new(Term::App(
                            Box::new(Term::App(
                                Box::new(Term::Var("x".to_string())),
                                Box::new(Term::Var("x".to_string())),
                            )),
                            Box::new(Term::Var("v".to_string())),
                        )),
                    ))),
                ))),
            )),
        )))
    )
}

pub const INTRINSICS: [IntrinsicOp1; 3] = [
    IntrinsicOp1 {
        op: Intrinsic::IsZero,
        display_name: "iszero",
        name: "__iszero",
        eval_func: |_, v| match v {
            Value::VNat(n) => Value::VBool(*n == 0),
            _ => panic!("iszero expects a number"),
        },
        typing_func: |_| Type::TArrow(Box::new(Type::TNat), Box::new(Type::TBool)),
    },
    IntrinsicOp1 {
        op: Intrinsic::Succ,
        display_name: "succ",
        name: "__succ",
        eval_func: |_, v| match v {
            Value::VNat(n) => Value::VNat(n + 1),
            _ => panic!("succ expects a number"),
        },
        typing_func: |_| Type::TArrow(Box::new(Type::TNat), Box::new(Type::TNat)),
    },
    IntrinsicOp1 {
        op: Intrinsic::Pred,
        display_name: "pred",
        name: "__pred",
        eval_func: |_, v| match v {
            Value::VNat(n) => {
                if *n == 0 {
                    panic!("pred expects a non-zero number");
                }
                Value::VNat(n - 1)
            }
            _ => panic!("pred expects a number"),
        },
        typing_func: |_| Type::TArrow(Box::new(Type::TNat), Box::new(Type::TNat)),
    },
];

pub struct Prologue {
    pub name: &'static str,
    pub ty: Type,
    pub term: Term,
}

pub fn prologue(gen: &mut NameGenerator) -> Vec<Prologue> {
    let mut res: Vec<Prologue> = INTRINSICS.iter().map(|op| Prologue {
        name: op.display_name,
        ty: op.typing(gen),
        term: Term::Intrinsic(op.op),
    }).collect();
    {
        // Add fix combinator
        let t = Type::fresh(gen);
        let fix = fix_combinator();
        res.push(Prologue {
            name: "fix",
            ty: Type::TArrow(
                Box::new(Type::TArrow(
                    Box::new(t.clone()),
                    Box::new(t.clone()),
                )),
                Box::new(t),
            ),
            term: fix,
        })
    }
    res
}

pub fn lookup(intrinsic: Intrinsic) -> &'static IntrinsicOp1 {
    &INTRINSICS[intrinsic as usize]
}
