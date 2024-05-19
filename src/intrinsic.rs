use crate::core::{Intrinsic, Type};
use crate::value::Value;

pub struct IntrinsicOp1 {
    pub op: Intrinsic,
    pub name: &'static str,
    // Function pointers cannot be generic over lifetimes
    func: fn(&Value<'static>) -> Value<'static>,
    pub typing: IntrinsicOp1Typing,
}

pub struct IntrinsicOp1Typing {
    pub arg: Type,
    pub ret: Type,
}

impl IntrinsicOp1 {
    pub fn call<'a>(&self, v: &Value<'a>) -> Value<'a> {
        // We need to bypass the lifetime checker
        (self.func)(unsafe { std::mem::transmute(v) })
    }
}

pub const INTRINSICS: [IntrinsicOp1; 3] = [
    IntrinsicOp1 {
        op: Intrinsic::IsZero,
        name: "iszero",
        func: |v| match v {
            Value::VNat(n) => Value::VBool(*n == 0),
            _ => panic!("iszero expects a number"),
        },
        typing: IntrinsicOp1Typing {
            arg: Type::TNat,
            ret: Type::TBool,
        },
    },
    IntrinsicOp1 {
        op: Intrinsic::Succ,
        name: "succ",
        func: |v| match v {
            Value::VNat(n) => Value::VNat(n + 1),
            _ => panic!("succ expects a number"),
        },
        typing: IntrinsicOp1Typing {
            arg: Type::TNat,
            ret: Type::TNat,
        },
    },
    IntrinsicOp1 {
        op: Intrinsic::Pred,
        name: "pred",
        func: |v| match v {
            Value::VNat(n) => {
                if *n == 0 {
                    panic!("pred expects a non-zero number");
                }
                Value::VNat(n - 1)
            }
            _ => panic!("pred expects a number"),
        },
        typing: IntrinsicOp1Typing {
            arg: Type::TNat,
            ret: Type::TNat,
        },
    },
];

pub fn lookup(intrinsic: Intrinsic) -> &'static IntrinsicOp1 {
    &INTRINSICS[intrinsic as usize]
}
