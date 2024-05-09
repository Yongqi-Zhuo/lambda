use super::core::{Intrinsic, Value};

pub struct IntrinsicOp1 {
    pub op: Intrinsic,
    pub name: &'static str,
    pub func: fn(&Value) -> Value,
}

pub const INTRINSICS: [IntrinsicOp1; 3] = [
    IntrinsicOp1 {
        op: Intrinsic::IsZero,
        name: "iszero",
        func: |v| match v {
            Value::VNat(n) => Value::VBool(*n == 0),
            _ => panic!("iszero expects a number"),
        },
    },
    IntrinsicOp1 {
        op: Intrinsic::Succ,
        name: "succ",
        func: |v| match v {
            Value::VNat(n) => Value::VNat(n + 1),
            _ => panic!("succ expects a number"),
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
    },
];
