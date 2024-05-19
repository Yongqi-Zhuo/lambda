use std::{collections::HashMap, fmt};

use crate::core::{Term, TermAbs, Type};
use crate::intrinsic;

// Implement a simple Hindley-Milner type inference algorithm

pub struct Context {
    defs: HashMap<String, Type>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            defs: HashMap::new(),
        }
    }

    pub fn extended(&self, x: &str, t: Type) -> Self {
        let mut defs = self.defs.clone();
        defs.insert(x.to_string(), t);
        Self { defs }
    }

    pub fn lookup(&self, x: &str) -> Type {
        self.defs.get(x).unwrap().clone()
    }
}

#[derive(Debug)]
pub struct InferContext {
    counter: i32,
    env: HashMap<String, Type>,
}

impl Type {
    fn subst(&self, var: &str, ty: &Type) -> Type {
        match self {
            Type::TBool => Type::TBool,
            Type::TNat => Type::TNat,
            Type::TVar(x) if x == var => ty.clone(),
            Type::TVar(_) => self.clone(),
            Type::TArrow(t1, t2) => {
                Type::TArrow(
                    Box::new(t1.subst(var, ty)),
                    Box::new(t2.subst(var, ty)),
                )
            }
        }
    }
}

impl InferContext {
    pub fn new() -> Self {
        Self {
            counter: 0,
            env: HashMap::new(),
        }
    }

    fn fresh(&mut self) -> Type {
        let ty = Type::TVar(format!("_{}", self.counter));
        self.counter += 1;
        ty
    }

    fn subst(&mut self, var: &str, ty: &Type) {
        for (_, t) in self.env.iter_mut() {
            *t = t.subst(var, ty);
        }
        let res = self.env.insert(var.to_string(), ty.clone());
        assert!(res.is_none(), "Variable {} already eliminated", var);
    }

    fn apply(&self, ty: &Type) -> Type {
        match ty {
            Type::TVar(x) => self.env.get(x).unwrap_or(ty).clone(),
            Type::TArrow(t1, t2) => {
                Type::TArrow(
                    Box::new(self.apply(t1)),
                    Box::new(self.apply(t2)),
                )
            }
            _ => ty.clone(),
        }
    }

    fn unify(&mut self, t1: &Type, t2: &Type) {
        let t1 = self.apply(t1);
        let t2 = self.apply(t2);
        match (&t1, &t2) {
            (Type::TBool, Type::TBool) | (Type::TNat, Type::TNat) => {}
            (Type::TVar(x), Type::TVar(y)) if x == y => {}
            (Type::TVar(x), _) => {
                self.subst(x, &t2);
            }
            (_, Type::TVar(x)) => {
                self.subst(x, &t1);
            }
            (Type::TArrow(t11, t12), Type::TArrow(t21, t22)) => {
                self.unify(t11, t21);
                self.unify(t12, t22);
            }
            _ => panic!("Cannot unify {:?} and {:?}", t1, t2),
        }
    }

    fn infer(&mut self, ctx: &Context, t: &Term) -> Type {
        match t {
            Term::Var(x) => ctx.lookup(x),
            Term::Bool(_) => Type::TBool,
            Term::Nat(_) => Type::TNat,
            Term::Abs(abs) => {
                let ty = abs.ty.clone().unwrap_or_else(|| self.fresh());
                let ty = self.apply(&ty);
                let ctx = ctx.extended(&abs.x, ty.clone());
                let t = self.infer(&ctx, &abs.t);
                Type::TArrow(Box::new(ty), Box::new(t))
            }
            Term::Intrinsic(intrinsic) => {
                let typing = &intrinsic::lookup(*intrinsic).typing;
                Type::TArrow(Box::new(typing.arg.clone()), Box::new(typing.ret.clone()))
            }
            Term::App(t1, t2) => {
                let t1 = self.infer(ctx, t1);
                let t2 = self.infer(ctx, t2);
                let ty = self.fresh();
                self.unify(&t1, &Type::TArrow(Box::new(t2), Box::new(ty.clone())));
                ty
            }
            Term::If(t1, t2, t3) => {
                let t1 = self.infer(ctx, t1);
                let t2 = self.infer(ctx, t2);
                let t3 = self.infer(ctx, t3);
                self.unify(&t1, &Type::TBool);
                self.unify(&t2, &t3);
                t2
            }
        }
    }

    pub fn infer_top(&mut self, ctx: &Context, t: &Term) -> Type {
        let ty = self.infer(ctx, t);
        self.apply(&ty)
    }
}

pub struct StructuralEqError {
    t1: Type,
    t2: Type,
}

impl fmt::Display for StructuralEqError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Cannot unify {} and {}", self.t1, self.t2)
    }
}

fn structural_eq_impl(t1: &Type, t2: &Type, env: &mut HashMap<String, String>) -> Result<(), StructuralEqError> {
    match (t1, t2) {
        (Type::TBool, Type::TBool) | (Type::TNat, Type::TNat) => Ok(()),
        (Type::TVar(x), Type::TVar(y)) => {
            let x = env.entry(x.clone()).or_insert_with(|| y.clone());
            if x == y {
                Ok(())
            } else {
                Err(StructuralEqError { t1: t1.clone(), t2: t2.clone() })
            }
        }
        (Type::TArrow(t11, t12), Type::TArrow(t21, t22)) => {
            structural_eq_impl(t11, t21, env)?;
            structural_eq_impl(t12, t22, env)?;
            Ok(())
        }
        _ => Err(StructuralEqError { t1: t1.clone(), t2: t2.clone() }),
    }
}

pub fn structural_eq(t1: &Type, t2: &Type) -> Result<(), StructuralEqError> {
    structural_eq_impl(t1, t2, &mut HashMap::new())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_infer() {
        let mut infer = InferContext::new();
        let ctx = Context::new();
        let t = Term::Abs(TermAbs::new(
            "x".to_string(),
            None,
            Box::new(Term::Var("x".to_string())),
        ));
        let ty = infer.infer_top(&ctx, &t);
        assert_eq!(ty, Type::TArrow(Box::new(Type::TVar("_0".to_string())), Box::new(Type::TVar("_0".to_string()))));
    }
}
