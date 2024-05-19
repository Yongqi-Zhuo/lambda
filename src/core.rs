use std::fmt;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Intrinsic {
    IsZero,
    Succ,
    Pred,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Type {
    TBool,
    TNat,
    TVar(String),
    TArrow(Box<Type>, Box<Type>),
}

impl Type {
    fn format(&self, f: &mut fmt::Formatter, atom: bool) -> fmt::Result {
        match self {
            Type::TBool => write!(f, "Bool"),
            Type::TNat => write!(f, "Nat"),
            Type::TVar(x) => write!(f, "{}", x),
            Type::TArrow(t1, t2) => {
                if atom {
                    write!(f, "(")?;
                    t1.format(f, true)?;
                    write!(f, " -> ")?;
                    t2.format(f, false)?;
                    write!(f, ")")?;
                } else {
                    t1.format(f, true)?;
                    write!(f, " -> ")?;
                    t2.format(f, false)?;
                }
                Ok(())
            }
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.format(f, false)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct TermAbs {
    pub x: String,
    pub ty: Option<Type>,
    pub t: Box<Term>,
}

impl TermAbs {
    pub fn new(x: String, ty: Option<Type>, t: Box<Term>) -> Self {
        Self { x, ty, t }
    }
}

impl Into<Term> for TermAbs {
    fn into(self) -> Term {
        Term::Abs(self)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Term {
    Var(String),
    Bool(bool),
    Nat(u32),
    Abs(TermAbs),
    Intrinsic(Intrinsic),
    App(Box<Term>, Box<Term>),
    If(Box<Term>, Box<Term>, Box<Term>),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_display() {
        assert_eq!(format!("{}", Type::TBool), "Bool");
        assert_eq!(format!("{}", Type::TNat), "Nat");
        assert_eq!(format!("{}", Type::TVar("a".to_string())), "a");
        assert_eq!(format!("{}", Type::TArrow(Box::new(Type::TBool), Box::new(Type::TNat))), "Bool -> Nat");
        assert_eq!(format!("{}", Type::TArrow(Box::new(Type::TArrow(Box::new(Type::TBool), Box::new(Type::TNat))), Box::new(Type::TBool))), "(Bool -> Nat) -> Bool");
        assert_eq!(format!("{}", Type::TArrow(Box::new(Type::TBool), Box::new(Type::TArrow(Box::new(Type::TBool), Box::new(Type::TNat)))),), "Bool -> Bool -> Nat");
    }
}
