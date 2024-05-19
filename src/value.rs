use std::{collections::HashMap, fmt, rc::Rc};

use crate::core::{Intrinsic, TermAbs};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Captures<'a> {
    pub captures: HashMap<String, Rc<Value<'a>>>,
}

impl<'a> Captures<'a> {
    pub fn new() -> Self {
        Self {
            captures: HashMap::new(),
        }
    }
    pub fn extend(&mut self, x: &str, v: Rc<Value<'a>>) {
        self.captures.insert(x.to_string(), v);
    }
    pub fn extended(&self, x: &str, v: Rc<Value<'a>>) -> Self {
        let mut captures = self.captures.clone();
        captures.insert(x.to_string(), v);
        Captures { captures }
    }
    pub fn lookup(&self, x: &str) -> Rc<Value<'a>> {
        self.captures
            .get(x)
            .expect(&format!("Unbound variable: {}", x))
            .clone()
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct ValueAbs<'a> {
    pub term: &'a TermAbs,
    pub captures: Captures<'a>,
}

impl<'a> ValueAbs<'a> {
    pub fn new(term: &'a TermAbs, captures: Captures<'a>) -> Self {
        Self { term, captures }
    }
}

impl<'a> Into<Value<'a>> for ValueAbs<'a> {
    fn into(self) -> Value<'a> {
        Value::VAbs(self)
    }
}

impl fmt::Display for ValueAbs<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<function>")
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Value<'a> {
    VBool(bool),
    VNat(u32),
    VAbs(ValueAbs<'a>),
    VIntrinsic(Intrinsic),
}

impl fmt::Display for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::VBool(b) => write!(f, "{}", b),
            Value::VNat(n) => write!(f, "{}", n),
            Value::VAbs(abs) => write!(f, "{}", abs),
            Value::VIntrinsic(intrinsic) => write!(f, "{:?}", intrinsic),
        }
    }
}
