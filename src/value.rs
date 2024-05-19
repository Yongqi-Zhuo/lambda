use std::{collections::HashMap, rc::Rc};

use crate::core::{Intrinsic, TermAbs};

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Captures<'a> {
    pub captures: HashMap<String, Rc<Value<'a>>>,
}

impl<'a> Captures<'a> {
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

#[derive(Debug, Eq, PartialEq)]
pub enum Value<'a> {
    VBool(bool),
    VNat(u32),
    VAbs(ValueAbs<'a>),
    VIntrinsic(Intrinsic),
}
