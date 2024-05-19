use std::io::{self, Write};
use std::rc::Rc;

use crate::core::{NameGenerator, Stmt, Term, Type};
use crate::eval::eval;
use crate::intrinsic::{Prologue, prologue};
use crate::parser::Parser;
use crate::typing::{Context, InferContext};
use crate::value::{Captures, Value};

pub struct REPL<'a> {
    gen: &'a mut NameGenerator,
    history: Vec<Box<Term>>,
    ctx: Context,
    captures: Captures<'a>,
}

impl<'a> REPL<'a> {
    pub fn new(gen: &'a mut NameGenerator) -> Self {
        let mut repl = Self {
            gen,
            history: Vec::new(),
            ctx: Context::new(),
            captures: Captures::new(),
        };
        for Prologue { name, ty, term } in prologue(repl.gen) {
            let term = repl.add_to_history(term);
            let v = repl.eval(&term);
            repl.define(name, ty, v);
        }
        repl
    }

    fn infer(&mut self, term: &Term) -> Type {
        // Constructing a new InferContext every time enables let polymorphism
        InferContext::new(&mut self.gen).infer_top(&self.ctx, term)
    }

    fn add_to_history(&mut self, term: Term) -> &'a Term {
        self.history.push(Box::new(term));
        unsafe {
            // SAFETY: term lives as long as history, because we only push to history
            std::mem::transmute(self.history.last().unwrap().as_ref())
        }
    }

    fn eval(&self, term: &'a Term) -> Rc<Value<'a>> {
        eval(&self.captures, term)
    }

    fn define(&mut self, var: &str, ty: Type, v: Rc<Value<'a>>) {
        self.ctx.extend(var, ty);
        self.captures.extend(var, v);
    }

    pub fn run(&mut self) -> Result<(), io::Error> {
        let mut line = String::new();
        loop {
            line.clear();
            print!("> ");
            io::stdout().flush()?;
            if io::stdin().read_line(&mut line)? == 0 {
                break;
            }
            let line = line.trim();
            if line.is_empty() {
                continue;
            }

            let Stmt { var, term } = Parser::new(line).parse_stmt();
            let ty = self.infer(&term);
            if let Some(var) = var {
                let term = self.add_to_history(term);
                let v = self.eval(&term);
                println!("{} :: {}", v, ty);
                self.define(&var, ty, v);
            } else {
                let v = self.eval(&term);
                println!("{} :: {}", v, ty);
            }
        }
        Ok(())
    }
}
