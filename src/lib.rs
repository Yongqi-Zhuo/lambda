mod core;
mod eval;
mod intrinsic;
mod parser;
mod repl;
mod typing;
mod value;

pub use core::{Intrinsic, NameGenerator, Stmt, Term, Type};
pub use eval::eval;
pub use intrinsic::{fix_combinator, prologue};
pub use parser::{Parser, Token, Tokenizer};
pub use repl::REPL;
pub use typing::{Context, InferContext, structural_eq};
pub use value::{Captures, Value};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eval_and_typing() {
        // fix = λf. (λx. f (λv. x x v)) (λx. f (λv. x x v))
        let fix = Parser::new("λf. (λx. f (λv. x x v)) (λx. f (λv. x x v))").parse_term();
        // plus = λm. λn. if (iszero m) n (plus (pred m) (succ n))
        // plus = fix (λf. λm. λn. if iszero m then n else (f (pred m) (succ n)))
        // because we have not defined the prologue, we must use the intrinsic names
        let helper = Parser::new("λf. λm. λn. if __iszero m then n else (f (__pred m) (__succ n))").parse_term();
        {
            // Check typing
            let mut gen = NameGenerator::new();
            let mut infer = typing::InferContext::new(&mut gen);
            let ctx = Context::new();
            let ty = infer.infer_top(&ctx, &helper);
            let expected = Parser::new("(Nat -> Nat -> Nat) -> Nat -> Nat -> Nat").parse_type();
            assert_eq!(ty, expected);
        }
        let plus = Term::App(
            Box::new(fix),
            Box::new(helper),
        );
        let t = Term::App(
            Box::new(Term::App(Box::new(plus), Box::new(Term::Nat(3)))),
            Box::new(Term::Nat(2)),
        );
        let captures = Captures::new();
        let v = eval(&captures, &t);
        assert_eq!(*v, Value::VNat(5));
    }
}
