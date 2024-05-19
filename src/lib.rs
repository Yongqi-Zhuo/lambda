mod core;
mod eval;
mod intrinsic;
mod parser;
mod typing;
mod value;

pub use core::{Intrinsic, Term, Type};
pub use eval::Evaluator;
pub use parser::{Parser, Token, Tokenizer};
pub use typing::{Context, InferContext, structural_eq};
pub use value::{Captures, Value};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eval_and_typing() {
        // fix = λf. (λx. f (λv. x x v)) (λx. f (λv. x x v))
        let fix = Parser::new("λf. (λx. f (λv. x x v)) (λx. f (λv. x x v))").parse();
        // plus = λm. λn. if (iszero m) n (plus (pred m) (succ n))
        // plus = fix (λf. λm. λn. if iszero m then n else (f (pred m) (succ n)))
        let helper = Parser::new("λf. λm. λn. if iszero m then n else (f (pred m) (succ n))").parse();
        {
            // Check typing
            let mut infer = typing::InferContext::new();
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
        let evaluator = Evaluator::new();
        let captures: Captures = Default::default();
        let v = evaluator.eval(&captures, &t);
        assert_eq!(*v, Value::VNat(5));
    }
}
