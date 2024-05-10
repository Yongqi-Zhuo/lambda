use std::collections::HashMap;

use super::core::{Intrinsic, Term};
use super::intrinsic::INTRINSICS;

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'a> {
    LParen,
    RParen,
    Dot,
    Lambda,
    Ident(&'a str),
    Bool(bool),
    Nat(u32),
    Intrinsic(Intrinsic),
    If, Then, Else,
}

const KEYWORDS: [(&str, Token::<'static>); 5] = [
    ("true", Token::Bool(true)),
    ("false", Token::Bool(false)),
    ("if", Token::If),
    ("then", Token::Then),
    ("else", Token::Else),
];

pub struct Tokenizer<'a> {
    input: &'a str,
    iter: std::iter::Peekable<std::str::CharIndices<'a>>,
    keywords: HashMap<&'static str, Token<'static>>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            iter: input.char_indices().peekable(),
            keywords: HashMap::<&'static str, Token<'static>>::from_iter(
                KEYWORDS.iter().cloned().chain(
                    INTRINSICS.iter().map(
                        |op| (op.name, Token::Intrinsic(op.op))
                    )
                )
            ),
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.iter.peek() {
                Some(&(_, c)) if c.is_whitespace() => {
                    self.iter.next();
                }
                Some(&(_, '(')) => {
                    self.iter.next();
                    return Some(Token::LParen);
                }
                Some(&(_, ')')) => {
                    self.iter.next();
                    return Some(Token::RParen);
                }
                Some(&(_, '.')) => {
                    self.iter.next();
                    return Some(Token::Dot);
                }
                Some(&(_, 'λ')) => {
                    self.iter.next();
                    return Some(Token::Lambda);
                }
                Some(&(_, '0'..='9')) => {
                    let mut n = 0;
                    while let Some(&(_, c)) = self.iter.peek() {
                        if c.is_digit(10) {
                            n = n * 10 + c.to_digit(10).unwrap();
                            self.iter.next();
                        } else {
                            break;
                        }
                    }
                    return Some(Token::Nat(n));
                }
                Some(&(start, c)) => {
                    if !c.is_alphabetic() {
                        panic!("Unexpected character: {}", c);
                    }
                    // We need to return a slice of the input string
                    let end;
                    loop {
                        if let Some(&(i, c)) = self.iter.peek() {
                            if c.is_alphanumeric() {
                                self.iter.next();
                            } else {
                                end = i;
                                break;
                            }
                        }  else {
                            end = self.input.len();
                            break;
                        }
                    }
                    let token = &self.input[start..end];
                    if let Some(token) = self.keywords.get(token) {
                        return Some(token.clone());
                    }
                    return Some(Token::Ident(token));
                }
                None => return None,
            }
        }
    }
}

pub struct Parser<'a> {
    tokenizer: std::iter::Peekable<Tokenizer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            tokenizer: Tokenizer::new(input).peekable(),
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        self.tokenizer.next()
    }

    fn peek_token(&mut self) -> Option<&Token> {
        self.tokenizer.peek()
    }

    fn consume(&mut self, token: Token<'a>) -> Token<'a> {
        if let Some(t) = self.tokenizer.next() {
            if t != token {
                panic!("Expected {:?}, got {:?}", token, t);
            }
            t
        } else {
            panic!("Expected {:?}, got EOF", token);
        }
    }

    fn consume_ident(&mut self) -> String {
        match self.next_token() {
            Some(Token::Ident(x)) => x.to_string(),
            tok => panic!("Expected identifier, got {:?}", tok),
        }
    }

    fn consume_bool(&mut self) -> bool {
        match self.next_token() {
            Some(Token::Bool(b)) => b,
            tok => panic!("Expected boolean, got {:?}", tok),
        }
    }

    fn consume_nat(&mut self) -> u32 {
        match self.next_token() {
            Some(Token::Nat(n)) => n,
            tok => panic!("Expected number, got {:?}", tok),
        }
    }

    fn consume_intrinsic(&mut self) -> Intrinsic {
        match self.next_token() {
            Some(Token::Intrinsic(intrinsic)) => intrinsic,
            tok => panic!("Expected intrinsic, got {:?}", tok),
        }
    }

    pub fn parse(&mut self) -> Term {
        self.parse_term()
    }

    fn parse_term(&mut self) -> Term {
        let mut t = self.parse_atom().unwrap();
        while let Some(atom) = self.parse_atom() {
            t = Term::App(Box::new(t), Box::new(atom));
        }
        t
    }

    fn parse_atom(&mut self) -> Option<Term> {
        match self.peek_token() {
            Some(Token::LParen) => {
                self.consume(Token::LParen);
                let t = self.parse_term();
                self.consume(Token::RParen);
                Some(t)
            }
            Some(Token::Lambda) => {
                self.consume(Token::Lambda);
                let x = self.consume_ident();
                self.consume(Token::Dot);
                let t = self.parse_term();
                Some(Term::Abs(x, Box::new(t)))
            }
            Some(Token::Bool(_)) => Some(Term::Bool(self.consume_bool())),
            Some(Token::Nat(_)) => Some(Term::Nat(self.consume_nat())),
            Some(Token::Intrinsic(_)) => Some(Term::Intrinsic(self.consume_intrinsic())),
            Some(Token::If) => {
                self.consume(Token::If);
                let t1 = self.parse_term();
                self.consume(Token::Then);
                let t2 = self.parse_term();
                self.consume(Token::Else);
                let t3 = self.parse_term();
                Some(Term::If(Box::new(t1), Box::new(t2), Box::new(t3)))
            }
            Some(Token::Ident(_)) => Some(Term::Var(self.consume_ident())),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenizer() {
        let input = "λx. λy. x y true 123";
        let mut tokenizer = Tokenizer::new(input);
        assert_eq!(tokenizer.next(), Some(Token::Lambda));
        assert_eq!(tokenizer.next(), Some(Token::Ident("x")));
        assert_eq!(tokenizer.next(), Some(Token::Dot));
        assert_eq!(tokenizer.next(), Some(Token::Lambda));
        assert_eq!(tokenizer.next(), Some(Token::Ident("y")));
        assert_eq!(tokenizer.next(), Some(Token::Dot));
        assert_eq!(tokenizer.next(), Some(Token::Ident("x")));
        assert_eq!(tokenizer.next(), Some(Token::Ident("y")));
        assert_eq!(tokenizer.next(), Some(Token::Bool(true)));
        assert_eq!(tokenizer.next(), Some(Token::Nat(123)));
        assert_eq!(tokenizer.next(), None);
    }

    #[test]
    fn test_parser() {
        let mut parser = Parser::new("λf. (λx. f (λv. x x v)) (λx. f (λv. x x v))");
        let term = parser.parse();
        let fix: Term = Term::Abs(
            "f".to_string(),
            Box::new(Term::App(
                Box::new(Term::Abs(
                    "x".to_string(),
                    Box::new(Term::App(
                        Box::new(Term::Var("f".to_string())),
                        Box::new(Term::Abs(
                            "v".to_string(),
                            Box::new(Term::App(
                                Box::new(Term::App(
                                    Box::new(Term::Var("x".to_string())),
                                    Box::new(Term::Var("x".to_string())),
                                )),
                                Box::new(Term::Var("v".to_string())),
                            )),
                        )),
                    )),
                )),
                Box::new(Term::Abs(
                    "x".to_string(),
                    Box::new(Term::App(
                        Box::new(Term::Var("f".to_string())),
                        Box::new(Term::Abs(
                            "v".to_string(),
                            Box::new(Term::App(
                                Box::new(Term::App(
                                    Box::new(Term::Var("x".to_string())),
                                    Box::new(Term::Var("x".to_string())),
                                )),
                                Box::new(Term::Var("v".to_string())),
                            )),
                        )),
                    )),
                )),
            ))
        );
        assert_eq!(term, fix);
    }
}
