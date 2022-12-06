use chumsky::{
    error::SimpleReason,
    prelude::Simple,
    primitive::{choice, just},
    recovery::nested_delimiters,
    recursive::recursive,
    select, Parser, combinator::Foldl,
};
use intaglio::Symbol;

use crate::lexer::{Token, TokenKind};

#[derive(Debug, PartialEq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Shr,
    Shl,
}

#[derive(Debug, PartialEq)]
pub enum Value {
    Int(isize),
    Bool(bool),
    String(Symbol),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Operator(Operator),
    Value(Value),
    Identifier(Symbol),

    Binary(Box<Self>, Operator, Box<Self>),

    Grouping(Box<Self>),
}

type ParserError = Simple<TokenKind, logos::Span>;

// fn parser() -> impl Parser<TokenKind, Vec<Expression>, Error = ParserError> {
//     recursive(|p| choice(parsers))
// }

fn parse_binary_expr() -> impl Parser<TokenKind, Expression, Error = ParserError> {}

fn parse() -> impl Parser<TokenKind, Expression, Error = ParserError> {
    recursive(|expr| {
            expr.then(parse_operator())
            
    })
}

fn parse_value() -> impl Parser<TokenKind, Expression, Error = ParserError> {
    select! {
        TokenKind::Integer(x) => Expression::Value(Value::Int(x)),
        TokenKind::Boolean(x) => Expression::Value(Value::Bool(x)),
        TokenKind::String(x) => Expression::Value(Value::String(x)),
    }
    .labelled("value")
}

fn parse_ident() -> impl Parser<TokenKind, Expression, Error = ParserError> {
    select! { TokenKind::Identifier(name) => Expression::Identifier(name) }.labelled("identifier")
}

fn parse_operator()-> impl Parser<TokenKind, Expression, Error = ParserError> {
    select! {
        TokenKind::Add => Expression::Operator(Operator::Add),
        TokenKind::Sub => Expression::Operator(Operator::Sub),
        TokenKind::Mul => Expression::Operator(Operator::Mul),
        TokenKind::Div => Expression::Operator(Operator::Div),
        TokenKind::Shr => Expression::Operator(Operator::Shr),
        TokenKind::Shl => Expression::Operator(Operator::Shl),
    }
}

// mod expr;
// pub use expr::*;

// use crate::lexer::{Token, TokenKind};

// #[derive(Debug)]
// pub enum ParserError {
//     NoMoreTokens,

//     Discard,

//     ExpectedToken {
//         expected: TokenKind,
//         found: Option<TokenKind>,
//     },

//     FoundMsg {
//         found: Option<TokenKind>,
//         msg: String,
//     },

//     General(String),
// }

// pub struct Parser<'a> {
//     tokens: crate::lexer::LexerIterator<'a>,
// }

// impl<'a> Parser<'a> {
//     pub fn new(tokens: crate::lexer::LexerIterator<'a>) -> Self {
//         Self { tokens }
//     }

//     pub(crate) fn peek(&mut self) -> Option<&TokenKind> {
//         self.tokens.peek().map(Token::kind)
//     }

//     pub(crate) fn expect(&mut self, expect: &TokenKind) -> Result<(), ParserError> {
//         match self.peek() {
//             Some(kind) if kind.eq(expect) => {
//                 self.discard();

//                 Ok(())
//             }

//             kind => Err(ParserError::ExpectedToken {
//                 expected: expect.clone(),
//                 found: kind.cloned(),
//             }),
//         }
//     }

//     pub(crate) fn expect_with<T>(
//         &mut self,
//         expect_fn: impl FnOnce(&TokenKind) -> Result<T, ParserError>,
//     ) -> Result<T, ParserError> {
//         match self.peek() {
//             Some(kind) => {
//                 let result = expect_fn(kind);
//                 if result.is_ok() {
//                     self.discard();
//                 }

//                 result
//             }

//             None => Err(ParserError::NoMoreTokens),
//         }
//     }

//     pub(crate) fn discard(&mut self) {
//         self.tokens.next().expect("cannot discard no tokens");
//     }

//     pub(crate) fn throw(&mut self, msg: &str) -> ! {
//         let token_src = match self.tokens.next() {
//             Some(t) => self.tokens.find_token(&t),
//             None => None,
//         };
//         crate::throw_error(msg, Some(self.tokens.src()), token_src)
//     }
// }

// impl Iterator for Parser<'_> {
//     type Item = Expression;

//     fn next(&mut self) -> Option<Self::Item> {
//         match Expression::try_from(self) {
//             Ok(expr) => Some(expr),

//             Err(ParserError::NoMoreTokens) => None,

//             Err(err) => {
//                 // FIXME: make this output real errors
//                 panic!("{:?}", err)
//             }
//         }
//     }
// }
