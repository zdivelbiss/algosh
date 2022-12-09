use chumsky::{
    prelude::Simple, primitive::just, recovery::nested_delimiters, recursive::recursive, select,
    Parser,
};
use intaglio::Symbol;

use crate::lexer::{Token, TokenKind};

// #[derive(Debug)]
// pub enum ParserError {
//     UnexpectedToken,

//     NoMoreTokens,

//     Expected {
//         expected: Token,
//         found: Option<Token>,
//     },

//     FoundMsg {
//         found: Option<Token>,
//         msg: String,
//     },

//     Msg(String),

//     ReplaceThisError,
// }

// pub struct Parser<'a> {
//     tokens: crate::lexer::LexerIterator<'a>,
// }

// impl<'a> Parser<'a> {
//     pub fn new(tokens: crate::lexer::LexerIterator<'a>) -> Self {
//         Self { tokens }
//     }

//     pub(crate) fn peek(&mut self) -> Option<&Token> {
//         self.tokens.peek()
//     }

//     pub(crate) fn expect(&mut self, expect: &Token) -> Result<(), ParserError> {
//         match self.peek() {
//             Some(kind) if kind.eq(expect) => {
//                 self.advance();

//                 Ok(())
//             }

//             kind => Err(ParserError::Expected {
//                 expected: expect.clone(),
//                 found: kind.cloned(),
//             }),
//         }
//     }

//     pub(crate) fn expect_with<T>(
//         &mut self,
//         expect_fn: impl FnOnce(&Token) -> Result<T, ParserError>,
//     ) -> Result<T, ParserError> {
//         match self.peek() {
//             Some(token) => {
//                 let result = expect_fn(token);
//                 if result.is_ok() {
//                     self.advance();
//                 }

//                 result
//             }

//             None => Err(ParserError::ReplaceThisError),
//         }
//     }

//     pub(crate) fn advance(&mut self) {
//         self.tokens.next().expect("cannot discard no tokens");
//     }

//     #[allow(dead_code)]
//     pub(crate) fn throw(&mut self, msg: &str) -> ! {
//         let token_src = match self.tokens.next() {
//             Some(t) => self.tokens.find_token(&t),
//             None => None,
//         };
//         crate::throw_error(msg, Some(self.tokens.src()), token_src)
//     }
// }

// impl Iterator for Parser<'_> {
//     type Item = expr::HeapExpr;

//     fn next(&mut self) -> Option<Self::Item> {
//         if self.tokens.peek().is_some() {
//             match expr::parse_expr(self) {
//                 Ok(expr) => Some(expr),

//                 Err(ParserError::ReplaceThisError) => {
//                     println!("REPALCE ERROR");
//                     None
//                 }

//                 Err(err) => {
//                     // FIXME: make this output real errors
//                     panic!("{:?}", err)
//                 }
//             }
//         } else {
//             None
//         }
//     }
// }

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
pub enum TypeKind {
    Int,
    Bool,
    String,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Error,
    Value(Value),
    Identifier(Symbol),
    Binary(HeapExpr, Operator, HeapExpr),
    Func(Vec<(Symbol, TypeKind)>, HeapExpr),
}

pub type HeapExpr = Box<Expression>;
pub type Spanned<T> = (T, logos::Span);
pub type ExprError = Simple<TokenKind, logos::Span>;

pub fn parse() -> impl Parser<TokenKind, Vec<HeapExpr>, Error = ExprError> {
    parse_aggregate().repeated()
}

fn parse_aggregate() -> impl Parser<TokenKind, HeapExpr, Error = ExprError> + Clone {
    recursive(|expr| {
        let value = select! {
            TokenKind::Integer(x) => Expression::Value(Value::Int(x)),
            TokenKind::Boolean(x) => Expression::Value(Value::Bool(x)),
            TokenKind::String(x) => Expression::Value(Value::String(x)),
        }
        .map(Box::new)
        .labelled("value");

        let ty = select! {
            TokenKind::TypeInt => TypeKind::Int,
            TokenKind::TypeBool => TypeKind::Bool,
            TokenKind::TypeString => TypeKind::String,
        }
        .labelled("type");

        let op = select! {
            TokenKind::Add => Operator::Add,
            TokenKind::Sub => Operator::Sub,
            TokenKind::Mul => Operator::Mul,
            TokenKind::Div => Operator::Div,
            TokenKind::Shr => Operator::Shr,
            TokenKind::Shl => Operator::Shl,
        }
        .labelled("operator");

        let ident = select! { TokenKind::Identifier(name) => name }.labelled("identifier");

        let binary = expr
            .clone()
            .then(op.then(expr.clone()).repeated())
            .foldl(|a, (op, b)| Box::new(Expression::Binary(a, op, b)));

        let params = ident
            .clone()
            .then_ignore(just(TokenKind::Assign))
            .then(ty)
            .separated_by(just(TokenKind::Separator))
            .delimited_by(
                just(TokenKind::ParameterBrace),
                just(TokenKind::ParameterBrace),
            )
            .labelled("parameters");

        let func = params
            .clone()
            .then(expr.clone())
            .map(|(params, expr)| Box::new(Expression::Func(params, expr)));

        value
            .or(ident.map(|i| Box::new(Expression::Identifier(i))))
            .or(binary)
            .or(func)
            .or(expr
                .clone()
                .delimited_by(just(TokenKind::GroupOpen), just(TokenKind::GroupClose)))

        // .map_with_span(|expr, span| (expr, span))
        // .recover_with(nested_delimiters(
        //     TokenKind::GroupOpen,
        //     TokenKind::GroupClose,
        //     [
        //         (TokenKind::ArrayOpen, TokenKind::ArrayClose),
        //         (TokenKind::TupleOpen, TokenKind::TupleClose),
        //     ],
        //     |span| (Box::new(Expression::Error), span),
        // ))
        // .recover_with(nested_delimiters(
        //     TokenKind::ArrayOpen,
        //     TokenKind::ArrayClose,
        //     [
        //         (TokenKind::GroupOpen, TokenKind::GroupClose),
        //         (TokenKind::TupleOpen, TokenKind::TupleClose),
        //     ],
        //     |span| (Box::new(Expression::Error), span),
        // ))
        // .recover_with(nested_delimiters(
        //     TokenKind::TupleOpen,
        //     TokenKind::TupleClose,
        //     [
        //         (TokenKind::GroupOpen, TokenKind::GroupClose),
        //         (TokenKind::ArrayOpen, TokenKind::ArrayClose),
        //     ],
        //     |span| (Box::new(Expression::Error), span),
        // ))
    })
}
