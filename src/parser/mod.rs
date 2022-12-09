use crate::lexer::TokenKind;
use chumsky::{
    prelude::Simple, primitive::just, recovery::nested_delimiters, recursive::recursive, select,
    Parser,
};
use intaglio::Symbol;

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Shr,
    Shl,
    Eq,
    NegEq,
    Assign,
    Insert,
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
    Tuple(Vec<(Symbol, Option<TypeKind>)>),
}

pub type Spanned<T> = (T, logos::Span);
pub type SpannedExpr = Spanned<Expression>;
pub type HeapExpr = Box<SpannedExpr>;
pub type ExprError = Simple<TokenKind, logos::Span>;

pub fn parse() -> impl Parser<TokenKind, Vec<HeapExpr>, Error = ExprError> {
    parse_aggregate().repeated()
}

fn parse_aggregate() -> impl Parser<TokenKind, HeapExpr, Error = ExprError> + Clone {
    recursive(|expr| {
        let atom = parse_value()
            .or(parse_identifier())
            .or(parse_tuple())
            .map_with_span(|expr, span| (expr, span))
            .or(expr
                .clone()
                .delimited_by(just(TokenKind::GroupOpen), just(TokenKind::GroupClose)))
            .recover_with(nested_delimiters(
                TokenKind::GroupOpen,
                TokenKind::GroupClose,
                [
                    (TokenKind::ArrayOpen, TokenKind::ArrayClose),
                    (TokenKind::TupleOpen, TokenKind::TupleClose),
                ],
                |span| (Expression::Error, span),
            ))
            .recover_with(nested_delimiters(
                TokenKind::ArrayOpen,
                TokenKind::ArrayClose,
                [
                    (TokenKind::GroupOpen, TokenKind::GroupClose),
                    (TokenKind::TupleOpen, TokenKind::TupleClose),
                ],
                |span| (Expression::Error, span),
            ))
            .recover_with(nested_delimiters(
                TokenKind::TupleOpen,
                TokenKind::TupleClose,
                [
                    (TokenKind::GroupOpen, TokenKind::GroupClose),
                    (TokenKind::ArrayOpen, TokenKind::ArrayClose),
                ],
                |span| (Expression::Error, span),
            ));

        /* parse binary expressions */
        let op = select! { TokenKind::Shr => Operator::Shr, TokenKind::Shl => Operator::Shl };
        let shift = atom
            .clone()
            .then(op.then(atom).repeated())
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;
                (Expression::Binary(Box::new(a), op, Box::new(b)), span)
            });

        let op = select! { TokenKind::Add => Operator::Add, TokenKind::Sub => Operator::Sub };
        let sum = shift
            .clone()
            .then(op.then(shift).repeated())
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;
                (Expression::Binary(Box::new(a), op, Box::new(b)), span)
            });

        let op = select! { TokenKind::Mul => Operator::Mul, TokenKind::Div => Operator::Div };
        let product = sum
            .clone()
            .then(op.then(sum).repeated())
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;
                (Expression::Binary(Box::new(a), op, Box::new(b)), span)
            });

        let op = select! { TokenKind::Eq => Operator::Eq, TokenKind::NegEq => Operator::NegEq };
        let eq = product
            .clone()
            .then(op.then(product).repeated())
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;
                (Expression::Binary(Box::new(a), op, Box::new(b)), span)
            });

        let op = select! { TokenKind::Insert => Operator::Insert };
        let insert = eq.clone().then(op.then(eq).repeated()).foldl(|a, (op, b)| {
            let span = a.1.start..b.1.end;
            (Expression::Binary(Box::new(a), op, Box::new(b)), span)
        });

        // `assign` is the last parsed operator
        let op = select! { TokenKind::Assign => Operator::Assign };
        insert
            .clone()
            .then(op.then(insert).repeated())
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;
                (Expression::Binary(Box::new(a), op, Box::new(b)), span)
            })
    })
    // map the final parser to a boxed expression.
    .map(Box::new)
}

fn parse_value() -> impl Parser<TokenKind, Expression, Error = ExprError> + Clone {
    select! {
        TokenKind::Integer(x) => Expression::Value(Value::Int(x)),
        TokenKind::Boolean(x) => Expression::Value(Value::Bool(x)),
        TokenKind::String(x) => Expression::Value(Value::String(x)),
    }
    .labelled("value")
}

fn parse_type() -> impl Parser<TokenKind, TypeKind, Error = ExprError> + Clone {
    select! {
        TokenKind::TypeInt => TypeKind::Int,
        TokenKind::TypeBool => TypeKind::Bool,
        TokenKind::TypeString => TypeKind::String,
    }
    .labelled("type")
}

fn parse_symbol() -> impl Parser<TokenKind, Symbol, Error = ExprError> + Clone {
    select! { TokenKind::Symbol(name) => name }.labelled("identifier")
}

fn parse_identifier() -> impl Parser<TokenKind, Expression, Error = ExprError> + Clone {
    parse_symbol().map(Expression::Identifier)
}

fn parse_tuple() -> impl Parser<TokenKind, Expression, Error = ExprError> + Clone {
    parse_symbol()
        .then_ignore(just(TokenKind::Assign))
        .then(parse_type().or_not())
        .repeated()
        .delimited_by(just(TokenKind::TupleOpen), just(TokenKind::TupleClose))
        .map(Expression::Tuple)
}
