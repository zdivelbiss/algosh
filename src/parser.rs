use crate::lexer::{Token, TokenKind};
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
}

#[derive(Debug, Clone, PartialEq)]
pub enum Control {
    Assign,
    Insert,
}

#[derive(Debug, PartialEq)]
pub enum Primitive {
    Int(isize),
    Bool(bool),
    String(Symbol),
}

#[derive(Debug, PartialEq)]
pub enum PrimitiveType {
    // TODO add `Char` type
    Int,
    Bool,
    String,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Error,

    Primitive(Primitive),
    Identifier(Symbol),

    // TODO: Figure out how to implement tuples and arrays in the type system.
    Tuple(Vec<(Symbol, Option<PrimitiveType>)>),
    Array(Vec<Spanned<Expression>>),

    Binary(HeapExpr, Operator, HeapExpr),
    Control(HeapExpr, Control, HeapExpr),

    TypeDef(Symbol, HeapExpr),
    VarDef(Symbol, HeapExpr),
}

pub type Spanned<T> = (T, logos::Span);
pub type HeapExpr = Box<Spanned<Expression>>;
pub type ExprError = Simple<TokenKind, logos::Span>;

pub fn parse(tokens: crate::lexer::Tokens) -> Result<Vec<HeapExpr>, Vec<ExprError>> {
    parse_aggregate().parse(tokens)
}

fn parse_aggregate() -> impl Parser<TokenKind, Vec<HeapExpr>, Error = ExprError> + Clone {
    parse_vardef()
        .or(parse_typedef())
        .or(parse_expr())
        .map(Box::new)
        .repeated()
        .then_ignore(chumsky::primitive::end())
}

fn parse_vardef() -> impl Parser<TokenKind, Spanned<Expression>, Error = ExprError> + Clone {
    just(TokenKind::VarDef)
        .ignore_then(parse_tld())
        .map(|(name, expr)| {
            let span = name.1.start..expr.1.end;
            (Expression::VarDef(name.0, Box::new(expr)), span)
        })
        .labelled("var define")
}

fn parse_typedef() -> impl Parser<TokenKind, Spanned<Expression>, Error = ExprError> + Clone {
    just(TokenKind::TypeDef)
        .ignore_then(parse_tld())
        .map(|(name, expr)| {
            let span = name.1.start..expr.1.end;
            (Expression::TypeDef(name.0, Box::new(expr)), span)
        })
        .labelled("type define")
}

fn parse_tld(
) -> impl Parser<TokenKind, (Spanned<Symbol>, Spanned<Expression>), Error = ExprError> + Clone {
    parse_symbol()
        .map_with_span(|expr, span| (expr, span))
        .clone()
        .then_ignore(just(TokenKind::Assign))
        .then(parse_expr())
        .then_ignore(just(TokenKind::Terminator))
        .labelled("top-level declaration")
}

fn parse_expr() -> impl Parser<TokenKind, Spanned<Expression>, Error = ExprError> + Clone {
    recursive(|expr| {
        let atom = parse_array()
            .or(parse_tuple())
            .or(parse_value())
            .or(parse_identifier())
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
            ))
            .labelled("atom");

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

        let op = select! { TokenKind::Insert => Control::Insert };
        let insert = eq
            .clone()
            .then(op.then(eq).repeated())
            .foldl(|a, (ctrl, b)| {
                let span = a.1.start..b.1.end;
                (Expression::Control(Box::new(a), ctrl, Box::new(b)), span)
            });

        // `assign` is the last parsed operator
        let op = select! { TokenKind::Assign => Control::Assign };
        let assign = insert
            .clone()
            .then(op.then(insert).repeated())
            .foldl(|a, (ctrl, b)| {
                let span = a.1.start..b.1.end;
                (Expression::Control(Box::new(a), ctrl, Box::new(b)), span)
            });

        assign
    })
    .labelled("expression")
}

fn parse_value() -> impl Parser<TokenKind, Expression, Error = ExprError> + Clone {
    select! {
        TokenKind::Integer(x) => Expression::Primitive(Primitive::Int(x)),
        TokenKind::Boolean(x) => Expression::Primitive(Primitive::Bool(x)),
        TokenKind::String(x) => Expression::Primitive(Primitive::String(x)),
    }
    .labelled("value")
}

fn parse_type() -> impl Parser<TokenKind, PrimitiveType, Error = ExprError> + Clone {
    select! {
        TokenKind::TypeInt => PrimitiveType::Int,
        TokenKind::TypeBool => PrimitiveType::Bool,
        TokenKind::TypeStr => PrimitiveType::String,
    }
    .labelled("type")
}

fn parse_symbol() -> impl Parser<TokenKind, Symbol, Error = ExprError> + Clone {
    select! { TokenKind::Symbol(name) => name }.labelled("symbol")
}

fn parse_identifier() -> impl Parser<TokenKind, Expression, Error = ExprError> + Clone {
    parse_symbol()
        .map(Expression::Identifier)
        .labelled("identifier")
}

fn parse_tuple() -> impl Parser<TokenKind, Expression, Error = ExprError> + Clone {
    parse_symbol()
        .then_ignore(just(TokenKind::Assign))
        .then(parse_type().or_not())
        .separated_by(just(TokenKind::Separator))
        .delimited_by(just(TokenKind::TupleOpen), just(TokenKind::TupleClose))
        .map(Expression::Tuple)
        .labelled("tuple")
}

fn parse_array() -> impl Parser<TokenKind, Expression, Error = ExprError> + Clone {
    parse_value()
        .clone()
        .map_with_span(|expr, span| (expr, span))
        .separated_by(just(TokenKind::Separator))
        .delimited_by(just(TokenKind::ArrayOpen), just(TokenKind::ArrayClose))
        .map(Expression::Array)
        .labelled("array")
}
