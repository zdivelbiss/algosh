use crate::{lexer::TokenKind,  Operator, Primitive, PrimitiveType};
use chumsky::{
    prelude::Simple, primitive::just, recovery::nested_delimiters, recursive::recursive, select,
    Parser,
};
use intaglio::Symbol;

#[derive(Debug, PartialEq)]
pub enum Expression {
    Error,

    Primitive(Primitive),
    Identifier(Symbol),

    // TODO: Figure out how to implement tuples and arrays in the type system.
    Tuple(Vec<(Symbol, Option<PrimitiveType>)>),
    Array(Vec<Spanned<Expression>>),

    Binary(HeapExpr, Operator, HeapExpr),

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

        fn parse_op(
            op_parser: impl Parser<TokenKind, Operator, Error = ExprError> + Clone,
            base_parser: impl Parser<TokenKind, Spanned<Expression>, Error = ExprError> + Clone,
        ) -> impl Parser<TokenKind, Spanned<Expression>, Error = ExprError> + Clone {
            base_parser
                .clone()
                .then(op_parser.then(base_parser).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.1.start..b.1.end;
                    (Expression::Binary(Box::new(a), op, Box::new(b)), span)
                })
        }

        let assign = parse_op(select! { TokenKind::Assign => Operator::Assign }, atom);
        let flow = parse_op(select! { TokenKind::Flow => Operator::Flow }, assign);
        let condition = parse_op(
            select! {
                TokenKind::Eq => Operator::Eq,
                TokenKind::NotEq => Operator::NotEq,
                TokenKind::Greater => Operator::Greater,
                TokenKind::GreaterEq => Operator::GreaterEq,
                TokenKind::Less => Operator::Less,
                TokenKind::LessEq => Operator::LessEq,
                TokenKind::Or => Operator::Or,
                TokenKind::Xor => Operator::Xor,
                TokenKind::And => Operator::And,
            },
            flow,
        );
        let exponent = parse_op(select! { TokenKind::Exp => Operator::Exp }, condition);
        let mul_div_rem = parse_op(
            select! { TokenKind::Mul => Operator::Mul,
            TokenKind::Div => Operator::Div,
            TokenKind::Rem => Operator::Rem },
            exponent,
        );
        let add_sub = parse_op(
            select! { TokenKind::Add => Operator::Add, TokenKind::Sub => Operator::Sub},
            mul_div_rem,
        );
        let bitshift = parse_op(
            select! { TokenKind::Shr => Operator::Shr, TokenKind::Shl => Operator::Shl},
            add_sub,
        );
        let bitand = parse_op(select! { TokenKind::BitAnd => Operator::BitAnd }, bitshift);
        let bitxor = parse_op(select! { TokenKind::BitXor => Operator::BitXor}, bitand);
        let bitor = parse_op(select! { TokenKind::BitOr => Operator::BitOr }, bitxor);
        let comparison = parse_op(
            select! {
                TokenKind::Eq => Operator::Eq,
                TokenKind::NotEq => Operator::NotEq,
                TokenKind::Greater => Operator::Greater,
                TokenKind::GreaterEq => Operator::GreaterEq,
                TokenKind::Less => Operator::Less,
                TokenKind::LessEq => Operator::LessEq,
            },
            bitor,
        );
        let logical_and = parse_op(
            select! {
                TokenKind::And => Operator::And
            },
            comparison,
        );
        let logical_xor = parse_op(select! { TokenKind::Xor => Operator::Xor}, logical_and);
        parse_op(select! { TokenKind::Or => Operator::Or}, logical_xor)
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
        .map_with_span(|expr, span| (expr, span))
        .separated_by(just(TokenKind::Separator))
        .delimited_by(just(TokenKind::ArrayOpen), just(TokenKind::ArrayClose))
        .map(Expression::Array)
        .labelled("array")
}
