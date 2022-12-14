use crate::{lexer::TokenKind, Error, Operator, Primitive, PrimitiveType, TupleComponent};
use chumsky::{
    primitive::{choice, just},
    recovery::nested_delimiters,
    recursive::recursive,
    select, BoxedParser, Parser,
};
use intaglio::Symbol;

#[derive(Debug, PartialEq)]
pub enum Expression {
    Error,

    Primitive(Primitive),
    Identifier(Symbol),

    // TODO: Figure out how to implement tuples and arrays in the type system.
    Tuple(Vec<(Symbol, TupleComponent)>),
    Array(Vec<Spanned<Expression>>),

    Binary(HeapExpr, Operator, HeapExpr),

    TypeDef(Symbol, HeapExpr),
    VarDef(Symbol, HeapExpr),
}

type Spanned<T> = (T, crate::Span);
type AlgoParser<'a, T> = BoxedParser<'a, TokenKind, T, Error>;
pub type HeapExpr = Box<Spanned<Expression>>;

pub fn parse(tokens: crate::lexer::Tokens) -> Result<Vec<HeapExpr>, Vec<Error>> {
    parse_aggregate().parse(tokens)
}

fn parse_aggregate<'a>() -> BoxedParser<'a, TokenKind, Vec<HeapExpr>, Error> {
    choice((parse_vardef(), parse_typedef(), parse_expr()))
        .map(Box::new)
        .repeated()
        .then_ignore(chumsky::primitive::end())
        .boxed()
}

fn parse_vardef() -> impl Parser<TokenKind, Spanned<Expression>, Error = Error> + Clone {
    just(TokenKind::VarDef)
        .ignore_then(parse_tld())
        .map(|(name, expr)| {
            let span = name.1.start..expr.1.end;
            (Expression::VarDef(name.0, Box::new(expr)), span)
        })
        .labelled("var define")
}

fn parse_typedef() -> impl Parser<TokenKind, Spanned<Expression>, Error = Error> + Clone {
    just(TokenKind::TypeDef)
        .ignore_then(parse_tld())
        .map(|(name, expr)| {
            let span = name.1.start..expr.1.end;
            (Expression::TypeDef(name.0, Box::new(expr)), span)
        })
        .labelled("type define")
}

fn parse_tld<'a>() -> BoxedParser<'a, TokenKind, (Spanned<Symbol>, Spanned<Expression>), Error> {
    parse_symbol()
        .map_with_span(|expr, span| (expr, span))
        .then_ignore(just(TokenKind::Assign))
        .then(parse_expr())
        .then_ignore(just(TokenKind::Terminator))
        .labelled("top-level declaration")
        .boxed()
}

fn parse_expr<'a>() -> BoxedParser<'a, TokenKind, Spanned<Expression>, Error> {
    recursive(|expr| {
        let atom = choice((
            parse_array(),
            parse_tuple(),
            parse_primitive().map(Expression::Primitive),
            parse_symbol().map(Expression::Identifier),
        ))
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
        .labelled("atom")
        .boxed();

        fn parse_op<'a>(
            op_parser: impl 'a + Parser<TokenKind, Operator, Error = Error> + Clone,
            base_parser: AlgoParser<'a, Spanned<Expression>>,
        ) -> AlgoParser<'a, Spanned<Expression>> {
            base_parser
                .clone()
                .then(op_parser.then(base_parser).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.1.start..b.1.end;
                    (Expression::Binary(Box::new(a), op, Box::new(b)), span)
                })
                .boxed()
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
    .boxed()
}

fn parse_primitive() -> impl Parser<TokenKind, Primitive, Error = Error> + Clone {
    select! {
        TokenKind::Integer(x) => Primitive::Int(x),
        TokenKind::Boolean(x) => Primitive::Bool(x),
        TokenKind::String(x) => Primitive::String(x),
    }
    .labelled("value")
}

fn parse_type() -> impl Parser<TokenKind, PrimitiveType, Error = Error> + Clone {
    select! {
        TokenKind::TypeInt => PrimitiveType::Int,
        TokenKind::TypeBool => PrimitiveType::Bool,
        TokenKind::TypeStr => PrimitiveType::String,
    }
    .labelled("type")
}

fn parse_symbol() -> impl Parser<TokenKind, Symbol, Error = Error> + Clone {
    select! { TokenKind::Symbol(name) => name }.labelled("symbol")
}

fn parse_tuple<'a>() -> BoxedParser<'a, TokenKind, Expression, Error> {
    parse_symbol()
        .then_ignore(just(TokenKind::Assign))
        .then(choice((
            parse_type().map(TupleComponent::Typed),
            parse_primitive().map(TupleComponent::Valued),
        )))
        .separated_by(just(TokenKind::Separator))
        .delimited_by(just(TokenKind::TupleOpen), just(TokenKind::TupleClose))
        .map(Expression::Tuple)
        .labelled("tuple")
        .boxed()
}

fn parse_array<'a>() -> BoxedParser<'a, TokenKind, Expression, Error> {
    parse_primitive()
        .map(Expression::Primitive)
        .map_with_span(|expr, span| (expr, span))
        .separated_by(just(TokenKind::Separator))
        .delimited_by(just(TokenKind::ArrayOpen), just(TokenKind::ArrayClose))
        .map(Expression::Array)
        .labelled("array")
        .boxed()
}
