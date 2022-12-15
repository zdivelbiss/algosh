use crate::{lexer::TokenKind, types::Type, Error, Operator, Primitive};
use chumsky::{
    primitive::{choice, end, just},
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
    Tuple(Vec<(Symbol, Option<Primitive>)>),
    Array(Vec<SpannedExpr>),

    Binary(HeapExpr, Operator, HeapExpr),

    VarDef {
        name: Symbol,
        expr: HeapExpr,
    },

    Control {
        exprs: Vec<SpannedExpr>,
    },

    Flow {
        from: HeapExpr,
        to: Option<HeapExpr>,
    },
}

type Spanned<T> = (T, crate::Span);
type AlgoParser<'a, T> = BoxedParser<'a, TokenKind, T, Error>;

pub type HeapExpr = Box<SpannedExpr>;
pub type SpannedExpr = Spanned<Expression>;

pub fn parse(tokens: crate::lexer::Tokens) -> Result<Vec<HeapExpr>, Vec<Error>> {
    parse_aggregate().parse(tokens)
}

fn parse_aggregate<'a>() -> AlgoParser<'a, Vec<HeapExpr>> {
    choice((parse_vardef(), parse_flow()))
        .map(Box::new)
        .repeated()
        .then_ignore(end())
        .boxed()
}

fn parse_vardef() -> impl Parser<TokenKind, SpannedExpr, Error = Error> + Clone {
    just(TokenKind::VarDef)
        .ignore_then(parse_symbol())
        .then_ignore(just(TokenKind::Assign))
        .then(parse_flow())
        .then_ignore(choice((
            just(TokenKind::Terminator).ignored(),
            end().ignored(),
        )))
        .map_with_span(|(name, expr), span| {
            (
                Expression::VarDef {
                    name,
                    expr: Box::new(expr),
                },
                span,
            )
        })
        .labelled("parse_vardef")
}

// fn parse_type<'a>() -> AlgoParser<'a, Type> {
//     recursive(|expr| choice(parsers))
// }

fn parse_array_type<'a>() -> AlgoParser<'a, Type> {
    let only_usize = choice((
        parse_uinteger(),
        parse_integer().try_map(|int, span| {
            usize::try_from(int)
                .map_err(|_| Error::general(span, "array len cannot be negative", None))
        }),
    ));

    recursive(|expr| {
        choice((
            parse_structural_type(),
            parse_symbol().map(Type::Named),
            expr,
        ))
        .then(just(TokenKind::Separator).ignore_then(only_usize).or_not())
        .delimited_by(just(TokenKind::ArrayOpen), just(TokenKind::ArrayClose))
        .map(|(base_ty, len)| Type::Array { ty: base_ty, len })
    })
}

fn parse_structural_type<'a>() -> impl Parser<TokenKind, Type, Error = Error> {
    select! {
        TokenKind::TypeUnit => Type::Unit,
        TokenKind::TypeInt => Type::Int,
        TokenKind::TypeUInt => Type::UInt,
        TokenKind::TypeBool => Type::Bool,
    }
}

#[test]
fn parse_type_test<'a>() {
    static SIMPLE_ARRAY: &str = "[Int]";
    static SIMPLE_ARRAY_LEN: &str = "[Int, 64]";
    static FULL_TYPE_STR: &str = "{ { a: Int, Int, [UInt, 64] }, Int, Bool }";

    parse_array_type()
        .parse_recovery_verbose(SIMPLE_ARRAY)
        .unwrap();
}

fn parse_flow<'a>() -> AlgoParser<'a, SpannedExpr> {
    recursive(|expr| {
        parse_control()
            .then(just(TokenKind::Flow).ignore_then(expr).or_not())
            .map_with_span(|(expr, next), span| {
                (
                    Expression::Flow {
                        from: Box::new(expr),
                        to: next.map(Box::new),
                    },
                    span,
                )
            })
    })
    .labelled("parse_flow")
    .boxed()
}

fn parse_control<'a>() -> AlgoParser<'a, SpannedExpr> {
    choice((parse_array(), parse_tuple()))
        .map_with_span(|expr, span| (expr, span))
        .or(parse_expr())
        .separated_by(just(TokenKind::Terminator))
        .at_least(1)
        .map_with_span(|exprs, span| (Expression::Control { exprs }, span))
        .labelled("parse_control")
        .boxed()
}

fn parse_expr<'a>() -> AlgoParser<'a, SpannedExpr> {
    recursive(|expr| {
        let atom = choice((
            parse_primitive().map(Expression::Primitive),
            parse_symbol().map(Expression::Identifier),
        ))
        .map_with_span(|expr, span| (expr, span))
        .or(expr.delimited_by(just(TokenKind::GroupOpen), just(TokenKind::GroupClose)))
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
        .labelled("parse_atom")
        .boxed();

        fn parse_op<'a>(
            op_parser: impl 'a + Parser<TokenKind, Operator, Error = Error> + Clone,
            base_parser: AlgoParser<'a, SpannedExpr>,
        ) -> AlgoParser<'a, SpannedExpr> {
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
    .labelled("parse_expr")
    .boxed()
}

fn parse_primitive() -> impl Parser<TokenKind, Primitive, Error = Error> + Clone {
    choice((parse_integer(), parse_uinteger(), parse_bool())).labelled("parse_primitive")
}

fn parse_integer() -> impl Parser<TokenKind, isize, Error = Error> + Clone {
    select! { TokenKind::Integer(x) => x }.labelled("parse_integer")
}

fn parse_uinteger() -> impl Parser<TokenKind, usize, Error = Error> + Clone {
    select! { TokenKind::UInteger(x) => x }.labelled("parse_uinteger")
}

fn parse_bool() -> impl Parser<TokenKind, bool, Error = Error> + Clone {
    select! { TokenKind::Bool(x) => x }.labelled("parse_bool")
}

// fn parse_type() -> impl Parser<TokenKind, Type, Error = Error> + Clone {
//     select! {
//         TokenKind::TypeUnit => Type::Unit,
//         TokenKind::TypeInt => Type::Int,
//         TokenKind::TypeUInt => Type::UInt,
//         TokenKind::TypeBool => Type::Bool,
//     }
//     .labelled("parse_type")
// }

fn parse_symbol() -> impl Parser<TokenKind, Symbol, Error = Error> + Clone {
    select! { TokenKind::Symbol(name) => name }.labelled("parse_symbol")
}

fn parse_tuple<'a>() -> AlgoParser<'a, Expression> {
    parse_symbol()
        .then(
            just(TokenKind::Assign)
                .ignore_then(parse_primitive())
                .or_not(),
        )
        .separated_by(just(TokenKind::Separator))
        .delimited_by(just(TokenKind::TupleOpen), just(TokenKind::TupleClose))
        .map(Expression::Tuple)
        .labelled("parse_tuple")
        .boxed()
}

fn parse_array<'a>() -> AlgoParser<'a, Expression> {
    parse_primitive()
        .map(Expression::Primitive)
        .map_with_span(|expr, span| (expr, span))
        .separated_by(just(TokenKind::Separator))
        .delimited_by(just(TokenKind::ArrayOpen), just(TokenKind::ArrayClose))
        .map(Expression::Array)
        .labelled("parse_array")
        .boxed()
}
