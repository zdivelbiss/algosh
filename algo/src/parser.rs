use crate::{lexer::TokenKind, strings::Symbol, types::Type, Error, Operator, Primitive};
use chumsky::{
    primitive::{choice, end, just},
    recovery::nested_delimiters,
    recursive::recursive,
    select, BoxedParser, Parser,
};

#[derive(Debug, PartialEq)]
pub enum Expression {
    Error,

    Primitive(Primitive),
    Identifier(Symbol),

    Binary {
        lhs: HeapExpr,
        op: Operator,
        rhs: HeapExpr,
    },

    TypeDef {
        name: Symbol,
        ty: Type,
    },

    VarDef {
        name: Symbol,
        in_ty: Option<Type>,
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

impl Expression {
    pub const fn is_tld(&self) -> bool {
        matches!(
            self,
            Expression::TypeDef { name: _, ty: _ }
                | Expression::VarDef {
                    name: _,
                    in_ty: _,
                    expr: _
                }
        )
    }
}

type Spanned<T> = (T, crate::Span);
type AlgoParser<'a, T> = BoxedParser<'a, TokenKind, T, Error>;

pub type HeapExpr = Box<SpannedExpr>;
pub type SpannedExpr = Spanned<Expression>;

pub fn parse(tokens: crate::lexer::Tokens) -> Result<Vec<HeapExpr>, Vec<Error>> {
    parse_aggregate().parse(tokens)
}

fn parse_aggregate<'a>() -> AlgoParser<'a, Vec<HeapExpr>> {
    choice((parse_typedef(), parse_vardef(), parse_flow()))
        .map(Box::new)
        .repeated()
        .then_ignore(end())
        .boxed()
}

fn parse_vardef<'a>() -> AlgoParser<'a, SpannedExpr> {
    just(TokenKind::VarDef)
        .ignore_then(parse_symbol())
        .then_ignore(just(TokenKind::Assign))
        .then(
            parse_tuple_type()
                .then_ignore(just(TokenKind::Flow))
                .or_not(),
        )
        .then(parse_flow())
        .then_ignore(just(TokenKind::Terminator))
        .map_with_span(|((name, in_ty), expr), span| {
            (
                Expression::VarDef {
                    name,
                    in_ty,
                    expr: Box::new(expr),
                },
                span,
            )
        })
        .labelled("parse_vardef")
        .boxed()
}

fn parse_typedef<'a>() -> AlgoParser<'a, SpannedExpr> {
    just(TokenKind::TypeDef)
        .ignore_then(parse_symbol())
        .then_ignore(just(TokenKind::Assign))
        .then(parse_type())
        .then_ignore(just(TokenKind::Terminator))
        .map_with_span(|(name, ty), span| (Expression::TypeDef { name, ty }, span))
        .labelled("parse_typedef")
        .boxed()
}

fn parse_type() -> impl Parser<TokenKind, Type, Error = Error> {
    choice((
        parse_tuple_type(),
        parse_array_type(),
        parse_structural_type(),
    ))
}

fn parse_tuple_type<'a>() -> AlgoParser<'a, Type> {
    recursive(|expr| {
        parse_symbol()
            .then_ignore(just(TokenKind::Assign))
            .or_not()
            .then(choice((expr, parse_array_type(), parse_structural_type())))
            .separated_by(just(TokenKind::Separator))
            .at_least(1)
            .delimited_by(just(TokenKind::TupleOpen), just(TokenKind::TupleClose))
            .try_map(|components, span| {
                // Checking naming uniformity here instead of parsing sensitively is probably
                // technically slower, but this is much more readable.
                let is_valid_named = components.iter().all(|(n, _)| n.is_some());
                let is_valid_unnamed = components.iter().all(|(n, _)| n.is_none());

                if is_valid_named || is_valid_unnamed {
                    Ok(Type::Tuple(components))
                } else {
                    Err(Error::general(
                        span,
                        "tuples must either be fully named or fully unnamed",
                        None,
                    ))
                }
            })
    })
    .labelled("parse_tuple_type")
    .boxed()
}

fn parse_array_type<'a>() -> AlgoParser<'a, Type> {
    recursive(|expr| {
        let only_usize = choice((
            parse_uinteger(),
            parse_integer().try_map(|int, span| {
                usize::try_from(int)
                    .map_err(|_| Error::general(span, "array len cannot be negative", None))
            }),
        ))
        .boxed();

        choice((
            parse_structural_type(),
            parse_symbol().map(Type::Checked),
            expr,
        ))
        .then(just(TokenKind::Separator).ignore_then(only_usize).or_not())
        .delimited_by(just(TokenKind::ArrayOpen), just(TokenKind::ArrayClose))
        .map(|(base_ty, len)| Type::Array {
            ty: Box::new(base_ty),
            len,
        })
    })
    .labelled("parse_array_type")
    .boxed()
}

fn parse_structural_type() -> impl Parser<TokenKind, Type, Error = Error> {
    select! {
        TokenKind::TypeUnit => Type::Unit,
        TokenKind::TypeInt => Type::Int,
        TokenKind::TypeUInt => Type::UInt,
        TokenKind::TypeBool => Type::Bool,
    }
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
        .map_with_span(|prim, span| (Expression::Primitive(prim), span))
        .or(parse_expr())
        .separated_by(just(TokenKind::Terminator))
        .at_least(1)
        .map_with_span(|exprs, span| (Expression::Control { exprs }, span))
        .labelled("parse_control")
        .boxed()
}

#[allow(clippy::too_many_lines)]
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
                .foldl(|lhs, (op, rhs)| {
                    let span = lhs.1.start..rhs.1.end;
                    (
                        Expression::Binary {
                            lhs: Box::new(lhs),
                            op,
                            rhs: Box::new(rhs),
                        },
                        span,
                    )
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
    choice((
        parse_integer().map(Primitive::Int),
        parse_uinteger().map(Primitive::UInt),
        parse_bool().map(Primitive::Bool),
    ))
    .labelled("parse_primitive")
}

fn parse_integer() -> impl Parser<TokenKind, isize, Error = Error> + Clone {
    select! { TokenKind::Integer(x) => x }.labelled("parse_integer")
}

fn parse_uinteger() -> impl Parser<TokenKind, usize, Error = Error> + Clone {
    select! { TokenKind::UInteger(x) => x }.labelled("parse_uinteger")
}

fn parse_bool() -> impl Parser<TokenKind, bool, Error = Error> + Clone {
    select! { TokenKind::Boolean(x) => x }.labelled("parse_bool")
}

fn parse_symbol() -> impl Parser<TokenKind, Symbol, Error = Error> + Clone {
    select! { TokenKind::Symbol(name) => name }.labelled("parse_symbol")
}

fn parse_tuple<'a>() -> AlgoParser<'a, Primitive> {
    parse_symbol()
        .map(Some)
        .then(
            just(TokenKind::Assign)
                .ignore_then(parse_primitive())
                .or_not(),
        )
        .separated_by(just(TokenKind::Separator))
        .at_least(1)
        .delimited_by(just(TokenKind::TupleOpen), just(TokenKind::TupleClose))
        .map(Primitive::Tuple)
        .labelled("parse_tuple")
        .boxed()
}

fn parse_array<'a>() -> AlgoParser<'a, Primitive> {
    parse_primitive()
        .separated_by(just(TokenKind::Separator))
        .at_least(1)
        .delimited_by(just(TokenKind::ArrayOpen), just(TokenKind::ArrayClose))
        .map(Primitive::Array)
        .labelled("parse_array")
        .boxed()
}

#[cfg(test)]
mod tests {
    use crate::{intern, tests::parse_and_eq, Primitive};

    #[test]
    fn parse_tuple() {
        parse_and_eq(
            "{ a: 1, b: false }",
            super::parse_tuple(),
            &Primitive::Tuple(vec![
                (Some(intern!("a")), Some(Primitive::Int(1))),
                (Some(intern!("b")), Some(Primitive::Bool(false))),
            ]),
        )
    }
}
