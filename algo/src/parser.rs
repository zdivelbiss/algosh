use crate::{lexer::TokenKind, strings::Symbol, types::Type, Error, Operator, Span};
use chumsky::{
    primitive::{choice, end, just},
    recovery::nested_delimiters,
    recursive::recursive,
    select, BoxedParser, Parser,
};

#[derive(Debug, PartialEq)]
pub enum ExpressionKind {
    Error,

    Unit,
    Int(isize),
    UInt(usize),
    Bool(bool),
    Array(Vec<Expression>),
    Tuple(Vec<Expression>),

    TypeId(Symbol),
    Identifier(Symbol),

    Binary {
        lhs: Box<Expression>,
        op: Operator,
        rhs: Box<Expression>,
    },

    Compound(Vec<Expression>),

    ControlFlow {
        from: Box<Expression>,
        into: Option<Box<Expression>>,
    },

    VarDef {
        name: Symbol,
        ty: Type,
        expr: Box<Expression>,
    },

    TypeDef {
        name: Symbol,
        ty: Type,
    },
}

#[derive(Debug, PartialEq)]
pub struct Expression {
    kind: ExpressionKind,
    span: Span,
}

impl Expression {
    #[inline]
    pub const fn kind(&self) -> &ExpressionKind {
        &self.kind
    }

    #[inline]
    pub const fn span(&self) -> &Span {
        &self.span
    }
}

macro_rules! expr {
    ($kind:expr, $span:expr) => {
        Expression {
            kind: $kind,
            span: $span,
        }
    };
}

pub fn parse(tokens: crate::lexer::Tokens) -> Result<Vec<Expression>, Vec<Error>> {
    parse_aggregate().parse(tokens)
}

type AlgoParser<'a, T> = BoxedParser<'a, TokenKind, T, Error>;

fn parse_aggregate<'a>() -> AlgoParser<'a, Vec<Expression>> {
    choice((parse_vardef(), parse_control_flow()))
        .repeated()
        .then_ignore(end())
        .boxed()
}

fn parse_vardef<'a>() -> AlgoParser<'a, Expression> {
    let unit = select! { TokenKind::TypeUnit => Type::Unit };
    let body = choice((parse_tuple_type(), unit))
        .then_ignore(just(TokenKind::Flow))
        .then(parse_control_flow())
        .boxed();

    let body_terminated = body.clone().then_ignore(just(TokenKind::Terminator));
    let body_delimited = body.delimited_by(just(TokenKind::BlockOpen), just(TokenKind::BlockClose));

    just(TokenKind::VarDef)
        .ignore_then(parse_symbol())
        .then_ignore(just(TokenKind::Assign))
        .then(choice((body_terminated, body_delimited)))
        .map_with_span(|(name, (ty, expr)), span| {
            let kind = ExpressionKind::VarDef {
                name,
                ty,
                expr: Box::new(expr),
            };

            expr!(kind, span)
        })
        .labelled("parse_vardef")
        .boxed()
}

fn parse_typedef<'a>() -> AlgoParser<'a, Expression> {
    just(TokenKind::TypeDef)
        .ignore_then(parse_symbol())
        .then_ignore(just(TokenKind::Assign))
        .then(parse_type())
        .then_ignore(just(TokenKind::Terminator))
        .map_with_span(|(name, ty), span| expr!(ExpressionKind::TypeDef { name, ty }, span))
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
            .then(choice((expr, parse_array_type(), parse_structural_type())))
            .separated_by(just(TokenKind::Separator))
            .at_least(1)
            .delimited_by(just(TokenKind::GroupOpen), just(TokenKind::GroupClose))
            .map(Type::Tuple)
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

fn parse_control_flow<'a>() -> AlgoParser<'a, Expression> {
    recursive(|next| {
        let expr = choice((parse_tuple(), parse_array())).or(parse_expr());
        let compound_expr = expr
            .clone()
            .separated_by(just(TokenKind::Terminator))
            .at_least(2)
            .map_with_span(|exprs, span| expr!(ExpressionKind::Compound(exprs), span));
        let expr = compound_expr.or(expr);

        let expr_block = expr
            .clone()
            .delimited_by(just(TokenKind::BlockOpen), just(TokenKind::BlockClose))
            .boxed();
        let expr = expr_block.or(expr);

        expr.then(just(TokenKind::Flow).ignore_then(next).or_not())
            .map_with_span(|(from, into), span| {
                expr!(
                    ExpressionKind::ControlFlow {
                        from: Box::new(from),
                        into: into.map(Box::new),
                    },
                    span
                )
            })
            .boxed()
    })
    .labelled("parse_control_flow")
    .boxed()
}

#[allow(clippy::too_many_lines)]
fn parse_expr<'a>() -> AlgoParser<'a, Expression> {
    recursive(|expr| {
        let atom = choice((
            parse_integer().map_with_span(|int, span| expr!(ExpressionKind::Int(int), span)),
            parse_uinteger().map_with_span(|uint, span| expr!(ExpressionKind::UInt(uint), span)),
            parse_bool().map_with_span(|bool, span| expr!(ExpressionKind::Bool(bool), span)),
            parse_symbol()
                .map_with_span(|symbol, span| expr!(ExpressionKind::Identifier(symbol), span)),
        ))
        .or(expr.delimited_by(just(TokenKind::GroupOpen), just(TokenKind::GroupClose)))
        .recover_with(nested_delimiters(
            TokenKind::GroupOpen,
            TokenKind::GroupClose,
            [
                (TokenKind::ArrayOpen, TokenKind::ArrayClose),
                (TokenKind::BlockOpen, TokenKind::BlockClose),
            ],
            |span| expr!(ExpressionKind::Error, span),
        ))
        .recover_with(nested_delimiters(
            TokenKind::ArrayOpen,
            TokenKind::ArrayClose,
            [
                (TokenKind::GroupOpen, TokenKind::GroupClose),
                (TokenKind::BlockOpen, TokenKind::BlockClose),
            ],
            |span| expr!(ExpressionKind::Error, span),
        ))
        .recover_with(nested_delimiters(
            TokenKind::BlockOpen,
            TokenKind::BlockClose,
            [
                (TokenKind::GroupOpen, TokenKind::GroupClose),
                (TokenKind::ArrayOpen, TokenKind::ArrayClose),
            ],
            |span| expr!(ExpressionKind::Error, span),
        ))
        .labelled("parse_atom")
        .boxed();

        fn parse_op<'a>(
            op_parser: impl 'a + Parser<TokenKind, Operator, Error = Error> + Clone,
            base_parser: AlgoParser<'a, Expression>,
        ) -> AlgoParser<'a, Expression> {
            base_parser
                .clone()
                .then(op_parser.then(base_parser).repeated())
                .foldl(|lhs, (op, rhs)| {
                    let span = lhs.span.start..rhs.span.end;
                    let kind = ExpressionKind::Binary {
                        lhs: Box::new(lhs),
                        op,
                        rhs: Box::new(rhs),
                    };

                    expr!(kind, span)
                })
                .boxed()
        }

        let assign = parse_op(select! { TokenKind::Assign => Operator::Assign }, atom);
        let conditional_flow = parse_op(
            select! {
                TokenKind::Clow => Operator::Clow,
                TokenKind::Cerm => Operator::Cerm,
            },
            assign,
        );
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
            conditional_flow,
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

fn parse_tuple<'a>() -> AlgoParser<'a, Expression> {
    parse_expr()
        .separated_by(just(TokenKind::Separator))
        .at_least(1)
        .delimited_by(just(TokenKind::GroupOpen), just(TokenKind::GroupClose))
        .map_with_span(|expr, span| expr!(ExpressionKind::Tuple(expr), span))
        .labelled("parse_tuple")
        .boxed()
}

fn parse_array<'a>() -> AlgoParser<'a, Expression> {
    parse_expr()
        .separated_by(just(TokenKind::Separator))
        .at_least(1)
        .delimited_by(just(TokenKind::ArrayOpen), just(TokenKind::ArrayClose))
        .map_with_span(|expr, span| expr!(ExpressionKind::Array(expr), span))
        .labelled("parse_array")
        .boxed()
}

fn parse_integer() -> impl Parser<TokenKind, isize, Error = Error> {
    select! { TokenKind::Integer(x) => x }.labelled("parse_integer")
}

fn parse_uinteger() -> impl Parser<TokenKind, usize, Error = Error> {
    select! { TokenKind::UInteger(x) => x }.labelled("parse_uinteger")
}

fn parse_bool() -> impl Parser<TokenKind, bool, Error = Error> {
    select! { TokenKind::Boolean(x) => x }.labelled("parse_bool")
}

fn parse_symbol() -> impl Parser<TokenKind, Symbol, Error = Error> {
    select! { TokenKind::Symbol(name) => name }.labelled("parse_symbol")
}

#[cfg(test)]
mod tests {
    // use crate::{interned, tests::parse_and_eq, Operator};

    // use super::Expression;

    // #[test]
    // fn conditional() {
    //     parse_and_eq(
    //         "(false ?> 1) ?? 3",
    //         super::parse_expr(),
    //         &(
    //             Expression::Binary {
    //                 lhs: Box::new((
    //                     Expression::Binary {
    //                         lhs: Box::new((Expression::Bool(false), 1..6)),
    //                         op: Operator::Clow,
    //                         rhs: Box::new((Expression::Int(1), 10..11)),
    //                     },
    //                     1..11,
    //                 )),
    //                 op: Operator::Cerm,
    //                 rhs: Box::new((Expression::Int(3), 16..17)),
    //             },
    //             1..17,
    //         ),
    //     );
    // }

    // #[test]
    // fn named_tuple() {
    //     parse_and_eq(
    //         "(a: 1, b: false)",
    //         super::parse_tuple(),
    //         &super::Expression::Tuple(vec![
    //             (
    //                 Expression::Binary {
    //                     lhs: Box::new((Expression::Identifier(interned!("a")), 1..2)),
    //                     op: Operator::Assign,
    //                     rhs: Box::new((Expression::Int(1), 4..5)),
    //                 },
    //                 1..5,
    //             ),
    //             (
    //                 Expression::Binary {
    //                     lhs: Box::new((Expression::Identifier(interned!("b")), 7..8)),
    //                     op: Operator::Assign,
    //                     rhs: Box::new((Expression::Bool(false), 10..15)),
    //                 },
    //                 7..15,
    //             ),
    //         ]),
    //     )
    // }
}
