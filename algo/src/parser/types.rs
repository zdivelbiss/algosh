use chumsky::{select, Parser};

use crate::{
    parser::exprs::{Expression, HeapExpr, SpannedExpr},
    types::Type,
    Error, Span,
};

pub type TypedExpr = (Type, Expression, Span);

pub fn parse(ast: Vec<HeapExpr>) -> () {}

fn parse_binary_ints() -> impl Parser<SpannedExpr, TypedExpr, Error = Error> {
    todo!()
}

fn parse_primitive() -> impl Parser<Expression, (Type, Expression), Error = Error> {
    select! { Expression::Primitive(prim) => (prim.ty(), prim) }
}
