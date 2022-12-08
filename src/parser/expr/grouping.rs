use crate::{
    lexer::TokenKind,
    parser::expr::{Expression, HeapExpr, Parser, ParserError},
    token,
};

pub struct Grouping {
    group: HeapExpr,
    next_expr: HeapExpr,
}

impl Expression for Grouping {
    type Error = ParserError;

    fn try_reduce(&mut self) -> Result<(), Self::Error> {
        todo!()
    }
}

impl TryFrom<&mut Parser<'_>> for Grouping {
    type Error = ParserError;

    fn try_from(parser: &mut Parser<'_>) -> Result<Self, Self::Error> {
        parser.expect(&token!(TokenKind::GroupingOpen));
        let inner_expr = Self::try_from(parser)?;
        parser.expect(&token!(TokenKind::GroupingClose))?;

        Ok(Self {
            group: Box::new(inner_expr),
            next_expr: Box::new(Self::try_from(parser)?),
        })
    }
}

pub struct GroupingEnd;

impl Expression for GroupingEnd {
    type Error = ParserError;
    fn try_reduce(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
}

impl TryFrom<&mut Parser<'_>> for GroupingEnd {
    type Error = ParserError;

    fn try_from(parser: &mut Parser<'_>) -> Result<Self, Self::Error> {
        parser.expect(&token!(TokenKind::GroupingClose))?;

        Ok(Self)
    }
}
