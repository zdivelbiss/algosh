use crate::{
    lexer::TokenKind,
    parser::expr::{Expression, Parser, ParserError},
    token,
};

pub struct Terminator;

impl Expression for Terminator {
    fn try_reduce(&mut self) -> Result<(), ParserError> {
        Ok(())
    }
}

impl TryFrom<&mut Parser<'_>> for Terminator {
    type Error = ParserError;

    fn try_from(parser: &mut Parser<'_>) -> Result<Self, Self::Error> {
        parser
            .expect(&token!(TokenKind::StatementEnd))
            .map(|_| Terminator)
    }
}
