use intaglio::Symbol;

use crate::{
    lexer::{Token, TokenKind},
    parser::expr::{Expression, HeapExpr, Parser, ParserError},
};

#[derive(Debug, PartialEq)]
pub enum ValueKind {
    Int(isize),
    Bool(bool),
    String(Symbol),
    Array(Vec<Box<Self>>),
    Tuple(Vec<Box<Self>>),
}

impl TryFrom<&TokenKind> for ValueKind {
    type Error = ParserError;

    fn try_from(kind: &TokenKind) -> Result<Self, Self::Error> {
        match kind {
            TokenKind::Integer(int) => Ok(Self::Int(*int)),
            TokenKind::Boolean(bool) => Ok(Self::Bool(*bool)),
            TokenKind::String(string) => Ok(Self::String(*string)),

            _ => Err(ParserError::UnexpectedToken),
        }
    }
}

pub struct Value {
    kind: ValueKind,
    next_expr: HeapExpr,
}

impl Expression for Value {
    fn try_reduce(&mut self) -> Result<(), ParserError> {
        todo!()
    }
}

impl TryFrom<&mut Parser<'_>> for Value {
    type Error = ParserError;

    fn try_from(parser: &mut Parser<'_>) -> Result<Self, Self::Error> {
        let kind = parser
            .peek()
            .map(Token::kind)
            .ok_or(ParserError::NoMoreTokens)?;

        if let Ok(value_kind) = ValueKind::try_from(kind) {
            Ok(Self {
                kind: value_kind,
                next_expr: super::parse_expr(parser)?,
            })
        } else {
            Err(ParserError::UnexpectedToken)
        }
    }
}
