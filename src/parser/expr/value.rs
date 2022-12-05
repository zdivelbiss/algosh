use intaglio::Symbol;

use crate::{lexer::TokenKind, parser::{Parser, ParserError,HeapExpr, Expression}}

#[derive(Debug, PartialEq)]
pub enum ValueKind {
    Integer(isize),
    String(Symbol),
    Boolean(bool),
    Array(Vec<Box<Self>>),
    // TODO tuple variable
}



impl TryFrom<&mut Parser<'_>> for ValueKind {
    type Error =  ParserError;

    fn try_from(parser: &mut Parser<'_>) -> Result<Self, Self::Error> {
        parser.next_kind_with(|kind| match kind {
            &TokenKind::Integer(int) => Ok(Self::Integer(*int)),
            &TokenKind::Boolean(bool) => Ok(Self::Boolean(*bool))
        })

        match kind {
            TokenKind::Integer(int) => Ok(Self::Integer(*int)),
            TokenKind::String(string) => Ok(Self::String(string.clone())),
            TokenKind::Boolean(boolean) => Ok(Self::Boolean(*boolean)),

            _ => Err(ParserError::ExpectedToken),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Value {
    kind: ValueKind,
}

 impl TryFrom<&mut Parser<'_>> for Value {
     type Error = ParserError;

     fn try_from(parser: &mut Parser) -> Result<Self, Self::Error> {
        let a = parser.next_kind_with(|kind| {
            match kind {
                TokenKind::Integer(int) => Ok(Self::Integer(*int)),
                TokenKind::String(string) => Ok(Self::String(string.clone())),
                TokenKind::Boolean(boolean) => Ok(Self::Boolean(*boolean)),
   
                _ => Err(ParserError::ExpectedToken),
            }
        });

        loop {}
        


     }
 }
