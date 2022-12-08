use crate::{
    lexer::{Token, TokenKind},
    parser::expr::{Expression, HeapExpr, Parser, ParserError, TypeKind},
    token,
};
use intaglio::Symbol;

#[derive(Debug, PartialEq)]
pub enum OperatorKind {
    Eq,
    NegEq,
    Add,
    Sub,
    Mul,
    Div,
    Shr,
    Shl,
    Assign,
}

impl TryFrom<&TokenKind> for OperatorKind {
    type Error = ParserError;

    fn try_from(kind: &TokenKind) -> Result<Self, Self::Error> {
        match kind {
            TokenKind::Add => Ok(Self::Add),
            TokenKind::Sub => Ok(Self::Sub),
            TokenKind::Mul => Ok(Self::Mul),
            TokenKind::Div => Ok(Self::Div),
            TokenKind::Shr => Ok(Self::Shr),
            TokenKind::Shl => Ok(Self::Shl),
            TokenKind::Eq => Ok(Self::Eq),
            TokenKind::NegEq => Ok(Self::NegEq),
            TokenKind::Assign => Ok(Self::Assign),

            _ => Err(ParserError::UnexpectedToken),
        }
    }
}

#[derive(Debug)]
pub struct Binary {
    kind: OperatorKind,
    next_expr: HeapExpr,
}

impl Expression for Binary {
    fn try_reduce(&mut self) -> Result<(), ParserError> {
        todo!()
    }
}

impl TryFrom<&mut Parser<'_>> for Binary {
    type Error = ParserError;

    fn try_from(parser: &mut Parser<'_>) -> Result<Self, Self::Error> {
        let Some(operator_kind) = parser.peek().and_then(|t|  OperatorKind::try_from(t.kind()).ok())
        else {
            return Err(ParserError::FoundMsg {
                found: parser.peek().cloned(),
                msg: String::from("binary expression exptects operator")
            });
        };

        Ok(Self {
            kind: operator_kind,
            next_expr: super::parse_expr(parser)?,
        })
    }
}
