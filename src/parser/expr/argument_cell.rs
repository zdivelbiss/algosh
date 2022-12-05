use std::borrow::BorrowMut;

use super::ExprErr;
use crate::{lexer::TokenKind, parser::Parser};

#[derive(Debug, PartialEq)]
pub enum PrimitiveIdentifier {
    Int,
    Bool,
    String,
}

impl TryFrom<&mut Parser<'_>> for PrimitiveIdentifier {
    type Error = ExprErr;

    fn try_from(parser: &mut Parser<'_>) -> Result<Self, Self::Error> {
        if let Some(true) = parser.next_kind_eq(&TokenKind::TypeInt) {
            Ok(Self::Int)
        } else if let Some(true) = parser.next_kind_eq(&TokenKind::TypeBool) {
            Ok(Self::Bool)
        } else if let Some(true) = parser.next_kind_eq(&TokenKind::TypeBool) {
            Ok(Self::String)
        } else {
            Err(())
        }
    }
}

// FIXME: Better name for this that isn't so confusing.
#[derive(Debug, PartialEq)]
pub enum TypeIdentifier {
    Primitive(PrimitiveIdentifier),
    // FIXME: This is pretty inefficient, having an allocation just to contain a complex type tree.
    Array(Box<Self>),
}

impl TryFrom<&mut Parser<'_>> for TypeIdentifier {
    type Error = ExprErr;

    fn try_from(parser: &mut Parser<'_>) -> Result<Self, Self::Error> {
        PrimitiveIdentifier::try_from(parser.borrow_mut())
            .map(|p| Self::Primitive(p))
            .or_else(|_| {
                if let Some(true) = parser.next_kind_eq(&TokenKind::ArrayOpen) {
                    let Ok(inner_type) = Self::try_from(parser.borrow_mut())
                        else { parser.throw("array expected type identifier") };
                    parser.expect_kind(&TokenKind::ArrayClose, "expected closing array bracket");

                    Ok(Self::Array(Box::new(inner_type)))
                } else {
                    Err(())
                }
            })
    }
}

#[derive(Debug, PartialEq)]
pub struct Argument {
    name: intaglio::Symbol,
    ty: TypeIdentifier,
}

impl TryFrom<&mut Parser<'_>> for Argument {
    type Error = ExprErr;

    fn try_from(parser: &mut Parser<'_>) -> Result<Self, Self::Error> {
        let (TokenKind::Identifier(name), _) = parser
            .next_kind_with(|kind| matches!(kind, TokenKind::Identifier(_)).then_some(()))
            .ok_or(())?
            else { panic!("token kind match unexpectdly not TokenKind::Identifier`") };

        parser
            .borrow_mut()
            .next_kind_eq(&TokenKind::Assign)
            .ok_or(())?;
        let Ok(ty) = TypeIdentifier::try_from(parser.borrow_mut())
            else { parser.throw("argument expected type identifier") };

        Ok(Self { name, ty })
    }
}

#[derive(Debug, PartialEq)]
pub struct ArgumentCell(Vec<Argument>);

impl TryFrom<&mut Parser<'_>> for ArgumentCell {
    type Error = ExprErr;

    fn try_from(parser: &mut Parser<'_>) -> Result<Self, Self::Error> {
        let mut arguments = Vec::new();
        while let Ok(argument) = Argument::try_from(parser.borrow_mut()) {
            arguments.push(argument);

            if let Some(true) = parser.next_kind_eq(&TokenKind::ArgumentCell) {
                break;
            } else {
                parser.expect_kind(
                    &TokenKind::Separator,
                    "argument cell expects argument separator `,`",
                );
            }
        }

        Ok(ArgumentCell(arguments))
    }
}
