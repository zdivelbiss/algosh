use intaglio::Symbol;

use crate::{
    lexer::{Token, TokenKind},
    parser::expr::{Expression, HeapExpr, Parser, ParserError, TypeKind},
    token,
};

#[derive(Debug)]
pub struct Transform {
    parameters: Vec<(Symbol, TypeKind)>,
    next_expr: HeapExpr,
}

impl Expression for Transform {
    fn try_reduce(&mut self) -> Result<(), ParserError> {
        todo!()
    }
}

impl TryFrom<&mut Parser<'_>> for Transform {
    type Error = ParserError;

    fn try_from(parser: &mut Parser<'_>) -> Result<Self, Self::Error> {
        parser.expect(&token!(TokenKind::ParameterBrace))?;

        let mut parameters = Vec::new();
        loop {
            let name = match parser.peek().map(Token::kind) {
                Some(TokenKind::Identifier(name)) => *name,
                _ => {
                    return Err(ParserError::FoundMsg {
                        found: parser.peek().cloned(),
                        msg: String::from(
                            "expected identifier (hint: parameter format is `name: Int`)",
                        ),
                    })
                }
            };

            parser.expect(&token!(TokenKind::Assign))?;
            parameters.push((
                name,
                parser.expect_with(|t| {
                    TypeKind::try_from(t.kind()).map_err(|_| ParserError::FoundMsg {
                        found: Some(t.clone()),
                        msg: String::from("expected type (hint: parameter format is `name: Type`)"),
                    })
                })?,
            ));

            match parser.peek().map(crate::lexer::Token::kind) {
                Some(&TokenKind::ParameterBrace) => {
                    parser.advance();
                    break;
                }

                Some(&TokenKind::Separator) => {
                    parser.advance();
                    continue;
                }

                _ => return Err(ParserError::ReplaceThisError),
            }
        }

        Ok(Self {
            parameters,
            next_expr: super::parse_expr(parser)?,
        })
    }
}
