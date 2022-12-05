use crate::{
    lexer::TokenKind,
    parser::{Expression, HeapExpr, Parser, ParserError},
};



struct Operator {
    kind: OperatorKind,
    next_expr: HeapExpr,
}

impl Expression for Operator {
    fn next_expr(&self) -> Option<&HeapExpr> {
        Some(&self.next_expr)
    }
}

impl TryFrom<&mut Parser<'_>> for Operator {
    type Error = ParserError;

    fn try_from(parser: &mut Parser) -> Result<Self, Self::Error> {
        let kind = {
            if parser.next_kind_eq(&TokenKind::Add).contains(&true) {
                OperatorKind::Add
            } else if parser.next_kind_eq(&TokenKind::Sub).contains(&true) {
                OperatorKind::Sub
            } else if parser.next_kind_eq(&TokenKind::Mul).contains(&true) {
                OperatorKind::Mul
            } else if parser.next_kind_eq(&TokenKind::Div).contains(&true) {
                OperatorKind::Div
            } else if parser.next_kind_eq(&TokenKind::Shr).contains(&true) {
                OperatorKind::Shr
            } else if parser.next_kind_eq(&TokenKind::Shl).contains(&true) {
                OperatorKind::Shl
            } else if parser.next_kind_eq(&TokenKind::Assign).contains(&true) {
                OperatorKind::Assign
            } else if parser.next_kind_eq(&TokenKind::Eq).contains(&true) {
                OperatorKind::Eq
            } else if parser.next_kind_eq(&TokenKind::NegEq).contains(&true) {
                OperatorKind::NegEq
            } else {
                return Err(ParserError::ExpectedToken);
            }
        };

        Ok(Self {
            kind,
            next_expr: parser.next().unwrap(),
        })
    }
}
