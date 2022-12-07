use crate::{
    lexer::{TokenKind, },
    parser::{Parser, ParserError},
    token,
};
use intaglio::Symbol;
use std::borrow::BorrowMut;

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

#[derive(Debug, PartialEq)]
pub enum TypeKind {
    Int,
    Bool,
    String,
    Array(Box<Self>),
    Tuple(Vec<Box<Self>>),
}

impl TryFrom<&TokenKind> for TypeKind {
    type Error = ParserError;

    fn try_from(kind: &TokenKind) -> Result<Self, Self::Error> {
        match kind {
            TokenKind::TypeInt => Ok(Self::Int),
            TokenKind::TypeBool => Ok(Self::Bool),
            TokenKind::TypeString => Ok(Self::String),

            _ => Err(ParserError::UnexpectedToken),
        }
    }
}

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

#[derive(Debug, PartialEq)]
pub struct Argument {
    name: Symbol,
    ty: TypeKind,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Evaluation {
        kind: EvalKind,
        next_expr: Box<Self>,
    },

    Transform {
        parameters: Vec<(Symbol, TypeKind)>,
        next_expr: Box<Self>,
    },

    Binary {
        kind: OperatorKind,
        next_expr: Box<Self>,
    },

    Named {
        name: Symbol,
        next_expr: Box<Self>,
    },

    Value {
        kind: ValueKind,
        next_expr: Box<Self>,
    },

    Type {
        kind: TypeKind,
        next_expr: Box<Self>,
    },

    Grouping {
        group: Box<Self>,
        next_expr: Box<Self>,
    },

    GroupingEnd,

    Terminator,
}

impl TryFrom<&mut Parser<'_>> for Expression {
    // FIXME: don't use static lifetime here
    type Error = ParserError;

    fn try_from(parser: &mut Parser<'_>) -> Result<Self, Self::Error> {
        // If there are no more tokens left in the parser, simply return a terminator.
        // An error with this kind of behaviour will result in an infinite loop.
        let peek = match parser.peek() {
            Some(peek) => peek,
            None => return Ok(Self::Terminator),
        };

        match peek.kind() {
            TokenKind::StatementEnd => {
                parser.advance();

                Ok(Self::Terminator)
            },

            TokenKind::GroupingOpen => {
                parser.advance();

                let inner_expr = Self::try_from(parser.borrow_mut())?;
                parser.expect(&token!(TokenKind::GroupingClose))?;

                Ok(Expression::Grouping {
                    group: Box::new(inner_expr),
                    next_expr: Box::new(Self::try_from(parser.borrow_mut())?)
                })
            },

            TokenKind::GroupingClose => Ok(Expression::GroupingEnd),

            TokenKind::ParameterBrace => {
                parser.advance();

                let mut parameters = Vec::new();
                loop {
                    let name = parser.expect_with(|t| {
                        if let TokenKind::Identifier(name) = t.kind() {
                            Ok(*name)
                        } else {
                            Err(ParserError::FoundMsg {
                                found: Some(t.clone()),
                                msg: String::from("expected identifier (hint: parameter format is `name: Int`)"),
                            })
                        }
                    })?;
                
                    parser.expect(&token!( TokenKind::Assign))?;
                    parameters.push((
                        name,
                        parser.expect_with(|t| {
                            TypeKind::try_from(t.kind()).map_err(|_| ParserError::FoundMsg {
                                found: Some(t.clone()),
                                msg: String::from( "parameter declaration expects type"),
                            })
                        })?,
                    ));

                    match parser.peek().map(crate::lexer::Token::kind) {
                        Some(&TokenKind::ParameterBrace) => {
                            parser.advance();

                            break
                        },
                        Some(&TokenKind::Separator) => {
                            parser.advance();

                            continue
                        },

                        _ => {
                            return Err(ParserError::ReplaceThisError)
                        }
                    }
                }

                Ok(Self::Transform {
                    parameters,
                    next_expr: Box::new(Self::try_from(parser)?),
                })
            }

            kind if let Ok(eval_kind) = EvalKind::try_from(kind) => {
                parser.advance();

                Ok(Self::Evaluation {
                    kind: eval_kind,
                    next_expr: Box::new(Self::try_from(parser)?),
                })
            },

            kind if let Ok(operator_kind) = OperatorKind::try_from(kind) => {
                parser.advance();

                Ok(Self::Binary {
                    kind: operator_kind,
                    next_expr: Box::new(Self::try_from(parser)?),
                })
            },

            kind if let Ok(value_kind) = ValueKind::try_from(kind) => {
                parser.advance();

                Ok(Self::Value { kind: value_kind, next_expr: Box::new(Self::try_from(parser)?) })
            }

            kind if let &TokenKind::Identifier(name) = kind => {
                parser.advance();

                Ok(Self::Named {
                    name,
                    next_expr: Box::new(Self::try_from(parser)?),
                })
            },


            kind => panic!("NOT YET IMPLEMENTED {:?}", kind),
        }
    }
}
