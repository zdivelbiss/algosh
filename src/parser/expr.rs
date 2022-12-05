use intaglio::Symbol;

use crate::{
    lexer::TokenKind,
    parser::{Parser, ParserError},
};

#[derive(Debug, PartialEq)]
pub enum EvalKind {
    Lazy,
    Eager,
    Const,
}

impl TryFrom<&TokenKind> for EvalKind {
    type Error = ParserError;

    fn try_from(kind: &TokenKind) -> Result<Self, Self::Error> {
        match kind {
            TokenKind::EvalLazy => Ok(Self::Lazy),
            TokenKind::EvalEager => Ok(Self::Eager),
            TokenKind::EvalConst => Ok(Self::Const),

            // Pass `Unknown` error to avoid returning large errors from frequently-called functions.
            _ => Err(ParserError::Discard),
        }
    }
}

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

            // Pass `Unknown` error to avoid returning large errors from frequently-called functions.
            _ => Err(ParserError::Discard),
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

            _ => Err(ParserError::Discard),
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
            TokenKind::Int(int) => Ok(Self::Int(*int)),
            TokenKind::Bool(bool) => Ok(Self::Bool(*bool)),
            TokenKind::String(string) => Ok(Self::String(*string)),

            _ => Err(ParserError::Discard),
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

    Terminator,
}

impl TryFrom<&mut Parser<'_>> for Expression {
    // FIXME: don't use static lifetime here
    type Error = ParserError;

    fn try_from(parser: &mut Parser<'_>) -> Result<Self, Self::Error> {
        match parser.peek().ok_or(ParserError::NoMoreTokens)? {
            &TokenKind::Terminator => {
                parser.discard();

                Ok(Self::Terminator)
            },

            &TokenKind::ParameterBrace => {
                parser.discard();

                let mut parameters = Vec::new();
                loop {
                    let name = parser.expect_with(|kind| match kind {
                        &TokenKind::Identifier(name) => Ok(name),

                        kind => Err(ParserError::FoundMsg {
                            found: Some(kind.clone()),
                            msg: "expected identifier (hint: parameter format is `name: Int`)"
                                .to_string(),
                        }),
                    })?;
                    parser.expect(&TokenKind::Assign)?;
                    parameters.push((
                        name,
                        parser.expect_with(|kind| {
                            TypeKind::try_from(kind).map_err(|_| ParserError::FoundMsg {
                                found: Some(kind.clone()),
                                msg: "parameter declaration expects type".to_string(),
                            })
                        })?,
                    ));

                    match parser.peek() {
                        Some(&TokenKind::ParameterBrace) => {
                            parser.discard();

                            break
                        },
                        Some(&TokenKind::Separator) => {
                            parser.discard();

                            continue
                        },

                        _ => {
                            return Err(ParserError::General(
                                "expected parameter separator `,` or parameter brace `|`"
                                    .to_string(),
                            ))
                        }
                    }
                }

                Ok(Self::Transform {
                    parameters,
                    next_expr: Box::new(Self::try_from(parser)?),
                })
            }

            kind if let Ok(eval_kind) = EvalKind::try_from(kind) => {
                parser.discard();

                Ok(Self::Evaluation {
                    kind: eval_kind,
                    next_expr: Box::new(Self::try_from(parser)?),
                })
            },

            kind if let Ok(operator_kind) = OperatorKind::try_from(kind) => {
                parser.discard();

                Ok(Self::Binary {
                    kind: operator_kind,
                    next_expr: Box::new(Self::try_from(parser)?),
                })
            },

            kind if let Ok(value_kind) = ValueKind::try_from(kind) => {
                parser.discard();

                Ok(Self::Value { kind: value_kind, next_expr: Box::new(Self::try_from(parser)?) })
            }

            kind if let &TokenKind::Identifier(name) = kind => {
                parser.discard();

                Ok(Self::Named {
                    name,
                    next_expr: Box::new(Self::try_from(parser)?),
                })
            },

            kind => panic!("NOT YET IMPLEMENTED {:?}", kind),
        }
    }
}
