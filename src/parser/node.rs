use crate::{
    lexer::{Token, TokenKind},
    parser::Parser,
};
use std::borrow::BorrowMut;

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
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

impl<'a> TryFrom<&Token> for BinaryOp {
    type Error = ();

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        match token.kind() {
            TokenKind::Eq => Ok(Self::Eq),
            TokenKind::NegEq => Ok(Self::NegEq),
            TokenKind::Add => Ok(Self::Add),
            TokenKind::Sub => Ok(Self::Sub),
            TokenKind::Mul => Ok(Self::Mul),
            TokenKind::Div => Ok(Self::Div),
            TokenKind::Shr => Ok(Self::Shr),
            TokenKind::Shl => Ok(Self::Shl),
            TokenKind::Assign => Ok(Self::Assign),

            _ => Err(()),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Variable {
    Integer(isize),
    String(String),
    Boolean(bool),
    Identifier(String),
    Array(Vec<Box<Self>>),
    EnvVar(String),
    EnvCommand(String),
}

impl<'a> TryFrom<&TokenKind> for Variable {
    type Error = ();

    fn try_from(kind: &TokenKind) -> Result<Self, Self::Error> {
        match kind {
            TokenKind::Integer(int) => Ok(Self::Integer(*int)),
            TokenKind::String(string) => Ok(Self::String(string.clone())),
            TokenKind::Boolean(boolean) => Ok(Self::Boolean(*boolean)),
            TokenKind::Identifier(identifier) => Ok(Self::Identifier(identifier.clone())),
            TokenKind::EnvCommand(command) => Ok(Self::EnvCommand(command.clone())),
            TokenKind::EnvVar(command) => Ok(Self::EnvVar(command.clone())),

            _ => Err(()),
        }
    }
}

type NodeError = ();

#[derive(Debug, PartialEq)]
pub enum Node {
    Variable(Variable),

    BinaryOp {
        lefthand: Variable,
        op: BinaryOp,
        righthand: Box<Self>,
    },

    Conditional {
        condition: Box<Self>,
        block: Box<Self>,
        next_conditional: Option<Box<Self>>,
    },

    Block(Vec<Node>),
}

impl TryFrom<&mut Parser<'_>> for Node {
    type Error = NodeError;

    fn try_from(parser: &mut Parser) -> Result<Self, Self::Error> {
        let token = parser.next_token().ok_or(())?;

        match token.kind() {
            &TokenKind::ArrayOpen => {
                let mut variables = Vec::new();
                loop {
                    let Ok(Node::Variable(variable)) = Node::try_from(parser.borrow_mut())
                        else { parser.throw("array expected a variable declaration") };
                    variables.push(Box::new(variable));

                    if let Some(true) = parser.next_eq(&TokenKind::ArrayClose) {
                        break;
                    } else if let Some(true) = parser.next_eq(&TokenKind::Separator) {
                        continue;
                    } else {
                        parser.throw("expected a variable separator or array closure")
                    }
                }

                Ok(Node::Variable(Variable::Array(variables)))
            }

            &TokenKind::StartCondition => {
                let Ok(condition) = Node::try_from(parser.borrow_mut())
                else { parser.throw("expected condition for `if` expression") };
                let Ok(Node::Block(block_nodes)) = Node::try_from(parser.borrow_mut())
                else { parser.throw("expected code block for `if` expression") };

                if let Some(true) = parser.next_eq(&TokenKind::NextCondition) {
                    let next_conditional = {
                        match Node::try_from(parser.borrow_mut()) {
                            Ok(Node::Conditional {
                                condition,
                                block,
                                next_conditional,
                            }) => Node::Conditional {
                                condition,
                                block,
                                next_conditional,
                            },

                            Ok(Node::Block(block_nodes)) => Node::Block(block_nodes),

                            _ => parser
                                .throw("expected code block or `if` following `else` expression"),
                        }
                    };

                    Ok(Node::Conditional {
                        condition: Box::new(condition),
                        block: Box::new(Node::Block(block_nodes)),
                        next_conditional: Some(Box::new(next_conditional)),
                    })
                } else {
                    Ok(Node::Conditional {
                        condition: Box::new(condition),
                        block: Box::new(Node::Block(block_nodes)),
                        next_conditional: None,
                    })
                }
            }

            &TokenKind::BlockOpen => {
                // Loop over nodes until an end block token is found.
                let mut block_nodes = Vec::new();
                loop {
                    match parser.next_eq(&TokenKind::BlockClose) {
                    Some(true) => break,
                    Some(false) if let Ok(block_node) = Node::try_from(parser.borrow_mut()) => block_nodes
                    .push(block_node),
                    _ => panic!("expected valid node"),
                }
                }

                // Return block node if any nodes were successfully parsed.
                Ok(Node::Block(block_nodes))
            }

            kind if let Ok(variable) = Variable::try_from(kind) => {
                if let Some(op) = parser.with_next(|t| BinaryOp::try_from(t).ok())
                    && let Ok(righthand_node) = Node::try_from(parser.borrow_mut())
                {
                    Ok(Node::BinaryOp {
                        lefthand: variable,
                        op,
                        righthand: Box::new(righthand_node)
                    })
                } else {
                    Ok(Node::Variable(variable))
                }
            }

            kind => parser.throw(format!("unexpected token: {:?}", kind).as_str())
        }
    }
}
