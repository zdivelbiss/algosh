use crate::lexer::{Token, TokenKind};
use core::panic;
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
}

impl<'a> TryFrom<&Token> for BinaryOp {
    type Error = ();

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        match token.kind() {
            TokenKind::Eq => Ok(BinaryOp::Eq),
            TokenKind::NegEq => Ok(BinaryOp::NegEq),
            TokenKind::Add => Ok(BinaryOp::Add),
            TokenKind::Sub => Ok(BinaryOp::Sub),
            TokenKind::Mul => Ok(BinaryOp::Mul),
            TokenKind::Div => Ok(BinaryOp::Div),
            TokenKind::Shr => Ok(BinaryOp::Shr),
            TokenKind::Shl => Ok(BinaryOp::Shl),

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
}

impl<'a> TryFrom<&Token> for Variable {
    type Error = ();

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        match token.kind() {
            TokenKind::Integer(int) => Ok(Self::Integer(*int)),
            TokenKind::String(string) => Ok(Self::String(string.clone())),
            TokenKind::Boolean(boolean) => Ok(Self::Boolean(*boolean)),
            TokenKind::Identifier(identifier) => Ok(Self::Identifier(identifier.clone())),

            _ => Err(()),
        }
    }
}

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
    type Error = ();

    fn try_from(parser: &mut Parser) -> Result<Self, Self::Error> {
        if let Some(variable0) = parser.with_next(|t| Variable::try_from(t).ok()) {
            if let Some(op) = parser.with_next(|t| BinaryOp::try_from(t).ok())
                && let Ok(righthand_node) = Node::try_from(parser.borrow_mut())
            {
                Ok(Node::BinaryOp {
                    lefthand: variable0,
                    op,
                    righthand: Box::new(righthand_node) })
            } else {
                Ok(Node::Variable(variable0))
            }
        } else if let Some(true) = parser.next_eq(&TokenKind::StartCondition) {
            let Ok(condition) = Node::try_from(parser.borrow_mut())
                else {
                    let peek_token = parser.peek_token().expect("no peekable token; this is a compiler error").clone();
                    crate::throw_error(parser.tokens.src(), "Expected conditional expression", &parser.tokens.find_token(&peek_token).unwrap(), true);
                    
                    loop{
                    }
                };
            let Ok(Node::Block(block_nodes)) = Node::try_from(parser.borrow_mut()) else { panic!("expected block") };

            if let Some(true) = parser.next_eq(&TokenKind::NextCondition) {
                Ok(Node::Conditional {
                    condition: Box::new(condition),
                    block: Box::new(Node::Block(block_nodes)),
                    next_conditional: Some(Box::new(
                        Node::try_from(parser.borrow_mut()).expect("expected block or condition"),
                    )),
                })
            } else {
                Ok(Node::Conditional {
                    condition: Box::new(condition),
                    block: Box::new(Node::Block(block_nodes)),
                    next_conditional: None,
                })
            }
        } else if let Some(true) = parser.next_eq(&TokenKind::BlockOpen) {
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
        } else {
            Err(())
        }
    }
}

pub struct Parser<'a> {
    tokens: crate::lexer::LexerIterator<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: crate::lexer::LexerIterator<'a>) -> Self {
        Self {
            tokens
        }
    }

    #[inline]
    fn next_token(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    #[inline]
    fn peek_token(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    fn with_next<T>(&mut self, if_func: impl FnOnce(&Token) -> Option<T>) -> Option<T> {
        let peek = self.peek_token()?;
        let result = if_func(peek);
        if result.is_some() {
            self.discard_token();
        }

        result
    }

    fn next_eq(&mut self, kind: &TokenKind) -> Option<bool> {
        let peek = self.peek_token()?;
        let eq = peek.kind().eq(kind);
        if eq {
            self.discard_token();
        }

        Some(eq)
    }

    #[inline]
    fn discard_token(&mut self) {
        self.next_token().expect("cannot discard no tokens");
    }
}

impl Iterator for Parser<'_> {
    type Item = Node;

    fn next(&mut self) -> Option<Self::Item> {
        Node::try_from(self).ok()
    }
}
