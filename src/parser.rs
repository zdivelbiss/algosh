use std::borrow::BorrowMut;

use crate::lexer::Token;

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

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value {
            Token::Eq => Ok(BinaryOp::Eq),
            Token::NegEq => Ok(BinaryOp::NegEq),
            Token::Add => Ok(BinaryOp::Add),
            Token::Sub => Ok(BinaryOp::Sub),
            Token::Mul => Ok(BinaryOp::Mul),
            Token::Div => Ok(BinaryOp::Div),
            Token::Shr => Ok(BinaryOp::Shr),
            Token::Shl => Ok(BinaryOp::Shl),

            _ => Err(()),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Variable {
    Integer(isize),
    String(String),
    Identifier(String),
}

impl<'a> TryFrom<&Token> for Variable {
    type Error = ();

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value {
            Token::Integer(int) => Ok(Self::Integer(*int)),
            Token::String(string) => Ok(Self::String(string.clone())),
            Token::Identifier(identifier) => Ok(Self::Identifier(identifier.clone())),

            _ => Err(()),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Conditional {
    Start { condition: Node, block: Node },
    Next { condition: Node, block: Node },
    End { block: Node },
}

impl TryFrom<&mut Parser> for Conditional {
    type Error = ();

    fn try_from(parser: &mut Parser) -> Result<Self, Self::Error> {
        match parser.peek_token() {
            Some(&Token::StartCondition) => {
                parser.discard_token();

                let condition = Node::try_from(parser.borrow_mut())?;
                let Ok(Node::Block(block_nodes)) = Node::try_from(parser.borrow_mut()) else { return Err(()) };

                Ok(Self::Start {
                    condition,
                    block: Node::Block(block_nodes),
                })
            }

            Some(&Token::NextCondition) => {
                parser.discard_token();

                let condition = Node::try_from(parser.borrow_mut())?;
                let Ok(Node::Block(block_nodes)) = Node::try_from(parser.borrow_mut()) else { return Err(()) };

                Ok(Self::Next {
                    condition,
                    block: Node::Block(block_nodes),
                })
            }

            Some(&Token::EndCondition) => {
                parser.discard_token();

                let Ok(Node::Block(block_nodes)) = Node::try_from(parser.borrow_mut()) else { return Err(()) };

                Ok(Self::End {
                    block: Node::Block(block_nodes),
                })
            }

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

    Conditionals(Vec<Conditional>),

    Block(Vec<Node>),
}

impl TryFrom<&mut Parser> for Node {
    type Error = ();

    fn try_from(parser: &mut Parser) -> Result<Self, Self::Error> {
        println!("PARSE TOKEN: {:?}", parser.peek_token().ok_or(())?);

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
        } else if let Ok(Conditional::Start { condition, block }) =
            Conditional::try_from(parser.borrow_mut())
        {
            let mut conditionals = vec![Conditional::Start { condition, block }];

            while let Ok(conditional) = Conditional::try_from(parser.borrow_mut()) {
                conditionals.push(conditional);
            }

            Ok(Node::Conditionals(conditionals))
        } else if let Some(&Token::BlockOpen) = parser.peek_token() {
            parser.discard_token();

            // Loop over nodes until an end block is found.
            let mut block_nodes = Vec::new();
            loop {
                let peek_token = parser.peek_token().ok_or(())?;
                if !peek_token.eq(&Token::BlockClose) {
                    let block_node = Node::try_from(parser.borrow_mut())?;
                    block_nodes.push(block_node);
                } else {
                    parser.discard_token();
                    break;
                }
            }

            // Return block node if any nodes were successfully parsed.
            Ok(Node::Block(block_nodes))
        } else {
            Err(())
        }
    }
}

pub struct Parser {
    tokens: std::iter::Peekable<Box<dyn Iterator<Item = Token>>>,
}

impl Parser {
    pub fn new(tokens: Box<dyn Iterator<Item = Token>>) -> Self {
        Self {
            tokens: tokens.peekable(),
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

    #[inline]
    fn discard_token(&mut self) {
        self.next_token().expect("cannot discard no tokens");
    }
}

impl Iterator for Parser {
    type Item = Node;

    fn next(&mut self) -> Option<Self::Item> {
        Node::try_from(self).ok()
    }
}
