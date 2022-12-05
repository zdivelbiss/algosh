pub mod operator;
pub mod value;

use intaglio::Symbol;

use crate::{
    lexer::TokenKind,
    parser::{Parser, ParserError},
};

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

#[derive(Debug, PartialEq)]
pub enum ValueType {
    Int(Option<isize>),
    Bool(Option<bool>),
    String(Option<Symbol>),
    Array(Option<Vec<Box<Self>>>),
    Tuple(Option<Vec<Box<Self>>>),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Binary {
        kind: OperatorKind,
        next_expr: Box<Self>,
    },

    Named {
        name: Symbol,
        next_expr: Box<Self>,
    },

    Value {
        name: Option<Symbol>,
        kind: ValueType,
        next_expr: Box<Self>,
    },

    Terminator,
}

impl TryFrom<&mut Parser<'_>> for Expression {
    type Error = ParserError;

    fn try_from(parser: &mut Parser<'_>) -> Result<Self, Self::Error> {
        match parser.peek().ok_or(ParserError::NoMoreTokens)? {
            &TokenKind::Terminator => Ok(Self::Terminator),

            &TokenKind::Add => Ok(Self::Binary {
                kind: OperatorKind::Add,
                next_expr: Self::try_from(parser)?,
            }),
            &TokenKind::Sub => Ok(Self::Binary {
                kind: OperatorKind::Sub,
                next_expr: Self::try_from(parser)?,
            }),
            &TokenKind::Mul => Ok(Self::Binary {
                kind: OperatorKind::Mul,
                next_expr: Self::try_from(parser)?,
            }),
            &TokenKind::Div => Ok(Self::Binary {
                kind: OperatorKind::Div,
                next_expr: Self::try_from(parser)?,
            }),
            &TokenKind::Shr => Ok(Self::Binary {
                kind: OperatorKind::Shr,
                next_expr: Self::try_from(parser)?,
            }),
            &TokenKind::Shl => Ok(Self::Binary {
                kind: OperatorKind::Shl,
                next_expr: Self::try_from(parser)?,
            }),
            &TokenKind::Eq => Ok(Self::Binary {
                kind: OperatorKind::Eq,
                next_expr: Self::try_from(parser)?,
            }),
            &TokenKind::NegEq => Ok(Self::Binary {
                kind: OperatorKind::NegEq,
                next_expr: Self::try_from(parser)?,
            }),
            &TokenKind::Assign => Ok(Self::Binary {
                kind: OperatorKind::Assign,
                next_expr: Self::try_from(parser)?,
            }),

            kind if let &TokenKind::Identifier(name) = kind => Ok(Self::Named {
                name,
                next_expr: Self::try_from(parser)?,
            }),


        }
    }
}

// mod argument_cell;
// pub use argument_cell::*;

// use crate::{lexer::TokenKind, parser::Parser};
// use intaglio::Symbol;
// use std::borrow::BorrowMut;

// #[derive(Debug, PartialEq)]
// pub enum OperatorKind {
//     Eq,
//     NegEq,
//     Add,
//     Sub,
//     Mul,
//     Div,
//     Shr,
//     Shl,
//     Assign,
// }

// impl<'a> TryFrom<&TokenKind> for OperatorKind {
//     type Error = ();

//     fn try_from(kind: &TokenKind) -> Result<Self, Self::Error> {
//         match kind {
//             TokenKind::Eq => Ok(Self::Eq),
//             TokenKind::NegEq => Ok(Self::NegEq),
//             TokenKind::Add => Ok(Self::Add),
//             TokenKind::Sub => Ok(Self::Sub),
//             TokenKind::Mul => Ok(Self::Mul),
//             TokenKind::Div => Ok(Self::Div),
//             TokenKind::Shr => Ok(Self::Shr),
//             TokenKind::Shl => Ok(Self::Shl),
//             TokenKind::Assign => Ok(Self::Assign),

//             _ => Err(()),
//         }
//     }
// }

// #[derive(Debug, PartialEq)]
// pub enum ValueKind {
//     Integer(isize),
//     String(Symbol),
//     Boolean(bool),
//     Array(Vec<Box<Self>>),
//     // TODO tuple variable
// }

// impl<'a> TryFrom<&TokenKind> for ValueKind {
//     type Error = ();

//     fn try_from(kind: &TokenKind) -> Result<Self, Self::Error> {
//         match kind {
//             TokenKind::Integer(int) => Ok(Self::Integer(*int)),
//             TokenKind::String(string) => Ok(Self::String(string.clone())),
//             TokenKind::Boolean(boolean) => Ok(Self::Boolean(*boolean)),

//             _ => Err(()),
//         }
//     }
// }

// #[derive(Debug, PartialEq)]
// pub enum EvaluationKind {
//     Lazy,
//     Eager,
//     Const,
// }

// impl TryFrom<&TokenKind> for EvaluationKind {
//     type Error = ExprErr;

//     fn try_from(kind: &TokenKind) -> Result<Self, Self::Error> {
//         match kind {
//             &TokenKind::LazyEval => Ok(Self::Lazy),
//             &TokenKind::EagerEval => Ok(Self::Eager),
//             &TokenKind::ConstEval => Ok(Self::Const),

//             _ => Err(())
//         }
//     }

// }

// #[derive(Debug, PartialEq)]
// pub enum Expression {
//     Evaluation {
//         kind: EvaluationKind,
//         expr: Box<Self>,
//     },

//     Transform {
//         arguments: ArgumentCell,
//         expr: Box<Self>,
//     },

//     Operator {
//         operator: OperatorKind,
//         expr: Box<Self>,
//     },

//     Named {
//         name: Symbol,
//         expr: Option<Box<Self>>,
//     },

//     Value {
//         value: ValueKind,
//         expr: Option<Box<Self>>,
//     },

//     Terminator,
// }

// impl Expression {
//     pub fn try_box_from(parser: &mut Parser) -> Result<Box<Self>, ExprErr> {
//         Self::try_from(parser).map(Box::new)
//     }
// }

// impl TryFrom<&mut Parser<'_>> for Expression {
//     type Error = ExprErr;

//     fn try_from(parser: &mut Parser) -> Result<Self, Self::Error> {
// parser.try_peek_kind()

//         match  {
//             Some(&TokenKind::Terminator) => Ok(Self::Terminator),

//             Some(TokenKind::Identifier(identifier)) => Ok(Self::Named {
//                 name: identifier,
//                 expr: Self::try_box_from(parser.borrow_mut()).ok(),
//             }),

//             Some(kind) if let Ok(evaluation) = EvaluationKind::try_from(&kind) => {
//                 Ok(Self::Evaluation { kind: evaluation, expr: Self::try_box_from(parser.borrow_mut())? })
//             },

//             Some(kind) if let Ok(operator) = OperatorKind::try_from(&kind) => {
//                 Ok(Self::Operator { operator, expr: Self::try_box_from(parser.borrow_mut())?  })
//             },

//             Some(kind) if let Ok(variable) = ValueKind::try_from(&kind) => {
//                 Ok(Self::Value { value: variable, expr: Self::try_box_from(parser.borrow_mut()).ok() })
//             }

//             Some(TokenKind::ArrayOpen) => {
//                  let mut variables = Vec::new();

//                  if let Some(false) = parser.next_kind_eq(&TokenKind::ArrayClose) {
//                  loop {
//                     let kind = parser.expect_next_kind();
//                     let Ok(variable) = ValueKind::try_from(&kind)
//                         else { parser.throw("array expected a variable declaration") };

//                     variables.push(Box::new(variable));

//                     if let Some(true) = parser.next_kind_eq(&TokenKind::ArrayClose) {
//                         break;
//                     } else if let Some(true) = parser.next_kind_eq(&TokenKind::Separator) {
//                         continue;
//                     } else {
//                         parser.throw("expected a variable separator or array closure")
//                     }

//                  }}

//                  Ok(Self::Value { value: ValueKind::Array(variables), expr: Self::try_box_from(parser.borrow_mut()).ok() })
//             }

//             _ => Err(())
//         }
//     }
// }

// pub type ExprErr = ();

// // #[derive(Debug, PartialEq)]
// // pub enum Node {
// //     Function {
// //         arguments: Vec<Argument>,
// //         body: Box<Self>,
// //     },

// //     Variable(Variable),

// //     BinaryOp {
// //         lefthand: Variable,
// //         op: BinaryOp,
// //         righthand: Box<Self>,
// //     },

// //     Conditional {
// //         condition: Box<Self>,
// //         block: Box<Self>,
// //         next_conditional: Option<Box<Self>>,
// //     },

// //     Block(Vec<Node>),
// // }

// // impl TryFrom<&mut Parser<'_>> for Node {
// //     type Error = NodeError;

// //     fn try_from(parser: &mut Parser) -> Result<Self, Self::Error> {
// //         let token = parser.next_kind().ok_or(())?;
// //         let peek_kind = parser.peek_token_kind();

// //         match token.kind() {

// //             &TokenKind::ArrayOpen if matches!(peek_kind, Some(&TokenKind::TypeInt) | Some(&TokenKind::TypeBool) | Some(&TokenKind::TypeString)) => {

// //             }

// //             &TokenKind::ArrayOpen => {
// //                 let mut variables = Vec::new();
// //                 loop {
// //                     let Ok(Node::Variable(variable)) = Node::try_from(parser.borrow_mut())
// //                         else { parser.throw("array expected a variable declaration") };
// //                     variables.push(Box::new(variable));

// //                     if let Some(true) = parser.next_eq(&TokenKind::ArrayClose) {
// //                         break;
// //                     } else if let Some(true) = parser.next_eq(&TokenKind::Separator) {
// //                         continue;
// //                     } else {
// //                         parser.throw("expected a variable separator or array closure")
// //                     }
// //                 }

// //                 Ok(Node::Variable(Variable::Array(variables)))
// //             }

// //             &TokenKind::StartCondition => {
// //                 let Ok(condition) = Node::try_from(parser.borrow_mut())
// //                 else { parser.throw("expected condition for `if` expression") };
// //                 let Ok(Node::Block(block_nodes)) = Node::try_from(parser.borrow_mut())
// //                 else { parser.throw("expected code block for `if` expression") };

// //                 if let Some(true) = parser.next_eq(&TokenKind::NextCondition) {
// //                     let next_conditional = {
// //                         match Node::try_from(parser.borrow_mut()) {
// //                             Ok(Node::Conditional {
// //                                 condition,
// //                                 block,
// //                                 next_conditional,
// //                             }) => Node::Conditional {
// //                                 condition,
// //                                 block,
// //                                 next_conditional,
// //                             },

// //                             Ok(Node::Block(block_nodes)) => Node::Block(block_nodes),

// //                             _ => parser
// //                                 .throw("expected code block or `if` following `else` expression"),
// //                         }
// //                     };

// //                     Ok(Node::Conditional {
// //                         condition: Box::new(condition),
// //                         block: Box::new(Node::Block(block_nodes)),
// //                         next_conditional: Some(Box::new(next_conditional)),
// //                     })
// //                 } else {
// //                     Ok(Node::Conditional {
// //                         condition: Box::new(condition),
// //                         block: Box::new(Node::Block(block_nodes)),
// //                         next_conditional: None,
// //                     })
// //                 }
// //             }

// //             &TokenKind::BlockOpen => {
// //                 // Loop over nodes until an end block token is found.
// //                 let mut block_nodes = Vec::new();
// //                 loop {
// //                     match parser.next_eq(&TokenKind::BlockClose) {
// //                     Some(true) => break,
// //                     Some(false) if let Ok(block_node) = Node::try_from(parser.borrow_mut()) => block_nodes
// //                     .push(block_node),
// //                     _ => panic!("expected valid node"),
// //                 }
// //                 }

// //                 // Return block node if any nodes were successfully parsed.
// //                 Ok(Node::Block(block_nodes))
// //             }

// //             kind if let Ok(variable) = Variable::try_from(kind) => {
// //                 if let Some(op) = parser.next_kind_with(|t| BinaryOp::try_from(t).ok())
// //                     && let Ok(righthand_node) = Node::try_from(parser.borrow_mut())
// //                 {
// //                     Ok(Node::BinaryOp {
// //                         lefthand: variable,
// //                         op,
// //                         righthand: Box::new(righthand_node)
// //                     })
// //                 } else {
// //                     Ok(Node::Variable(variable))
// //                 }
// //             }

// //             kind => parser.throw(format!("unexpected token: {:?}", kind).as_str())
// //         }
// //     }
// // }
