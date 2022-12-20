use std::collections::BTreeMap;

use crate::{ssa::Scope, types::Type, Primitive};

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Binding(usize);

impl Binding {
    #[inline]
    pub fn generate() -> Self {
        Self(fastrand::usize(..))
    }

    pub fn from_raw(value: usize) -> Self {
        Self(value)
    }

    #[inline]
    pub const fn as_usize(&self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Bind(Primitive),
    Add(Binding, Binding),
    Sub(Binding, Binding),
    Jump(Binding),

    Parameter(Type),
    Expression(Binding, Scope),
    Call(Binding),
}

impl Node {
    pub fn simplify(&mut self, prev_nodes: &[Node]) {
        match self {
            Self::Add(lhs, rhs) => {
                if let (Self::Bind(lhs), Self::Bind(rhs)) =
                    (&prev_nodes[lhs.as_usize()], &prev_nodes[rhs.as_usize()])
                {
                    if let Some(sum) = *lhs + *rhs {
                        *self = Node::Bind(sum);
                    }
                }
            }

            Self::Sub(lhs, rhs) => {
                if let (Self::Bind(lhs), Self::Bind(rhs)) =
                    (&prev_nodes[lhs.as_usize()], &prev_nodes[rhs.as_usize()])
                {
                    if let Some(sum) = *lhs - *rhs {
                        *self = Node::Bind(sum);
                    }
                }
            }

            _ => {}
        }
    }
}

pub type Nodes = BTreeMap<Binding, Node>;
