use crate::{
    ssa::{Binding, Scope},
    types::Type,
    Primitive,
};

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
        println!("{:?}", self);
        match self {
            Self::Add(lhs, rhs) => {
                if let (Self::Bind(lhs), Self::Bind(rhs)) =
                    (&prev_nodes[lhs.as_usize()], &prev_nodes[rhs.as_usize()])
                {
                    if let Some(sum) = *lhs + *rhs {
                        println!("LHS {:?} RHS {:?} SIMPLIFY TO {:?}", lhs, rhs, &sum);

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

#[derive(Debug)]
pub struct Nodes(Vec<Node>);

impl Nodes {
    pub const fn new() -> Self {
        Self(Vec::new())
    }
}

impl From<Binding> for usize {
    fn from(value: Binding) -> Self {
        value.0
    }
}

impl core::ops::Deref for Nodes {
    type Target = Vec<Node>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl core::ops::DerefMut for Nodes {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: Into<usize>> core::ops::Index<T> for Nodes {
    type Output = Node;

    fn index(&self, index: T) -> &Self::Output {
        &self.0[index.into()]
    }
}

impl<T: Into<usize>> core::ops::IndexMut<T> for Nodes {
    fn index_mut(&mut self, index: T) -> &mut Self::Output {
        &mut self.0[index.into()]
    }
}
