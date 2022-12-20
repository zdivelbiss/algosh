use std::{cell::UnsafeCell, collections::BTreeMap};

use crate::{ssa::Scope, types::Type, Primitive};

#[repr(transparent)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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

pub enum NodeIndexer {
    Binding(Binding),
    Index(usize),
}

pub trait NodeIndex {
    fn into_indexer(self) -> NodeIndexer;
}

impl NodeIndex for &Binding {
    fn into_indexer(self) -> NodeIndexer {
        NodeIndexer::Binding(self)
    }
}

impl NodeIndex for usize {
    fn into_indexer(self) -> NodeIndexer {
        NodeIndexer::Index(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

pub struct Nodes {
    cache: BTreeMap<Binding, UnsafeCell<Node>>,
    composition: Vec<Binding>,
}

impl Nodes {
    pub const fn new() -> Self {
        Self {
            cache: BTreeMap::new(),
            composition: Vec::new(),
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.composition.len()
    }

    pub fn bind_push(&mut self, node: Node) -> Binding {
        let binding = Binding::generate();
        assert!(self
            .cache
            .insert(binding.clone(), UnsafeCell::new(node))
            .is_none());
        self.composition.push(binding.clone());

        binding
    }

    pub fn get<Idx: NodeIndex>(&self, index: Idx) -> Option<NodeRef> {
        let binding = match index.into_indexer() {
            NodeIndexer::Binding(binding) => binding,
            NodeIndexer::Index(index) => self.composition.get(index)?.clone(),
        };

        self.cache.get(&binding).map(NodeRef)
    }
}

pub struct NodeRef<'a>(&'a UnsafeCell<Node>);

impl core::ops::Deref for NodeRef<'_> {
    type Target = Node;

    fn deref(&self) -> &Self::Target {
        // Safety: Requirements are met by the invariants of `UnsafeCell` itself.
        unsafe { self.0.get().as_ref() }.unwrap()
        // Additionally, the lifetime requirements are guaranteed by the `&self` borrow.
    }
}

impl core::ops::DerefMut for NodeRef<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // Safety: Requirements are met by the invariants of `UnsafeCell` itself.
        unsafe { self.0.get().as_mut() }.unwrap()
        // Additionally, the lifetime requirements are guaranteed by the `&mut self` borrow.
    }
}
