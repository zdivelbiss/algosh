use std::{cell::RefCell, collections::BTreeMap};

use crate::{ssa::Scope, types::Type, Primitive};

#[repr(transparent)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Binding(usize);

impl Binding {
    #[inline]
    pub fn generate() -> Self {
        Self(fastrand::usize(..))
    }
}

pub enum NodeIndexer {
    Binding(Binding),
    Index(usize),
}

pub trait NodeIndex {
    fn into_indexer(self) -> NodeIndexer;
}

impl NodeIndex for Binding {
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
    pub fn simplify(&mut self, nodes: &Nodes) {
        match self {
            Self::Add(lhs, rhs) => {
                if let (Some(Self::Bind(lhs)), Some(Self::Bind(rhs))) = (
                    nodes.get(lhs.clone()).as_deref(),
                    nodes.get(rhs.clone()).as_deref(),
                ) {
                    if let Some(sum) = *lhs + *rhs {
                        *self = Node::Bind(sum);
                    }
                }
            }

            Self::Sub(lhs, rhs) => {
                if let (Some(Self::Bind(lhs)), Some(Self::Bind(rhs))) = (
                    nodes.get(lhs.clone()).as_deref(),
                    nodes.get(rhs.clone()).as_deref(),
                ) {
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
    cache: BTreeMap<Binding, RefCell<Node>>,
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

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.composition.is_empty()
    }

    pub fn bind_push(&mut self, node: Node) -> Binding {
        let binding = Binding::generate();
        assert!(self
            .cache
            .insert(binding.clone(), RefCell::new(node))
            .is_none());
        self.composition.push(binding.clone());

        binding
    }

    fn binding_index<Idx: NodeIndex>(&self, index: Idx) -> Option<Binding> {
        match index.into_indexer() {
            NodeIndexer::Binding(binding) => Some(binding),
            NodeIndexer::Index(index) => self.composition.get(index).map(Binding::clone),
        }
    }

    pub fn get<Idx: NodeIndex>(&self, index: Idx) -> Option<core::cell::Ref<Node>> {
        self.cache
            .get(&self.binding_index(index)?)
            .map(RefCell::borrow)
    }

    pub fn get_mut<Idx: NodeIndex>(&self, index: Idx) -> Option<core::cell::RefMut<Node>> {
        self.cache
            .get(&self.binding_index(index)?)
            .map(RefCell::borrow_mut)
    }
}

impl core::fmt::Debug for Nodes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(
                self.composition
                    .iter()
                    .map(|binding| self.cache.get(binding).unwrap().borrow().clone()),
            )
            .finish()
    }
}
