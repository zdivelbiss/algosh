use crate::{types::Type, Primitive};
use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
};

#[repr(transparent)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

    Parameter(Type),
    Call(Binding),
    Jump(Binding),
    Expression(super::Params),
    Return(Binding),
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

type NodeRef<'a> = (&'a Binding, Ref<'a, Node>);
type NodeMut<'a> = (&'a Binding, RefMut<'a, Node>);

pub struct Nodes {
    cache: HashMap<Binding, RefCell<Node>>,
    composition: Vec<Binding>,
}

impl Nodes {
    pub fn new() -> Self {
        Self {
            cache: HashMap::new(),
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

    pub fn get<Idx: NodeIndex>(&self, index: Idx) -> Option<Ref<Node>> {
        self.cache
            .get(&self.binding_index(index)?)
            .map(RefCell::borrow)
    }

    pub fn get_mut<Idx: NodeIndex>(&self, index: Idx) -> Option<RefMut<Node>> {
        self.cache
            .get(&self.binding_index(index)?)
            .map(RefCell::borrow_mut)
    }

    pub fn remove<Idx: NodeIndex>(&mut self, index: Idx) {
        self.binding_index(index)
            .and_then(|binding| self.cache.remove(&binding))
            .expect("index not found");
    }

    pub fn iter(&self) -> impl Iterator<Item = NodeRef> {
        self.composition
            .iter()
            .map(|binding| (binding, self.get(binding.clone()).unwrap()))
            .into_iter()
    }

    pub fn iter_mut(&self) -> impl Iterator<Item = NodeMut> {
        self.composition
            .iter()
            .map(|binding| (binding, self.get_mut(binding.clone()).unwrap()))
            .into_iter()
    }

    pub fn first(&self) -> Option<NodeRef> {
        self.composition
            .first()
            .map(|binding| (binding, self.get(binding.clone()).unwrap()))
    }

    pub fn first_mut(&self) -> Option<NodeMut> {
        self.composition
            .first()
            .map(|binding| (binding, self.get_mut(binding.clone()).unwrap()))
    }

    pub fn last(&self) -> Option<NodeRef> {
        self.composition
            .last()
            .map(|binding| (binding, self.get(binding.clone()).unwrap()))
    }

    pub fn last_mut(&self) -> Option<NodeMut> {
        self.composition
            .last()
            .map(|binding| (binding, self.get_mut(binding.clone()).unwrap()))
    }

    pub fn retain(&mut self, f: impl Fn(&Binding) -> bool) {
        self.cache.retain(|binding, _| f(binding));
        self.composition
            .retain(|binding| self.cache.contains_key(binding));
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
