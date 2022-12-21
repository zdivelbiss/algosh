use crate::ssa::{Node, Nodes};
use std::collections::HashSet;

pub fn optimize(nodes: &mut Nodes) {
    simplify(nodes);
    prune(nodes);
}

fn simplify(nodes: &mut Nodes) {
    nodes
        .iter_mut()
        .for_each(|(_, mut node)| node.simplify(nodes));
}

fn prune(nodes: &mut Nodes) {
    let mut used = HashSet::new();

    nodes.iter().for_each(|(slf, node)| match &*node {
        Node::Add(lhs, rhs) => {
            used.insert(lhs.clone());
            used.insert(rhs.clone());
        }
        Node::Sub(lhs, rhs) => {
            used.insert(lhs.clone());
            used.insert(rhs.clone());
        }

        Node::Call(to) => {
            used.insert(to.clone());
        }
        Node::Jump(to) => {
            used.insert(to.clone());
        }
        Node::Expression(scope) => {
            scope.iter().for_each(|(_, binding)| {
                used.insert(binding.clone());
            });
        }
        Node::Return(value) => {
            used.insert(slf.clone());
            used.insert(value.clone());
        }

        Node::Bind(_) | Node::Parameter(_) => {}

        #[allow(unreachable_patterns)]
        node => todo!("{:?}", node),
    });

    nodes.retain(|binding| used.contains(binding));
}
