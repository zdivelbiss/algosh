use std::ops::Add;

use crate::{
    ssa::{Node, Nodes},
    Primitive,
};

pub fn optimize(nodes: &mut Nodes) {
    let mut index = 0;
    while index < nodes.len() {
        let (prev, cur) = nodes.split_at_mut(index);
        let cur = &mut cur[0];

        match cur {
            Node::Add(lhs, rhs) => {
                let lhs = &prev[lhs.as_usize()];
                let rhs = &prev[rhs.as_usize()];

                match (lhs, rhs) {
                    (Node::Bind(lhs), Node::Bind(rhs)) => {
                        if let Some(sum) = *lhs + *rhs {
                            *cur = Node::Bind(sum);
                        }
                    },``
                }
            }

            _ => {}
        }

        index += 1;
    }
}
