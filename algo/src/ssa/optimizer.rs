use crate::ssa::Nodes;

use super::Node;

pub fn optimize(nodes: &mut Nodes) {
    simplify(nodes);
}

fn simplify(nodes: &mut Nodes) {
    let mut index = 0;
    while let Some(cur) = nodes.get(index) {
        match &*cur {
            Node::Add(lhs, rhs) => {
                if let (Some(Node::Bind(lhs)), Some(Node::Bind(rhs))) =
                    (nodes.get(lhs).as_deref(), nodes.get(rhs).as_deref())
                {
                    if let Some(sum) = lhs
                }
            }
        }

        index += 1;
    }
}
