use crate::ssa::Nodes;

pub fn optimize(nodes: &mut Nodes) {
    simplify(nodes);
}

fn simplify(nodes: &mut Nodes) {
    let mut index = 0;
    while let Some(mut cur) = nodes.get_mut(index) {
        cur.simplify(nodes);

        index += 1;
    }
}
