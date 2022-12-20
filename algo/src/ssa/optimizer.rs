use crate::ssa::Nodes;

pub fn optimize(nodes: &mut Nodes) {
    simplify(nodes);
}

// fn simplify(nodes: &mut Nodes) {
//     let mut index = 0;
//     while index < nodes.len() {
//         let (prev, cur) = nodes.split_at_mut(index);
//         let cur = &mut cur[0];

//         cur.simplify(prev);

//         index += 1;
//     }
// }
