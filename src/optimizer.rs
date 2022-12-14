use crate::{linearizer::LinearNode, Operator};

pub fn optimize(nodes: &mut Vec<LinearNode>) {
    simplify_consts(nodes);
}

fn simplify_consts(nodes: &mut Vec<LinearNode>) {
    let mut from_end = nodes.len();
    loop {
        let index = nodes.len() - from_end;

        let op = match nodes.get(index) {
            Some(LinearNode::Operator(op)) => *op,
            Some(_) => {
                from_end -= 1;
                continue;
            }
            None => break,
        };

        // Operators & operands are in postfix notation.
        match (&nodes[index - 2], op, &nodes[index - 1]) {
            (LinearNode::Int(lhs), op, LinearNode::Int(rhs)) => {
                let splice_value = match op {
                    Operator::Add => Some(lhs.wrapping_add(*rhs)),
                    Operator::Sub => Some(lhs.wrapping_sub(*rhs)),
                    Operator::Mul => Some(lhs.wrapping_mul(*rhs)),
                    Operator::Div => Some(lhs.wrapping_div(*rhs)),
                    Operator::Rem => Some(lhs.wrapping_rem(*rhs)),
                    Operator::Shr => Some(lhs.wrapping_shr(u32::try_from(*rhs).unwrap())),
                    Operator::Shl => Some(lhs.wrapping_shl(u32::try_from(*rhs).unwrap())),

                    _ => None,
                };

                if let Some(splice_value) = splice_value {
                    nodes
                        .splice((index - 2)..=index, [LinearNode::Int(splice_value)])
                        .count();
                }
            }

            _ => {}
        }

        from_end -= 1;
    }
}
