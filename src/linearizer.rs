use crate::{
    parser::{Expression, HeapExpr},
    Operator, Primitive,
};
use intaglio::Symbol;

#[derive(Debug, Clone)]
pub enum LinearNode {
    Int(isize),
    Operator(Operator),

    Var(Symbol, Vec<Self>),
    Type(Symbol, Vec<Self>),
}

pub fn linearize(exprs: &[HeapExpr]) -> Vec<LinearNode> {
    let mut nodes = Vec::new();

    for expr in exprs {
        extract_nodes(&mut nodes, expr);
    }

    nodes
}

fn extract_nodes(nodes: &mut Vec<LinearNode>, expr: &HeapExpr) {
    match &expr.0 {
        Expression::Binary(leftexpr, op, rightexpr) => {
            extract_nodes(nodes, leftexpr);
            extract_nodes(nodes, rightexpr);
            nodes.push(LinearNode::Operator(op.clone()));
        }

        Expression::Primitive(Primitive::Int(int)) => nodes.push(LinearNode::Int(*int)),

        unhandled => panic!("unimplemented: {:?}", unhandled),
    }
}
