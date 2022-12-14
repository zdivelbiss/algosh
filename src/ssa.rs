use crate::{
    parser::{Expression, HeapExpr},
    Operator, Primitive,
};
use intaglio::Symbol;
use std::collections::BTreeMap;

type VarMap = BTreeMap<Symbol, Binding>;

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Binding(usize);

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Bind(isize),
    Add(Binding, Binding),
    Sub(Binding, Binding),
}

pub fn translate(ast: &[HeapExpr]) -> Vec<Node> {
    let mut nodes = Vec::new();
    let mut var_map = VarMap::new();

    for expr in ast {
        bind_expr(&expr, &mut nodes, &mut var_map);
    }

    nodes
}

fn bind_push(nodes: &mut Vec<Node>, new_node: Node) -> Binding {
    let binding = Binding(nodes.len());
    nodes.push(new_node);

    binding
}

fn bind_expr(expr: &HeapExpr, nodes: &mut Vec<Node>, var_map: &mut VarMap) -> Binding {
    match &expr.0 {
        Expression::Binary(lhs, op, rhs) => {
            let lhs_binding = bind_expr(&lhs, nodes, var_map);
            let rhs_binding = bind_expr(&rhs, nodes, var_map);

            match op {
                Operator::Add => bind_push(nodes, Node::Add(lhs_binding, rhs_binding)),
                Operator::Sub => bind_push(nodes, Node::Add(lhs_binding, rhs_binding)),

                _ => unimplemented!(),
            }
        }

        Expression::Primitive(Primitive::Int(int)) => bind_push(nodes, Node::Bind(*int)),

        _ => unimplemented!(),
    }
}
