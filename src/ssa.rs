use crate::{
    parser::{Expression, HeapExpr},
    Error, Operator, Primitive, TupleComponent,
};
use intaglio::Symbol;
use std::collections::BTreeMap;

pub type Scope = BTreeMap<Symbol, Binding>;

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Binding(usize);

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Bind(Primitive),
    Add(Binding, Binding),
    Sub(Binding, Binding),
}

pub fn translate(ast: Box<[HeapExpr]>) -> Result<(Vec<Instruction>, Vec<Scope>), Vec<Error>> {
    let mut nodes = Vec::new();
    let mut scopes = vec![Scope::new()];
    let mut errs = Vec::new();

    for expr in ast.iter() {
        if let Err(err) = bind_expr(&expr, &mut nodes, &mut scopes) {
            errs.push(err);
        }
    }

    if errs.is_empty() {
        Ok((nodes, scopes))
    } else {
        Err(errs)
    }
}

fn bind_push(nodes: &mut Vec<Instruction>, new_node: Instruction) -> Binding {
    let binding = Binding(nodes.len());
    nodes.push(new_node);

    binding
}

fn bind_expr(
    expr: &HeapExpr,
    nodes: &mut Vec<Instruction>,
    scopes: &mut Vec<Scope>,
) -> Result<Binding, Error> {
    match &expr.0 {
        Expression::Binary(lhs, op, rhs) => {
            let lhs_binding = bind_expr(&lhs, nodes, scopes)?;
            let rhs_binding = bind_expr(&rhs, nodes, scopes)?;

            match op {
                Operator::Add => Ok(bind_push(nodes, Instruction::Add(lhs_binding, rhs_binding))),
                Operator::Sub => Ok(bind_push(nodes, Instruction::Sub(lhs_binding, rhs_binding))),

                _ => unimplemented!(),
            }
        }

        Expression::Primitive(primitive) => Ok(bind_push(nodes, Instruction::Bind(*primitive))),
        Expression::Identifier(symbol) => find_var_binding(symbol, scopes).ok_or_else(|| {
            Error::undeclared_var(
                expr.1.clone(),
                crate::strings::get_intern_str(*symbol),
                Some("ssa_identifier"),
            )
        }),

        Expression::Tuple(components) => {
            let mut binding = None;
            let current_scope = scopes.last_mut().unwrap();
            for (name, component) in components {
                let TupleComponent::Valued(primitive) = component else { unimplemented!() };
                binding = Some(bind_push(nodes, Instruction::Bind(*primitive)));
                current_scope.insert(*name, binding.unwrap());
            }

            Ok(binding.unwrap())
        }

        _ => unimplemented!(),
    }
}

fn find_var_binding(symbol: &Symbol, scopes: &mut Vec<Scope>) -> Option<Binding> {
    scopes
        .iter()
        .rev()
        .find_map(|scope| scope.get(symbol).cloned())
}
