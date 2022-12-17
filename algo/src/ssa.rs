use crate::{
    parser::{Expression, SpannedExpr},
    Error, Operator, Primitive,
};
use intaglio::Symbol;
use std::collections::BTreeMap;

pub type Scope = BTreeMap<Symbol, Binding>;
pub type Scopes = Vec<BTreeMap<Symbol, Binding>>;

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Binding(usize);

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Bind(Primitive),
    Add(Binding, Binding),
    Sub(Binding, Binding),
}

pub fn translate(expr: &SpannedExpr) -> Result<(Vec<Instruction>, Scopes), Error> {
    let mut nodes = Vec::new();
    let mut scopes = vec![Scope::new()];

    bind_expr(expr, &mut nodes, &mut scopes).map(|_| (nodes, scopes))
}

fn bind_push(nodes: &mut Vec<Instruction>, new_node: Instruction) -> Binding {
    let binding = Binding(nodes.len());
    nodes.push(new_node);

    binding
}

fn bind_expr(
    expr: &SpannedExpr,
    nodes: &mut Vec<Instruction>,
    scopes: &mut Scopes,
) -> Result<Option<Binding>, Error> {
    match &expr.0 {
        Expression::Binary { lhs, op, rhs } => {
            // TODO uninitialized variable error
            let lhs_binding = bind_expr(lhs.as_ref(), nodes, scopes)?.unwrap();
            let rhs_binding = bind_expr(rhs.as_ref(), nodes, scopes)?.unwrap();

            match op {
                Operator::Add => Ok(Some(bind_push(
                    nodes,
                    Instruction::Add(lhs_binding, rhs_binding),
                ))),
                Operator::Sub => Ok(Some(bind_push(
                    nodes,
                    Instruction::Sub(lhs_binding, rhs_binding),
                ))),

                _ => unimplemented!(),
            }
        }

        Expression::Primitive(primitive) => {
            Ok(Some(bind_push(nodes, Instruction::Bind(primitive.clone()))))
        }
        Expression::Identifier(symbol) => match find_var_binding(*symbol, scopes) {
            Some(binding) => Ok(Some(binding)),
            None => Err(Error::undeclared_var(
                expr.1.clone(),
                crate::strings::get_intern_str(*symbol).as_str(),
                Some("ssa_identifier"),
            )),
        },

        Expression::Flow {
            from: expr,
            to: next,
        } => {
            scopes.push(Scope::new());
            bind_expr(expr.as_ref(), nodes, scopes)?;
            if let Some(next) = next {
                bind_expr(next, nodes, scopes)?;
            }

            Ok(None)
        }

        Expression::Control { exprs } => {
            for expr in exprs {
                bind_expr(expr, nodes, scopes)?;
            }

            Ok(None)
        }

        expr => unimplemented!("no SSA path: {:?}", expr),
    }
}

fn find_var_binding(symbol: Symbol, scopes: &mut Scopes) -> Option<Binding> {
    scopes.iter().rev().find_map(|scope| scope.get(&symbol).copied())
}
