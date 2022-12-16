use crate::{
    parser::{Expression, HeapExpr, SpannedExpr},
    Error, Operator, Primitive,
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
    expr: &SpannedExpr,
    nodes: &mut Vec<Instruction>,
    scopes: &mut Vec<Scope>,
) -> Result<Option<Binding>, Error> {
    match &expr.0 {
        Expression::Binary { lhs, op, rhs } => {
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
            Ok(Some(bind_push(nodes, Instruction::Bind(*primitive))))
        }
        Expression::Identifier(symbol) => match find_var_binding(symbol, scopes) {
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

        Expression::VarDef { name: _, expr } => {
            // TODO bind it.... somehow?
            bind_expr(expr.as_ref(), nodes, scopes)
        }

        expr => unimplemented!("{:?}", expr),
    }
}

fn find_var_binding(symbol: &Symbol, scopes: &mut Vec<Scope>) -> Option<Binding> {
    scopes
        .iter()
        .rev()
        .find_map(|scope| scope.get(symbol).cloned())
}
