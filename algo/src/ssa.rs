use chumsky::chain::Chain;

use crate::{
    parser::{Expression, SpannedExpr},
    strings::Symbol,
    types::Type,
    Error, Operator, Primitive,
};
use std::collections::BTreeMap;

type Scope = BTreeMap<Symbol, Binding>;
type Scopes = Vec<BTreeMap<Symbol, Binding>>;

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Binding(usize);

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Bind(Primitive),
    Add(Binding, Binding),
    Sub(Binding, Binding),
    Jump(Binding),
    Call(Binding),
}

pub fn translate(
    tle: &SpannedExpr,
    defs: Vec<crate::parser::HeapExpr>,
) -> Result<(Vec<Instruction>, Scopes), Error> {
    let mut nodes = Vec::new();
    let mut scopes = vec![Scope::new()];
    let defs = defs
        .into_iter()
        .map(|expr| {
            let (expr, span) = *expr;
            let Expression::VarDef { name, in_ty, expr } = expr
            else { panic!("invalid definition type"); };

            (name, *expr)
        })
        .collect::<BTreeMap<Symbol, SpannedExpr>>();

    bind_expr(tle, &mut nodes, &mut scopes, &defs).map(|_| (nodes, scopes))
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
    defs: &BTreeMap<Symbol, SpannedExpr>,
) -> Result<Binding, Error> {
    match &expr.0 {
        Expression::Binary { lhs, op, rhs } => {
            // TODO uninitialized variable error
            let lhs_binding = bind_expr(lhs.as_ref(), nodes, scopes, defs)?;
            let rhs_binding = bind_expr(rhs.as_ref(), nodes, scopes, defs)?;

            match op {
                Operator::Add => Ok(bind_push(nodes, Instruction::Add(lhs_binding, rhs_binding))),
                Operator::Sub => Ok(bind_push(nodes, Instruction::Sub(lhs_binding, rhs_binding))),

                _ => unimplemented!(),
            }
        }

        Expression::Primitive(primitive) => {
            Ok(bind_push(nodes, Instruction::Bind(primitive.clone())))
        }

        Expression::Identifier(symbol) => {
            match scopes
                .iter()
                .rev()
                .find_map(|scope| scope.get(symbol).copied())
            {
                Some(binding) => Ok(binding),

                None => {
                    let Some(expr) = defs.get(symbol)
                    else {
                        return Err(Error::undeclared_var(
                            expr.1.clone(),
                            crate::strings::get_intern_str(*symbol).as_str(),
                            Some("ssa_identifier"),
                        ));
                    };

                    bind_expr(expr, nodes, scopes, defs)
                }
            }
        }

        Expression::Flow {
            from: expr,
            to: next,
        } => {
            scopes.push(Scope::new());
            bind_expr(expr.as_ref(), nodes, scopes, defs)?;
            if let Some(next) = next {
                bind_expr(next, nodes, scopes, defs)?;
            }

            todo!()
        }

        Expression::Control { exprs } => {
            for expr in exprs {
                bind_expr(expr, nodes, scopes, defs)?;
            }

            todo!()
        }

        expr => unimplemented!("no SSA path: {:?}", expr),
    }
}
