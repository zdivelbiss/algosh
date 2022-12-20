mod optimizer;
pub use optimizer::*;

mod nodes;
pub use nodes::*;

use crate::{
    parser::{Expression, HeapExpr, SpannedExpr},
    strings::Symbol,
    types::Type,
    Error, Operator,
};
use std::collections::BTreeMap;

type Scope = Vec<(Symbol, Binding)>;
type Defs = BTreeMap<Symbol, (Type, SpannedExpr)>;

fn stratify_exprs(ast: Vec<HeapExpr>) -> Result<(SpannedExpr, Defs), Vec<Error>> {
    // TLE, or `Top Level Expression`
    let (defs, tles) =
        ast.into_iter()
            .fold((Defs::new(), Vec::new()), |(mut defs, mut tles), expr| {
                let (expr, span) = *expr;
                match expr {
                    Expression::VarDef { name, in_ty, expr } => {
                        assert!(defs.insert(name, (in_ty, *expr)).is_none());
                    }

                    expr if !expr.is_def() => tles.push((expr, span)),

                    _ => panic!("failed to stratify all def expressions"),
                }

                (defs, tles)
            });

    let mut tles_iter = tles.into_iter();
    let tle = {
        match tles_iter.len() {
            1 => tles_iter.next().unwrap(),
            0 => return Err(vec![Error::no_top_level_expr()]),
            _ => {
                return Err(tles_iter
                    .map(|(_, span)| {
                        Error::general(
                            span,
                            "cannot have multiple top-level expressions",
                            Some("parse_input.tle_check"),
                        )
                    })
                    .collect())
            }
        }
    };

    Ok((tle, defs))
}

pub fn translate(ast: Vec<HeapExpr>) -> Result<Nodes, Vec<Error>> {
    let (tle, mut defs) = stratify_exprs(ast)?;

    let mut nodes = Nodes::new();
    let mut scope = Scope::new();

    match bind_expr(&tle, &mut nodes, &mut scope, &mut defs) {
        Ok(_) => Ok(nodes),

        Err(err) => Err(vec![err]),
    }
}

fn bind_expr(
    expr: &SpannedExpr,
    nodes: &mut Nodes,
    scope: &mut Scope,
    defs: &mut Defs,
) -> Result<Binding, Error> {
    // mark scope start
    let scope_mark = scope.len();

    let result = {
        match &expr.0 {
            Expression::Binary { lhs, op, rhs } => {
                // TODO uninitialized variable error
                let lhs_binding = bind_expr(lhs.as_ref(), nodes, scope, defs)?;
                let rhs_binding = bind_expr(rhs.as_ref(), nodes, scope, defs)?;

                match op {
                    Operator::Add => Ok({
                        let new_node = Node::Add(lhs_binding, rhs_binding);
                        nodes.bind_push(new_node)
                    }),
                    Operator::Sub => Ok({
                        let new_node = Node::Sub(lhs_binding, rhs_binding);
                        nodes.bind_push(new_node)
                    }),

                    _ => unimplemented!(),
                }
            }

            Expression::Primitive(primitive) => Ok({
                let new_node = Node::Bind(*primitive);
                nodes.bind_push(new_node)
            }),

            Expression::Identifier(symbol) => match find_binding(*symbol, scope) {
                Some(binding) => Ok(binding),
                None => bind_def(*symbol, nodes, defs),
            },

            Expression::Flow {
                from: expr,
                to: next,
            } => {
                let bind_from = bind_expr(expr.as_ref(), nodes, scope, defs)?;
                let bind_to = next
                    .as_ref()
                    .map(|expr| bind_expr(expr.as_ref(), nodes, scope, defs));

                bind_to.unwrap_or(Ok(bind_from))
            }

            Expression::Control { exprs } => exprs
                .iter()
                .map(|expr| bind_expr(expr, nodes, scope, defs))
                .last()
                .unwrap(),

            expr => unimplemented!("no SSA path: {:?}", expr),
        }
    };
    // return to scope start
    scope.drain(scope_mark..);

    result
}

fn bind_def(name: Symbol, nodes: &mut Nodes, defs: &mut Defs) -> Result<Binding, Error> {
    let Some((ty, expr)) = defs.remove(&name)
                    else {
                        return Err(Error::undeclared_var(
                            0..0, // TODO
                            crate::strings::get_intern_str(name).as_str(),
                            Some("ssa_identifier"),
                        ));
                    };

    let mut def_scope = Scope::new();
    let params = match ty {
        Type::Unit => {
            let new_node = Node::Parameter(Type::Unit);
            nodes.bind_push(new_node)
        }
        Type::Tuple(params) => params
            .into_iter()
            .map(|(name, ty)| {
                let param_bind = {
                    let new_node = Node::Parameter(ty);
                    nodes.bind_push(new_node)
                };
                def_scope.push((name, param_bind.clone()));

                param_bind
            })
            .last()
            .unwrap(),

        _ => unimplemented!(),
    };

    bind_expr(&expr, nodes, &mut def_scope, defs)?;
    Ok({
        let new_node = Node::Expression(params, def_scope);
        nodes.bind_push(new_node)
    })
}

fn find_binding(symbol: Symbol, scopes: &mut Scope) -> Option<Binding> {
    scopes.iter().rev().find_map(|(other, binding)| {
        if symbol.eq(other) {
            Some(binding.clone())
        } else {
            None
        }
    })
}

#[cfg(test)]
mod tests {
    // use crate::{
    //     interned,
    //     ssa::{bind_def, stratify_exprs, Binding, Node},
    //     types::Type,
    // };

    // #[test]
    // fn def_to_bindings() {
    //     static SCRIPT: &str = "
    //     var add_one: (a: Int) => a + 1;
    //     1 => add_one
    //     ";

    //     let ast = crate::parser::parse(crate::lexer::lex(SCRIPT)).unwrap();
    //     let (_, mut defs) = stratify_exprs(ast).unwrap();

    //     let mut nodes = Vec::new();
    //     let def = bind_def(&interned!("add_one"), &mut nodes, &mut defs).unwrap();
    //     assert_eq!(def, Binding::generate(3));
    //     assert_eq!(
    //         nodes,
    //         vec![
    //             Node::Parameter(Type::Int),
    //             Node::Bind(crate::Primitive::Int(1)),
    //             Node::Add(Binding::generate(0), Binding::generate(1)),
    //             Node::Expression(
    //                 Binding::generate(0),
    //                 vec![(interned!("a"), Binding::generate(0))]
    //             )
    //         ]
    //     );
    // }
}
