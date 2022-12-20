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

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Binding(usize);

impl Binding {
    #[inline]
    pub const fn as_usize(&self) -> usize {
        self.0
    }
}

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

                    expr => {
                        if !expr.is_def() {
                            tles.push((expr, span))
                        } else {
                            panic!("failed to stratify all def expressions")
                        }
                    }
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
                            span.clone(),
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
        Ok(_) => {
            nodes.shrink_to_fit();
            Ok(nodes)
        }

        Err(err) => Err(vec![err]),
    }
}

fn bind_push(nodes: &mut Vec<Node>, new_node: Node) -> Binding {
    let binding = Binding(nodes.len());
    nodes.push(new_node);

    binding
}

fn bind_expr(
    expr: &SpannedExpr,
    nodes: &mut Vec<Node>,
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
                    Operator::Add => Ok(bind_push(nodes, Node::Add(lhs_binding, rhs_binding))),
                    Operator::Sub => Ok(bind_push(nodes, Node::Sub(lhs_binding, rhs_binding))),

                    _ => unimplemented!(),
                }
            }

            Expression::Primitive(primitive) => Ok(bind_push(nodes, Node::Bind(primitive.clone()))),

            Expression::Identifier(symbol) => match find_binding(symbol, scope) {
                Some(binding) => Ok(binding),
                None => bind_def(symbol, nodes, defs),
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

fn bind_def(name: &Symbol, nodes: &mut Vec<Node>, defs: &mut Defs) -> Result<Binding, Error> {
    let Some((ty, expr)) = defs.remove(name)
                    else {
                        return Err(Error::undeclared_var(
                            0..0, // TODO
                            crate::strings::get_intern_str(*name).as_str(),
                            Some("ssa_identifier"),
                        ));
                    };

    let mut def_scope = Scope::new();
    let params = match ty {
        Type::Unit => bind_push(nodes, Node::Parameter(Type::Unit)),
        Type::Tuple(params) => params
            .into_iter()
            .map(|(name, ty)| {
                let param_bind = bind_push(nodes, Node::Parameter(ty));
                def_scope.push((name, param_bind));
                param_bind
            })
            .last()
            .unwrap(),

        _ => unimplemented!(),
    };

    bind_expr(&expr, nodes, &mut def_scope, defs)?;
    Ok(bind_push(nodes, Node::Expression(params, def_scope)))
}

fn find_binding(symbol: &Symbol, scopes: &mut Scope) -> Option<Binding> {
    scopes.iter().rev().find_map(|(other, binding)| {
        if symbol.eq(other) {
            Some(*binding)
        } else {
            None
        }
    })
}

#[cfg(test)]
mod tests {
    use crate::{
        interned,
        ssa::{bind_def, stratify_exprs, Binding, Node},
        types::Type,
    };

    #[test]
    fn def_to_bindings() {
        static SCRIPT: &str = "
        var add_one: (a: Int) => a + 1;
        1 => add_one
        ";

        let ast = crate::parser::parse(crate::lexer::lex(SCRIPT)).unwrap();
        let (_, mut defs) = stratify_exprs(ast).unwrap();

        let mut nodes = Vec::new();
        let def = bind_def(&interned!("add_one"), &mut nodes, &mut defs).unwrap();
        assert_eq!(def, Binding(3));
        assert_eq!(
            nodes,
            vec![
                Node::Parameter(Type::Int),
                Node::Bind(crate::Primitive::Int(1)),
                Node::Add(Binding(0), Binding(1)),
                Node::Expression(Binding(0), vec![(interned!("a"), Binding(0))])
            ]
        );
    }
}
