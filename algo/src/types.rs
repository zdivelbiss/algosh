///! Module defining everything related to the Algo type system.
use crate::{
    parser::{Expression, ExpressionKind},
    strings::Symbol,
    Error,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Unit, // is `()`
    Int,  // is `isize`
    UInt, // is `usize`
    Bool, // is `bool`

    Tuple(Vec<(Symbol, Self)>),
    Array { ty: Box<Self>, len: Option<usize> },

    Expression { input: Box<Self>, output: Box<Self> },

    Checked(Symbol),
}

#[derive(Debug)]
pub struct TypedExpression {
    ty: Type,
    expr: Expression,
}

pub fn type_exprs(exprs: Vec<Expression>) -> Result<Vec<TypedExpression>, Vec<Error>> {
    let (exprs, errs) =
        exprs
            .into_iter()
            .fold((Vec::new(), Vec::new()), |(mut exprs, mut errs), expr| {
                match type_expr(&expr) {
                    Ok(ty) => exprs.push(TypedExpression { ty, expr }),
                    Err(err) => errs.push(err),
                }

                (exprs, errs)
            });

    if errs.is_empty() {
        Ok(exprs)
    } else {
        Err(errs)
    }
}

#[allow(unused_variables)]
fn type_expr(expr: &Expression) -> Result<Type, Error> {
    match expr.kind() {
        ExpressionKind::Error => todo!(),
        ExpressionKind::Unit => Ok(Type::Unit),
        ExpressionKind::Int(_) => Ok(Type::Int),
        ExpressionKind::UInt(_) => Ok(Type::UInt),
        ExpressionKind::Bool(_) => Ok(Type::Bool),

        ExpressionKind::Binary { lhs, op, rhs } => {
            let lhs_ty = type_expr(&*lhs)?;
            let rhs_ty = type_expr(&*rhs)?;

            if lhs_ty == rhs_ty {
                Ok(lhs_ty)
            } else {
                Err(Error::general(
                    lhs.span().start..rhs.span().end,
                    "type mistmatch in binary expression",
                    Some("crate::types::type_exprs.type_expr.binary"),
                ))
            }
        }

        ExpressionKind::Compound(exprs) => {
            exprs.iter().try_fold(Type::Unit, |_, expr| type_expr(expr))
        }

        ExpressionKind::ControlFlow { from, into } => type_expr(&*from)
            .and_then(|from_ty| into.as_ref().map_or(Ok(from_ty), |into| type_expr(&*into))),

        ExpressionKind::Array(_) | ExpressionKind::Tuple(_) | ExpressionKind::Identifier(_) => {
            todo!()
        }

        ExpressionKind::Var { name, ty, expr } => todo!(),
        ExpressionKind::Type { name, ty } => todo!(),
    }
}
