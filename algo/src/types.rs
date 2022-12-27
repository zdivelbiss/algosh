use std::collections::BTreeMap;

///! Module defining everything related to the Algo type system.
use crate::{
    interned,
    parser::{Expression, ExpressionKind},
    strings::Symbol,
    Error, Operator,
};

// #[derive(Debug, Clone, PartialEq, Eq)]
// pub enum Type {
//     Unit, // is `()`
//     Int,  // is `isize`
//     UInt, // is `usize`
//     Bool, // is `bool`

//     Tuple(Vec<(Symbol, Self)>),
//     Array { ty: Box<Self>, len: Option<usize> },

//     Checked(Symbol),
// }

static TYPES_TMP: Types = Types::new();

pub struct Type {
    ops: Box<[Operator]>,
}

pub struct Types(BTreeMap<Symbol, Type>);

impl Types {
    const fn new() -> Self {
        Self(BTreeMap::new())
    }
}

#[derive(Debug)]
pub struct TypedExpression {
    ty_id: Symbol,
    expr: Expression,
}

pub fn type_exprs(
    types: &Types,
    exprs: Vec<Expression>,
) -> Result<Vec<TypedExpression>, Vec<Error>> {
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
fn type_expr(types: &Types, expr: &Expression) -> Result<Type, Error> {
    match expr.kind() {
        ExpressionKind::Error => todo!(),

        ExpressionKind::Unit => Ok(types[&interned!("Unit")]),
        ExpressionKind::Int(_) => Ok(types[&interned!("Int")]),
        ExpressionKind::UInt(_) => Ok(types[&interned!("UInt")]),
        ExpressionKind::Bool(_) => Ok(types[&interned!("Bool")]),

        ExpressionKind::Binary { lhs, op, rhs } => {
            let lhs_ty = type_expr(&*lhs)?;
            let rhs_ty = type_expr(&*rhs)?;
        }

        ExpressionKind::Compound(exprs) => todo!(),

        ExpressionKind::ControlFlow { from, into } => type_expr(&*from)
            .and_then(|from_ty| into.as_ref().map_or(Ok(from_ty), |into| type_expr(&*into))),

        ExpressionKind::Array(_) | ExpressionKind::Tuple(_) | ExpressionKind::Identifier(_) => {
            todo!()
        }

        ExpressionKind::VarDef { name, ty, expr } => todo!(),
        ExpressionKind::TypeDef { name, ty } => todo!(),
    }
}
