///! Module defining everything related to the Algo type system.
use crate::{defs::Defs, strings::Symbol, Operator};

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

pub enum TypedExpression {
    Unit,
    Int(isize),
    UInt(usize),
    Bool(bool),

    Binary {
        out_ty: Type,
        lhs: Box<Self>,
        op: Operator,
        rhs: Box<Self>,
    },

    ControlFlow {
        from_ty: Type,
        to_ty: Type,
        from: Box<Self>,
        to: Box<Self>,
    },
}

pub fn type_tag_defs(defs: Defs) -> () {}
