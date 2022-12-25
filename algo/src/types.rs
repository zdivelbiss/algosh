///! Module defining everything related to the Algo type system.
use crate::strings::Symbol;

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