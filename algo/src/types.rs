use crate::strings::Symbol;

///! Module defining everything related to the Algo type system.

#[derive(Debug, PartialEq)]
pub enum Type {
    Unit, // is `()`
    Int,  // is `isize`
    UInt, // is `usize`
    Bool, // is `bool`

    Tuple(Vec<(Option<Symbol>, Self)>),
    Array { ty: Box<Self>, len: Option<usize> },

    Expression { input: Box<Self>, output: Box<Self> },

    Checked(Symbol),
}
