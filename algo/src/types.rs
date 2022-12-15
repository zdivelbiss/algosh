///! Module defining everything related to the Algo type system.

pub enum Type {
    Int,  // is `isize`
    UInt, // is `usize`
    Bool, // is `bool`
    Unit, // is `()`

    Tuple(Vec<Self>),
    Array(Vec<Self>),

    Expression { input: Box<Self>, output: Box<Self> },
}