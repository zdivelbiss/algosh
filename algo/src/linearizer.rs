use crate::{Primitive, strings::Symbol, parser::HeapExpr};

enum Linearal {
    Primitive(Primitive),
    Variable(Symbol),

    Add(Box<Self>, Box<Self>),
}

fn linearize(ast: &[HeapExpr])