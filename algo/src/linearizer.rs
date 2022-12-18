use crate::{parser::HeapExpr, strings::Symbol, Primitive};

enum Linearal {
    Primitive(Primitive),
    Variable(Symbol),

    Add(Box<Self>, Box<Self>),
}

fn linearize(ast: &mut Vec<HeapExpr>) -> Result<Vec<Linearal>, Error> {
    let root_expr = ast.iter().
}

fn linearize_expr(expr: &HeapExpr) -> Linearal {}
