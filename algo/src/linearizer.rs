use crate::{parser::HeapExpr, strings::Symbol, Error, Primitive};


pub enum Linearal {
    Primitive(Primitive),
    Variable(Symbol),

    Add(Box<Self>, Box<Self>),
}

pub fn linearize(ast: &mut Vec<HeapExpr>) -> Result<Vec<Linearal>, Vec<Error>> {
    let root_expr = {
        let p = |expr: &HeapExpr| !expr.0.is_def();

        let mut iter = ast.iter();
        let Some(root_expr_index) = iter.position(p) else { return Err(vec![Error::no_tld()]); };

        let errs = iter
            .filter(|e| p(e))
            .map(|expr| {
                Error::general(
                    expr.1.clone(),
                    "cannot have multiple top-level expressions",
                    Some("linearize_root_expr"),
                )
            })
            .collect::<Vec<Error>>();

        if !errs.is_empty() {
            return Err(errs);
        }

        ast.remove(root_expr_index)
    };




    Ok(Vec::new())
}

// fn linearize_expr(expr: &HeapExpr) -> Linearal {}
