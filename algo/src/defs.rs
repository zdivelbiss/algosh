// use crate::{
//     parser::{Atom, Expression},
//     types::Type,
//     Error, Span,
// };
// use std::collections::BTreeMap;

// pub struct DefInfo {
//     in_ty: Type,
//     expr: Expression,
//     span: Span,
// }

// pub type Defs = BTreeMap<intaglio::Symbol, DefInfo>;

// pub fn parse(ast: Vec<Atom>) -> Result<(Atom, Defs), Vec<Error>> {
//     let (tles, defs): (Vec<_>, Vec<_>) = ast.into_iter().partition(|atom| atom.name.is_none());

//     let mut tles = tles.into_iter();
//     let tle = match tles.len() {
//         0 => return Err(vec![Error::no_top_level_expr()]),
//         1 => tles.next().unwrap(),
//         _ => {
//             return Err(tles
//                 .map(|atom| {
//                     Error::general(
//                         atom.span,
//                         "cannot have multiple top-level expressions",
//                         Some("crate::defs::parse.tle_check"),
//                     )
//                 })
//                 .collect())
//         }
//     };

//     let defs: Defs = defs
//         .into_iter()
//         .map(|atom| {
//             (
//                 atom.name.unwrap(),
//                 DefInfo {
//                     in_ty: atom.in_ty,
//                     expr: atom.expr,
//                     span: atom.span,
//                 },
//             )
//         })
//         .collect();

//     Ok((tle, defs))
// }
