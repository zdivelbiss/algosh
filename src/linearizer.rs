use crate::parser::{Parser, Expression};

enum LinearNode {

}

pub struct Linearizer<'a> {
    parser: Parser<'a>,
}


impl Iterator for Linearizer<'_>{
    type Item = ();

    fn next(&mut self) -> Option<Self::Item> {
        let mut expr = self.parser.next()?;
        
        match &expr {
            Expression::Value { kind, next_expr } => {
                
            }
        }

    }
}