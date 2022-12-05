use crate::parser::Parser;

pub struct Minifier<'a> {
    parser: Parser<'a>,
}


impl Iterator for Minifier<'_>{
    type Item = ();

    fn next(&mut self) -> Option<Self::Item> {
        let expr = self.parser.next()?;

        
    }
}