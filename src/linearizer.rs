use crate::parser::{Expression, Parser};



pub struct Linearizer {
    exprs: Vec<Expression>,
}

impl Linearizer {
    pub fn from_parser(parser: Parser) -> Self {
        Self {
            exprs: parser.collect(),
        }
    }

    pub fn linearize(&mut self) ->
}
