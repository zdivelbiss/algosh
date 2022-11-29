use std::iter::Peekable;

use crate::lexer::Token;

enum Type {
    Binary,
    Conditional,
}

struct Expression {
    ty: Type,
    next: Box<Expression>,
}

pub struct Parser {
    tokens: Box<dyn Iterator<Item = Token>>,
}

impl Parser {
    #[inline]
    fn next_token(&mut self) -> Option<Token> {
        self.tokens.next()
    }
}

impl Iterator for Parser {
    type Item = Expression;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token()? {
            Token::         FXx
        }
    }
}
