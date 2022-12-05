mod expr;
pub use expr::*;

use crate::lexer::{Token, TokenKind};

enum ParserError {
    NoMoreTokens,

    ExpectedToken {
        expected: TokenKind,
        found: TokenKind,
    },


}

pub struct Parser<'a> {
    tokens: crate::lexer::LexerIterator<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: crate::lexer::LexerIterator<'a>) -> Self {
        Self { tokens }
    }

    fn peek(&mut self) -> Option<&TokenKind> {
        self.tokens.peek().map(Token::kind)
    }

    fn expect(&mut self, kind: &TokenKind) {
        let peek = self.peek();
        if peek.contains(&kind) {
            self.discard();
        } else {
            self.throw(format!("expected token: {:?}; found token: {:?}", kind, peek).as_str());
        }
    }

    fn discard(&mut self) {
        self.tokens.next().expect("cannot discard no tokens");
    }

    fn throw(&mut self, msg: &str) -> ! {
        let token_src = match self.tokens.next() {
            Some(t) => self.tokens.find_token(&t),
            None => None,
        };
        crate::throw_error(msg, Some(self.tokens.src()), token_src)
    }
}

impl Iterator for Parser<'_> {
    type Item = Expression;

    fn next(&mut self) -> Option<Self::Item> {
        // Expression::try_box_from(self).ok()
    }
}
