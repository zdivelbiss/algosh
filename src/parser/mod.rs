mod node;
pub use node::*;

use crate::lexer::{Token, TokenKind};

pub struct Parser<'a> {
    tokens: crate::lexer::LexerIterator<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: crate::lexer::LexerIterator<'a>) -> Self {
        Self { tokens }
    }

    #[inline]
    fn next_token(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    #[inline]
    fn peek_token(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    #[inline]
    fn discard_token(&mut self) {
        self.next_token().expect("cannot discard no tokens");
    }

    fn with_next<T>(&mut self, if_func: impl FnOnce(&Token) -> Option<T>) -> Option<T> {
        let peek = self.peek_token()?;
        let result = if_func(peek);
        if result.is_some() {
            self.discard_token();
        }

        result
    }

    fn next_eq(&mut self, kind: &TokenKind) -> Option<bool> {
        let peek = self.peek_token()?;
        let eq = peek.kind().eq(kind);
        if eq {
            self.discard_token();
        }

        Some(eq)
    }

    fn throw(&mut self, msg: &str) -> ! {
        let peek_token = self
            .peek_token()
            .map(Token::clone)
            .unwrap_or(Token::new(TokenKind::Unknown, 0..0));

        crate::throw_error(self.tokens.src(), msg, self.tokens.find_token(&peek_token))
    }
}

impl Iterator for Parser<'_> {
    type Item = Node;

    fn next(&mut self) -> Option<Self::Item> {
        Node::try_from(self).ok()
    }
}
