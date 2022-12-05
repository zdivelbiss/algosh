mod expr;
pub use expr::*;

use crate::lexer::{Token, TokenKind};

#[derive(Debug)]
pub enum ParserError {
    NoMoreTokens,

    Discard,

    ExpectedToken {
        expected: TokenKind,
        found: Option<TokenKind>,
    },

    FoundMsg {
        found: Option<TokenKind>,
        msg: String,
    },

    General(String),
}

pub struct Parser<'a> {
    tokens: crate::lexer::LexerIterator<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: crate::lexer::LexerIterator<'a>) -> Self {
        Self { tokens }
    }

    pub(crate) fn peek(&mut self) -> Option<&TokenKind> {
        self.tokens.peek().map(Token::kind)
    }

    pub(crate) fn expect(&mut self, expect: &TokenKind) -> Result<(), ParserError> {
        match self.peek() {
            Some(kind) if kind.eq(expect) => {
                self.discard();

                Ok(())
            }

            kind => Err(ParserError::ExpectedToken {
                expected: expect.clone(),
                found: kind.cloned(),
            }),
        }
    }

    pub(crate) fn expect_with<T>(
        &mut self,
        expect_fn: impl FnOnce(&TokenKind) -> Result<T, ParserError>,
    ) -> Result<T, ParserError> {
        match self.peek() {
            Some(kind) => {
                let result = expect_fn(kind);
                if result.is_ok() {
                    self.discard();
                }

                result
            }

            None => Err(ParserError::NoMoreTokens),
        }
    }

    pub(crate) fn discard(&mut self) {
        self.tokens.next().expect("cannot discard no tokens");
    }

    pub(crate) fn throw(&mut self, msg: &str) -> ! {
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
        match Expression::try_from(self) {
            Ok(expr) => Some(expr),

            Err(ParserError::NoMoreTokens) => None,

            Err(err) => {
                // FIXME: make this output real errors
                panic!("{:?}", err)
            }
        }
    }
}
