mod expr;
pub use expr::*;

use crate::lexer::Token;

#[derive(Debug)]
pub enum ParserError {
    UnexpectedToken,

    NoMoreTokens,

    Expected {
        expected: Token,
        found: Option<Token>,
    },

    FoundMsg {
        found: Option<Token>,
        msg: String,
    },

    Msg(String),

    ReplaceThisError,
}

pub struct Parser<'a> {
    tokens: crate::lexer::LexerIterator<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: crate::lexer::LexerIterator<'a>) -> Self {
        Self { tokens }
    }

    pub(crate) fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    pub(crate) fn expect(&mut self, expect: &Token) -> Result<(), ParserError> {
        match self.peek() {
            Some(kind) if kind.eq(expect) => {
                self.advance();

                Ok(())
            }

            kind => Err(ParserError::Expected {
                expected: expect.clone(),
                found: kind.cloned(),
            }),
        }
    }

    pub(crate) fn expect_with<T>(
        &mut self,
        expect_fn: impl FnOnce(&Token) -> Result<T, ParserError>,
    ) -> Result<T, ParserError> {
        match self.peek() {
            Some(token) => {
                let result = expect_fn(token);
                if result.is_ok() {
                    self.advance();
                }

                result
            }

            None => Err(ParserError::ReplaceThisError),
        }
    }

    pub(crate) fn advance(&mut self) {
        self.tokens.next().expect("cannot discard no tokens");
    }

    #[allow(dead_code)]
    pub(crate) fn throw(&mut self, msg: &str) -> ! {
        let token_src = match self.tokens.next() {
            Some(t) => self.tokens.find_token(&t),
            None => None,
        };
        crate::throw_error(msg, Some(self.tokens.src()), token_src)
    }
}

impl Iterator for Parser<'_> {
    type Item = expr::HeapExpr;

    fn next(&mut self) -> Option<Self::Item> {
        if self.tokens.peek().is_some() {
            match expr::parse_expr(self) {
                Ok(expr) => Some(expr),

                Err(ParserError::ReplaceThisError) => {
                    println!("REPALCE ERROR");
                    None
                }

                Err(err) => {
                    // FIXME: make this output real errors
                    panic!("{:?}", err)
                }
            }
        } else {
            None
        }
    }
}
