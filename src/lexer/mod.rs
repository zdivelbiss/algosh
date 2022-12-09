mod token;
pub use token::*;

use chumsky::Stream;
use logos::Logos;

pub type Token = (TokenKind, logos::Span);

pub struct TokenSource<'a> {
    pub src: &'a str,
    pub row: usize,
    pub col: usize,
}

struct LexerTokenizationIterator<'a>(logos::SpannedIter<'a, TokenKind>);
impl<'a> Iterator for LexerTokenizationIterator<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let (kind, span) = self.0.next()?;

        Some((kind, span))
    }
}

pub struct LexerIterator<'a> {
    src: &'a str,
    lexer: std::iter::Peekable<LexerTokenizationIterator<'a>>,
}

impl LexerIterator<'_> {
    pub fn src(&self) -> &str {
        self.src
    }

    pub fn peek(&mut self) -> Option<&Token> {
        self.lexer.peek()
    }

    pub fn find_token(&self, (_, span): &Token) -> Option<TokenSource> {
        let span_str = self.src.get(..span.end)?;
        let (row, end_col) = span_str
            .lines()
            .enumerate()
            .last()
            .map(|(row, line_str)| (row, line_str.chars().count()))?;

        Some(TokenSource {
            src: span_str.get(span.start..)?,
            row,
            col: end_col - span.len(),
        })
    }
}

impl Iterator for LexerIterator<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.next()
    }
}

pub type LexerSpan = core::ops::Range<usize>;

impl<'a> From<LexerIterator<'a>> for Stream<'a, TokenKind, LexerSpan, LexerIterator<'a>> {
    fn from(lexer: LexerIterator<'a>) -> Self {
        Self::from_iter(lexer.src().len()..(lexer.src().len() + 1), lexer)
    }
}

pub fn lexer(input: &str) -> LexerIterator {
    LexerIterator {
        src: input,
        lexer: LexerTokenizationIterator(TokenKind::lexer(input).spanned()).peekable(),
    }
}
