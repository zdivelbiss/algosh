mod token;
pub use token::*;

use logos::Logos;

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

        Some(Token::new(kind, span))
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

    pub fn find_token(&self, token: &Token) -> Option<TokenSource> {
        let token_span = token.span();
        let span_str = self.src.get(..token_span.end)?;
        let (row, end_col) = span_str
            .lines()
            .enumerate()
            .last()
            .map(|(row, line_str)| (row, line_str.chars().count()))?;

        Some(TokenSource {
            src: span_str.get(token.span().start..)?,
            row,
            col: end_col - token_span.len(),
        })
    }
}

impl Iterator for LexerIterator<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.next()
    }
}

pub fn lexer(input: &str) -> LexerIterator {
    LexerIterator {
        src: input,
        lexer: LexerTokenizationIterator(TokenKind::lexer(input).spanned()).peekable(),
    }
}
