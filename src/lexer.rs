use logos::{Lexer, Logos, Span};

#[derive(Logos, Debug, Clone, PartialEq)]
pub enum TokenKind {
    #[regex(r"[\s]+", logos::skip)]
    Discard,

    #[token("{")]
    BlockOpen,
    #[token("}")]
    BlockClose,

    #[token("if")]
    StartCondition,
    #[token("else")]
    NextCondition,

    #[token(":+")]
    AddAssign,
    #[token(":-")]
    SubAssign,
    #[token(":*")]
    MulAssign,
    #[token(":/")]
    DivAssign,
    #[token(":>>")]
    ShrAssign,
    #[token(":<<")]
    ShlAssign,
    #[token(":")]
    Assign,

    #[token("+")]
    Add,
    #[token("-")]
    Sub,
    #[token("*")]
    Mul,
    #[token("/")]
    Div,
    #[token(">>")]
    Shr,
    #[token("<<")]
    Shl,
    #[token("^")]
    Xor,

    #[token(">=")]
    GreaterEq,
    #[token(">")]
    Greater,

    #[token("<=")]
    LessEq,
    #[token("<")]
    Less,

    #[token("!=")]
    NegEq,
    #[token("=")]
    Eq,

    #[token("&&")]
    AndCircuit,
    #[token("&")]
    And,

    #[token("||")]
    OrCircuit,
    #[token("|")]
    Or,

    #[token("loop")]
    Loop,
    #[token("exit")]
    Exit,

    #[regex(r"true|false", |lex| lex.slice().parse())]
    Boolean(bool),

    #[regex(r#"\$"[\w]+""#, trim_string)]
    Variable(String),

    #[regex(r"\$[\w]+", trim_string)]
    Command(String),

    #[regex(r#""[^"\\]*(?:\\.[^"\\]*)*""#, trim_string)]
    String(String),

    #[regex(r"![\d]+", match_neg_integer)]
    #[regex(r"[\d]+", |lex| lex.slice().parse(), priority = 2)]
    Integer(isize),

    #[regex(r"[\w]+", match_identifier)]
    Identifier(String),

    #[error]
    Unknown,
}

fn trim_string(lexer: &mut Lexer<TokenKind>) -> Option<String> {
    Some(
        lexer
            .slice()
            .trim_start_matches('$')
            .trim_start_matches('"')
            .trim_end_matches('"')
            .to_string(),
    )
}

fn match_neg_integer(lexer: &mut Lexer<TokenKind>) -> Option<isize> {
    isize::from_str_radix(lexer.slice().replace('!', "-").as_str(), 10).ok()
}

fn match_identifier(lexer: &mut Lexer<TokenKind>) -> Option<String> {
    let identifier = lexer.slice();
    if !identifier.starts_with(|ch: char| ch.is_numeric()) {
        Some(identifier.to_string())
    } else {
        None
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    kind: TokenKind,
    span: Span,
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.kind.eq(&other.kind)
    }
}

impl Token {
    #[inline]
    pub const fn kind(&self) -> &TokenKind {
        &self.kind
    }

    #[inline]
    pub const fn span(&self) -> &Span {
        &self.span
    }
}

pub struct TokenSource<'a> {
    pub src: &'a str,
    pub row: usize,
    pub col: usize,
}

struct LexerTokenizationIterator<'a>(logos::SpannedIter<'a, TokenKind>);
impl Iterator for LexerTokenizationIterator<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let (kind, span) = self.0.next()?;

        Some(Token { kind, span })
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
        let span_start = token.span().start;
        let span_end = token.span().end;
        let span_str = self.src.get(..span_end)?;
        let (row, col) = span_str.lines().fold((0usize, 0usize), |(row, _), line| {
            (row + 1, line.chars().count())
        });

        Some(TokenSource {
            src: span_str.get(token.span().start..)?,
            row,
            col: col - (span_end - span_start),
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
