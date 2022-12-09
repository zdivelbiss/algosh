use chumsky::Stream;
use intaglio::Symbol;
use logos::{Lexer, Logos};

#[derive(Logos, Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenKind {
    #[regex(r"[\s]+", logos::skip)]
    #[regex(r"//.*\n", logos::skip)]
    #[regex(r"/\*[\S\s]*\*/", logos::skip)]
    Discard,

    #[regex(r"#\[.]?\]", |lex| lex.slice().parse())]
    Preprocess(String),

    #[token(";")]
    Terminator,
    #[token(",")]
    Separator,
    #[token("?")]
    Ternary,

    #[token("(")]
    GroupOpen,
    #[token(")")]
    GroupClose,
    #[token("{")]
    TupleOpen,
    #[token("}")]
    TupleClose,
    #[token("[")]
    ArrayOpen,
    #[token("]")]
    ArrayClose,

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
    #[token("=>")]
    Insert,

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

    #[token("||")]
    OrCircuit,
    #[token("&&")]
    AndCircuit,
    #[token("&")]
    And,

    #[token("loop")]
    Loop,
    #[token("exit")]
    Exit,

    #[token("Int")]
    TypeInt,
    #[token("Bool")]
    TypeBool,
    #[token("String")]
    TypeString,

    #[regex(r"![\d]+", parse_neg_integer)]
    #[regex(r"[\d]+", |lex| lex.slice().parse(), priority = 2)]
    Integer(isize),
    #[regex(r"true|false", |lex| lex.slice().parse())]
    Boolean(bool),
    #[regex(r#""[^"\\]*(?:\\.[^"\\]*)*""#, trim_and_cache)]
    String(Symbol),

    #[regex(r#"\$"[\w]+""#, trim_and_cache)]
    EnvVar(Symbol),
    #[regex(r"\$[\w]+", trim_and_cache)]
    EnvCmd(Symbol),

    #[regex(r"[A-Za-z_][\w]*", trim_and_cache)]
    Symbol(Symbol),

    #[error]
    Unknown,
}

fn trim_and_cache(lexer: &mut Lexer<TokenKind>) -> Symbol {
    crate::strings::intern_str(
        lexer
            .slice()
            .trim_start_matches('$')
            .trim_start_matches('"')
            .trim_end_matches('"'),
    )
}

fn parse_neg_integer(lexer: &mut Lexer<TokenKind>) -> Option<isize> {
    lexer.slice().replace('!', "-").as_str().parse().ok()
}

pub type Token = (TokenKind, logos::Span);

pub struct TokenSource<'a> {
    pub src: &'a str,
    pub row: usize,
    pub col: usize,
}

struct Tokenizer<'a>(logos::SpannedIter<'a, TokenKind>);
impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let (kind, span) = self.0.next()?;

        Some((kind, span))
    }
}

pub struct TokenIterator<'a> {
    src: &'a str,
    lexer: std::iter::Peekable<Tokenizer<'a>>,
}

impl TokenIterator<'_> {
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

impl Iterator for TokenIterator<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.next()
    }
}

pub type Span = core::ops::Range<usize>;

impl<'a> From<TokenIterator<'a>> for Stream<'a, TokenKind, Span, TokenIterator<'a>> {
    fn from(lexer: TokenIterator<'a>) -> Self {
        #[allow(clippy::range_plus_one)]
        Self::from_iter(lexer.src().len()..(lexer.src().len() + 1), lexer)
    }
}

pub fn lexer(input: &str) -> TokenIterator {
    TokenIterator {
        src: input,
        lexer: Tokenizer(TokenKind::lexer(input).spanned()).peekable(),
    }
}
