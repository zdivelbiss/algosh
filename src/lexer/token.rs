use intaglio::Symbol;
use logos::{Lexer, Logos, Span};

#[derive(Logos, Debug, Clone, PartialEq)]
pub enum TokenKind {
    #[regex(r"[\s]+", logos::skip)]
    Discard,

    #[token("|")]
    ArgumentCell,
    #[token(";")]
    Terminator,
    #[token(",")]
    Separator,
    #[token("?")]
    Ternary,
    #[token("=>")]
    Flow,

    #[token("{")]
    BlockOpen,
    #[token("}")]
    BlockClose,

    #[token("(")]
    TupleOpen,
    #[token(")")]
    TupleClose,

    #[token("[")]
    ArrayOpen,
    #[token("]")]
    ArrayClose,

    #[token("const")]
    ConstEval,
    #[token("lazy")]
    LazyEval,
    #[token("eager")]
    EagerEval,

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

    #[regex(r"true|false", |lex| lex.slice().parse())]
    Boolean(bool),

    #[regex(r#"\$"[\w]+""#, trim_and_cache)]
    EnvVar(Symbol),

    #[regex(r"\$[\w]+", trim_and_cache)]
    EnvCommand(Symbol),

    #[regex(r#""[^"\\]*(?:\\.[^"\\]*)*""#, trim_and_cache)]
    String(Symbol),

    #[regex(r"![\d]+", parse_neg_integer)]
    #[regex(r"[\d]+", |lex| lex.slice().parse(), priority = 2)]
    Integer(isize),

    #[regex(r"[A-Za-z_][\w]*", trim_and_cache)]
    Identifier(Symbol),

    #[error]
    Unknown,
}

fn trim_and_cache(lexer: &mut Lexer<TokenKind>) -> Option<Symbol> {
    Some(crate::strings::intern_str(
        lexer
            .slice()
            .trim_start_matches('$')
            .trim_start_matches('"')
            .trim_end_matches('"'),
    ))
}

fn parse_neg_integer(lexer: &mut Lexer<TokenKind>) -> Option<isize> {
    isize::from_str_radix(lexer.slice().replace('!', "-").as_str(), 10).ok()
}

#[derive(Clone)]
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
    pub const fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    #[inline]
    pub const fn kind(&self) -> &TokenKind {
        &self.kind
    }

    #[inline]
    pub const fn span(&self) -> &Span {
        &self.span
    }

    #[inline]
    pub const fn take_kind(self) -> TokenKind {
        self.kind
    }
}

impl std::fmt::Debug for Token {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.debug_tuple("Token").field(self.kind()).finish()
    }
}
