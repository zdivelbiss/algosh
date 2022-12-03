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
    #[token("->")]
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

    #[regex(r#"\$"[\w]+""#, trim_string)]
    EnvVar(String),

    #[regex(r"\$[\w]+", trim_string)]
    EnvCommand(String),

    #[regex(r#""[^"\\]*(?:\\.[^"\\]*)*""#, trim_string)]
    String(String),

    #[regex(r"![\d]+", parse_neg_integer)]
    #[regex(r"[\d]+", |lex| lex.slice().parse(), priority = 2)]
    Integer(isize),

    #[regex(r"[\w]+", parse_identifier)]
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

fn parse_neg_integer(lexer: &mut Lexer<TokenKind>) -> Option<isize> {
    isize::from_str_radix(lexer.slice().replace('!', "-").as_str(), 10).ok()
}

fn parse_identifier(lexer: &mut Lexer<TokenKind>) -> Option<String> {
    let identifier = lexer.slice();
    if !identifier.starts_with(|ch: char| ch.is_numeric()) {
        Some(identifier.to_string())
    } else {
        None
    }
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
}

impl std::fmt::Debug for Token {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.debug_tuple("Token").field(self.kind()).finish()
    }
}