use intaglio::Symbol;
use logos::{Lexer, Logos};

#[derive(Logos, Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenKind {
    #[regex(r"[\s]+", logos::skip)]
    Discard,

    #[regex(r"#\[.]?\]", |lex| lex.slice().parse())]
    Preprocess(String),

    #[token(";")]
    StatementEnd,
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
    lexer.slice().replace('!', "-").as_str().parse().ok()
}
