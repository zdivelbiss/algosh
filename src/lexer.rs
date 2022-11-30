use logos::{Lexer, Logos};

#[derive(Logos, Debug, Clone, PartialEq)]
pub enum Token {
    #[regex(r"[\s]+", logos::skip)]
    Discard,

    #[token("{@")]
    LoopOpen,
    #[token("@}")]
    LoopClose,

    #[token("{")]
    BlockOpen,
    #[token("}")]
    BlockClose,

    #[token("if")]
    StartCondition,
    #[token("else if")]
    NextCondition,
    #[token("else")]
    EndCondition,

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
    #[regex(r"[\d]+", |lex| lex.slice().parse())]
    Integer(isize),

    #[regex(r"[\w]+", match_identifier, priority = 2)]
    Identifier(String),

    #[error]
    Unknown,
}

fn trim_string(lexer: &mut Lexer<Token>) -> Option<String> {
    Some(
        lexer
            .slice()
            .trim()
            .trim_start_matches('$')
            .trim_start_matches('"')
            .trim_end_matches('"')
            .to_string(),
    )
}

fn match_neg_integer(lexer: &mut Lexer<Token>) -> Option<isize> {
    isize::from_str_radix(lexer.slice().replace('!', "-").as_str(), 10).ok()
}

fn match_identifier(lexer: &mut Lexer<Token>) -> Option<String> {
    let identifier = lexer.slice();
    if !identifier.starts_with(|ch: char| ch.is_numeric()) {
        Some(identifier.to_string())
    } else {
        None
    }
}

pub fn parse(string: &str) -> logos::Lexer<Token> {
    Token::lexer(string)
}
