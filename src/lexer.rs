use logos::{Lexer, Logos};

#[derive(Logos, Debug, Clone, PartialEq)]
pub enum Token {
    #[token("{@")]
    LoopOpen,
    #[token("@}")]
    LoopClose,

    #[token("{")]
    BlockOpen,
    #[token("}")]
    BlockClose,

    #[token("else if")]
    NextCondition,
    #[token("else")]
    EndCondition,
    #[token("if")]
    StartCondition,

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

    #[token("~")]
    Neg,

    #[token(">=")]
    GreaterEq,
    #[token(">")]
    Greater,

    #[token("<=")]
    LessEq,
    #[token("<")]
    Less,

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

    #[regex(r#"\$"[\w]+""#, trim_string)]
    Variable(String),

    #[regex(r"\$[\w]+", trim_string)]
    Command(String),

    #[regex(r#""[^"\\]*(?:\\.[^"\\]*)*""#, trim_string)]
    String(String),

    #[regex(r"[\d]+", |lex| lex.slice().parse())]
    Integer(isize),

    #[regex(r"true|false", |lex| lex.slice().parse())]
    Boolean(bool),

    #[token("exit")]
    Exit,

    #[error]
    #[regex(r"[\s]+", logos::skip)]
    Unknown,
}

fn trim_string(lexer: &mut Lexer<Token>) -> Option<String> {
    Some(
        lexer
            .slice()
            .trim_start_matches('$')
            .trim_start_matches("#\"")
            .trim_start_matches('"')
            .trim_end_matches("\"#")
            .trim_end_matches('"')
            .to_string(),
    )
}

pub fn parse(string: &str) -> logos::Lexer<Token> {
    Token::lexer(string)
}
