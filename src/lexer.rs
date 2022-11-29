use alloc::string::String;
use logos::Logos;

#[derive(Logos, Debug, Clone, PartialEq)]
pub enum Token {
    #[token("{")]
    BlockOpen,
    #[token("}")]
    BlockClose,
    #[token("{@")]
    LoopOpen,
    #[token("@}")]
    LoopClose,

    #[token("if")]
    ConditionalOpen,
    #[token("else if")]
    ConditionalContinue,
    #[token("else")]
    ConditionalEnd,

    #[token(":")]
    Assign,
    #[token("=")]
    Eq,
    #[token("&")]
    And,
    #[token("|")]
    Or,

    #[regex(r#"\$"[A-Za-z_]+""#, |lex| lex.slice().parse())]
    Variable(String),

    #[regex(r"\$[A-Za-z_]+", |lex| lex.slice().parse())]
    Command(String),

    #[regex("[0-9]+", |lex| lex.slice().parse())]
    Integer(isize),

    #[regex(r#""[A-Za-z]+""#, |lex| lex.slice().parse())]
    String(String),

    #[error]
    #[regex(r"[\s\t\r\n\f]+", logos::skip)]
    Unknown,
}

pub fn parse(string: &str) -> logos::Lexer<'_, Token> {
    Token::lexer(string)
}
