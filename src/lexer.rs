use chumsky::Stream;
use intaglio::Symbol;
use logos::{Lexer, Logos};

#[derive(Logos, Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenKind {
    #[regex(r"#\[.]?\]", |lex| lex.slice().parse())]
    Preprocess(String),

    #[token(";")]
    Terminator,
    #[token(",")]
    Separator,

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

    #[token("|")]
    Or,
    #[token("&")]
    And,
    #[token("||")]
    OrCircuit,
    #[token("&&")]
    AndCircuit,

    #[token("var")]
    VarDef,
    #[token("ty")]
    TypeDef,

    #[token("Int")]
    TypeInt,
    #[token("Bool")]
    TypeBool,
    #[token("Str")]
    TypeStr,

    #[regex(r"![\d]+", parse_neg_integer)]
    #[regex(r"[\d]+", |lex| lex.slice().parse(), priority = 2)]
    Integer(isize),
    #[regex(r"true|false", |lex| lex.slice().parse())]
    Boolean(bool),
    #[regex(r#""[^"\\]*(?:\\.[^"\\]*)*""#, trim_and_cache)]
    String(Symbol),
    #[regex(r"[A-Za-z_][\w]*", trim_and_cache)]
    Symbol(Symbol),

    #[regex(r#"\$"[\w]+""#, trim_and_cache)]
    EnvVar(Symbol),
    #[regex(r"\$[\w]+", trim_and_cache)]
    EnvCmd(Symbol),

    #[token("exit")]
    Exit,

    #[regex(r"[\s]+", logos::skip)]
    #[regex(r"//[\S\t\v\r ]*\n?", logos::skip)]
    Discard,

    #[error]
    Unknown,
}

impl From<&TokenKind> for &str {
    fn from(value: &TokenKind) -> Self {
        match value {
            TokenKind::Terminator => ";",
            TokenKind::Separator => ",",
            TokenKind::GroupOpen => "(",
            TokenKind::GroupClose => ")",
            TokenKind::TupleOpen => "{",
            TokenKind::TupleClose => "}",
            TokenKind::ArrayOpen => "[",
            TokenKind::ArrayClose => "]",
            TokenKind::AddAssign => ":+",
            TokenKind::SubAssign => ":-",
            TokenKind::MulAssign => ":*",
            TokenKind::DivAssign => ":/",
            TokenKind::ShrAssign => ":>>",
            TokenKind::ShlAssign => ":<<",
            TokenKind::Assign => ":",
            TokenKind::Add => "+",
            TokenKind::Sub => "-",
            TokenKind::Mul => "*",
            TokenKind::Div => "/",
            TokenKind::Shr => ">>",
            TokenKind::Shl => "<<",
            TokenKind::Xor => "^",
            TokenKind::Insert => "=>",
            TokenKind::GreaterEq => ">=",
            TokenKind::Greater => ">",
            TokenKind::LessEq => "<=",
            TokenKind::Less => "<",
            TokenKind::NegEq => "!=",
            TokenKind::Eq => "=",
            TokenKind::Or => "|",
            TokenKind::And => "&",
            TokenKind::OrCircuit => "||",
            TokenKind::AndCircuit => "&&",
            TokenKind::VarDef => "var",
            TokenKind::TypeDef => "ty",
            TokenKind::TypeInt => "Int",
            TokenKind::TypeBool => "Bool",
            TokenKind::TypeStr => "Str",
            TokenKind::Integer(_) => "integer",
            TokenKind::Boolean(_) => "boolean",
            TokenKind::String(_) => "string",
            TokenKind::Symbol(_) => "symbol",
            TokenKind::EnvVar(_) => r#"$"VAR_NAME""#,
            TokenKind::EnvCmd(_) => "$cmd_name",
            TokenKind::Exit => "exit",

            _ => "NOPRINT",
        }
    }
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
}


impl Clone for TokenIterator<'_> {
    fn clone(&self) -> Self {
        let lexer = Tokenizer(TokenKind::lexer(self.src).spanned()).peekable();
        Self { src: self.src, lexer  }
    }
}

impl Iterator for TokenIterator<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.lexer.next() {
                Some((kind, span)) if let TokenKind::Unknown = kind => {
                    println!("Error processing token: {:?}", self.src().get(span));
                    continue;
                }
                
                token => return token
            }
            
        }
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
