use intaglio::Symbol;
use logos::{Lexer, Logos, Span};

#[derive(Logos, Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenKind {
    #[regex(r"#\[.+\]", |lex| lex.slice().parse())]
    Preprocess(String),

    #[token("this")]
    This,
    #[token("::")]
    Path,

    #[token(";")]
    Terminator,
    #[token(",")]
    Separator,

    #[token("(")]
    GroupOpen,
    #[token(")")]
    GroupClose,
    #[token("{")]
    BlockOpen,
    #[token("}")]
    BlockClose,
    #[token("[")]
    ArrayOpen,
    #[token("]")]
    ArrayClose,

    #[token("var")]
    VarDef,
    #[token("type")]
    TypeDef,

    #[token("()")]
    TypeUnit,
    #[token("Int")]
    TypeInt,
    #[token("UInt")]
    TypeUInt,
    #[token("Bool")]
    TypeBool,

    #[token("+")]
    Add,
    #[token("-")]
    Sub,
    #[token("*")]
    Mul,
    #[token("/")]
    Div,
    #[token("**")]
    Exp,
    #[token("%")]
    Rem,
    #[token(">>")]
    Shr,
    #[token("<<")]
    Shl,
    #[token("^")]
    BitXor,
    #[token("|")]
    BitOr,
    #[token("&")]
    BitAnd,
    #[token("=")]
    Eq,
    #[token("!=")]
    NotEq,
    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterEq,
    #[token("<")]
    Less,
    #[token("<=")]
    LessEq,
    #[token("||")]
    Or,
    #[token("^^")]
    Xor,
    #[token("&&")]
    And,
    #[token("?>")]
    Clow,
    #[token("??")]
    Cerm,
    #[token(":")]
    Assign,
    #[token("=>")]
    Flow,

    #[regex(r"!?\d+I?", lex_integer, priority = 3)]
    Integer(isize),
    #[regex(r"\d+U?", lex_uinteger, priority = 2)]
    UInteger(usize),
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

    #[error]
    #[regex(r"[\s]+", logos::skip)]
    #[regex(r"#.*", logos::skip)]
    Error,
}

fn lex_integer(lexer: &mut Lexer<TokenKind>) -> Option<isize> {
    let slice = lexer.slice();

    let back_offset = if slice.ends_with('I') {
        slice.len() - 1
    } else {
        slice.len()
    };
    let front_offset = if slice.starts_with('!') { 1 } else { 0 };

    slice[front_offset..back_offset]
        .parse::<isize>()
        .map(|int| if front_offset > 0 { !int } else { int })
        .ok()
}

fn lex_uinteger(lexer: &mut Lexer<TokenKind>) -> Option<usize> {
    let slice = lexer.slice();

    let back_offset = if slice.ends_with('U') {
        slice.len() - 1
    } else {
        slice.len()
    };

    slice[0..back_offset].parse::<usize>().ok()
}

impl core::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let display_str = match self {
            TokenKind::Terminator => ";",
            TokenKind::Separator => ",",
            TokenKind::GroupOpen => "(",
            TokenKind::GroupClose => ")",
            TokenKind::BlockOpen => "{",
            TokenKind::BlockClose => "}",
            TokenKind::ArrayOpen => "[",
            TokenKind::ArrayClose => "]",
            TokenKind::VarDef => "var",
            TokenKind::TypeDef => "type",
            TokenKind::TypeUnit => "()",
            TokenKind::TypeInt => "Int",
            TokenKind::TypeUInt => "UInt",
            TokenKind::TypeBool => "Bool",
            TokenKind::Add => "+",
            TokenKind::Sub => "-",
            TokenKind::Mul => "*",
            TokenKind::Div => "/",
            TokenKind::Exp => "**",
            TokenKind::Rem => "%",
            TokenKind::Shr => ">>",
            TokenKind::Shl => "<<",
            TokenKind::BitXor => "^",
            TokenKind::BitOr => "|",
            TokenKind::BitAnd => "&",
            TokenKind::Eq => "=",
            TokenKind::NotEq => "!=",
            TokenKind::Greater => ">",
            TokenKind::GreaterEq => ">=",
            TokenKind::Less => "<",
            TokenKind::LessEq => "<=",
            TokenKind::Or => "||",
            TokenKind::Xor => "^^",
            TokenKind::And => "&&",
            TokenKind::Assign => ":",
            TokenKind::Flow => "=>",

            _ => return Err(std::fmt::Error),
        };

        f.write_str(display_str)
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

pub type Token = (TokenKind, Span);

#[derive(Debug)]
pub struct Tokens {
    tokens: Box<[Token]>,
    index: usize,
}

impl Iterator for Tokens {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.tokens.get(self.index).cloned();
        if token.is_some() {
            self.index += 1;
        }

        token
    }
}

impl From<Tokens> for chumsky::Stream<'_, TokenKind, Span, Tokens> {
    fn from(tokens: Tokens) -> Self {
        let tokens_len = tokens.tokens.len();
        #[allow(clippy::range_plus_one)]
        chumsky::Stream::from_iter(tokens_len..(tokens_len + 1), tokens)
    }
}

pub fn lex(input: &str) -> Tokens {
    let mut tokens = TokenKind::lexer(input).spanned().collect::<Vec<Token>>();
    tokens.shrink_to_fit();

    Tokens {
        tokens: tokens.into_boxed_slice(),
        index: 0,
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::TokenKind, tests::lex_and_eq};

    #[test]
    fn integer() {
        lex_and_eq("12345", [TokenKind::Integer(12345)].into_iter());
    }

    #[test]
    fn neg_integer() {
        lex_and_eq("!12345I", [TokenKind::Integer(!12345)].into_iter())
    }

    #[test]
    fn integer_i() {
        lex_and_eq("12345I", [TokenKind::Integer(12345)].into_iter());
    }

    #[test]
    fn neg_integer_i() {
        lex_and_eq("!12345I", [TokenKind::Integer(!12345)].into_iter())
    }

    #[test]
    fn uinteger_u() {
        lex_and_eq("12345U", [TokenKind::UInteger(12345)].into_iter());
    }
}
