use crate::{lexer::TokenKind, Error};
use ariadne::Source;
use chumsky::Parser;
use std::fmt::Debug;

pub fn lex_and_eq<T: Debug + Iterator<Item = TokenKind>>(input: &str, other: T) {
    let lex = crate::lexer::lex(input).map(|t| t.0);

    if let Some(non_eq) = lex.zip(other).find(|(a, b)| a.ne(b)) {
        panic!("{non_eq:?}");
    }
}

pub fn parse_and_eq<T: Debug + PartialEq>(
    input: &str,
    parser: impl Parser<TokenKind, T, Error = Error>,
    other_t: &T,
) {
    match parser.parse(crate::lexer::lex(input)) {
        Ok(t) => assert_eq!(&t, other_t),
        Err(errs) => {
            for err in errs {
                err.generate_report().eprint(Source::from(input)).unwrap();
            }

            panic!()
        }
    }
}
