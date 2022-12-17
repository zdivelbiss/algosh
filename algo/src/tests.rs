use crate::{lexer::TokenKind, Error};
use ariadne::Source;
use chumsky::Parser;

pub fn parse_and_eq<T: std::fmt::Debug + PartialEq>(
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

            std::process::exit(-1)
        }
    }
}


