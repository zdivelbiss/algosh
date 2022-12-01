pub mod token;

use std::str::pattern::Pattern;
use token::*;

pub struct Lexer<'a> {
    data: &'a str,
    index: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(data: &'a str) -> Self {
        Lexer { data, index: 0 }
    }

    fn new_token(&self, kind: TokenKind) -> Token {
        Token::new(self.index, kind)
    }

    fn remaining_data(&self) -> Option<&'a str> {
        self.data.get(self.index..)
    }

    fn next_eq(
        &mut self,
        p: impl Pattern<'a>,
        tokenize: impl FnOnce(&str) -> TokenKind,
    ) -> Option<Token> {
        let remaining_data = self.remaining_data()?;
        let pattern_suffix_data = p.strip_prefix_of(remaining_data)?;
        let pattern_len = remaining_data.len() - pattern_suffix_data.len();

        if pattern_len > 0 {
            let pattern_str = self.data.get(self.index..(self.index + pattern_len))?;
            self.index += pattern_len;

            Some(self.new_token(tokenize(pattern_str)))
        } else {
            None
        }
    }

    fn next_while(
        &mut self,
        mut predicate: impl FnMut(usize, char) -> bool,
        tokenize: impl FnOnce(&str) -> TokenKind,
    ) -> Option<Token> {
        let match_len = self
            .remaining_data()?
            .chars()
            .enumerate()
            .take_while(|(iteration, ch)| predicate(*iteration, *ch))
            .count();
        if match_len > 0 {
            let pattern_str = self.data.get(self.index..(self.index + match_len))?;
            self.index += match_len;

            Some(self.new_token(tokenize(pattern_str)))
        } else {
            None
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index > self.data.len() {
            None
        } else {
            self.next_eq(|ch: char| ch.is_whitespace(), |_| TokenKind::Discard)
                .or_else(|| self.next_eq('{', |_| TokenKind::BlockOpen))
                .or_else(|| self.next_eq('}', |_| TokenKind::BlockClose))
                .or_else(|| self.next_eq("true", |_| TokenKind::Boolean(true)))
                .or_else(|| self.next_eq("false", |_| TokenKind::Boolean(false)))
                // TokenKind::Command
                .or_else(|| {
                    self.next_while(
                        |iteration, ch| {
                            (iteration == 0 && ch == '$') || (iteration > 0 && ch.is_alphanumeric())
                        },
                        |str| TokenKind::Command(trim_string(str).to_string()),
                    )
                })
                // TokenKind::Variable
                .or_else(|| {
                    let mut has_closing_quotation = false;
                    self.next_while(
                        |iteration, ch| match (iteration, ch) {
                            (0, '$') | (1, '"') => true,

                            (iteration, '"') if iteration > 0 => {
                                has_closing_quotation = true;
                                true
                            }

                            (iteration, ch) => {
                                iteration > 0
                                    && !has_closing_quotation
                                    && (ch == '_' || ch.is_alphanumeric())
                            }
                        },
                        |str| TokenKind::Variable(trim_string(str).to_string()),
                    )
                })
                // TokenKind::Integer
                .or_else(|| {
                    self.next_while(
                        |iteration, ch| (iteration == 0 && ch == '!') || ch.is_numeric(),
                        |num| {
                            TokenKind::Integer(
                                isize::from_str_radix(num.replace('!', "-").as_str(), 10).unwrap(),
                            )
                        },
                    )
                })
                // TokenKind::String
                .or_else(|| {
                    let mut is_escaped = false;
                    let mut has_closing_quotation = false;
                    self.next_while(
                        |iteration, ch| match ch {
                            _ if has_closing_quotation => false,
                            _ if is_escaped => {
                                is_escaped = false;
                                true
                            }

                            '"' if iteration == 0 => true,
                            '"' => {
                                has_closing_quotation = true;
                                true
                            }

                            '\\' => {
                                is_escaped = true;
                                true
                            }

                            _ => iteration > 0,
                        },
                        |str| TokenKind::String(trim_string(str).to_string()),
                    )
                })
                // TokenKind::Identifier
                .or_else(|| {
                    self.next_while(
                        |iteration, ch| {
                            (iteration == 0 && ch.is_alphabetic())
                                || ch.is_alphanumeric()
                                || ch == '_'
                        },
                        |str| TokenKind::Identifier(str.to_string()),
                    )
                })
                .or_else(|| self.next_eq(|_: char| true, |_| TokenKind::Unknown))
        }
    }
}

fn trim_string(str: &str) -> &str {
    str.trim_start_matches('$')
        .trim_start_matches('"')
        .trim_end_matches('"')
}
