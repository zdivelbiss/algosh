use alloc::string::String;

#[derive(Debug, Clone)]
pub enum Token {
    BlockOpen,
    BlockClose,
    LoopOpen,
    LoopClose,
    Identifier(String),
    Variable(String),
    Command(String),
    Integer(isize),
    String(String),
    Unknown(char),
}

pub struct Lexer<'a> {
    pub chars: core::iter::Peekable<core::str::Chars<'a>>,
}

impl Lexer<'_> {
    #[inline]
    fn is_identifier_character(chr: char) -> bool {
        chr.is_alphanumeric() || chr == '_'
    }

    #[inline]
    fn discard_char(&mut self) {
        self.chars
            .next()
            .expect("attempted to discard with no characters");
    }

    #[inline]
    fn peek_char(&mut self) -> Option<char> {
        self.chars.peek().copied()
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let peek = *self.chars.peek()?;

        match peek {
            '{' => {
                self.discard_char();
                Some(Token::BlockOpen)
            }

            '}' => {
                self.discard_char();
                Some(Token::BlockClose)
            }

            '$' => {
                // Discard the '$'.
                self.discard_char();

                let mut identifier = String::new();
                if self.peek_char() == Some('"') {
                    // Discard the '"'.
                    self.discard_char();

                    'capture: loop {
                        match self.peek_char() {
                            Some('"') => {
                                // Discard the '"'.
                                self.discard_char();
                                break 'capture;
                            }

                            Some(peek) if Lexer::is_identifier_character(peek) => {
                                identifier.push(peek);
                                self.discard_char();
                            }

                            Some(peek) => panic!("invalid variable name character: {:?}", peek),
                            None => panic!("expected variable terminator"),
                        }
                    }

                    Some(Token::Variable(identifier))
                } else {
                    'capture: loop {
                        match self.peek_char() {
                            Some(peek) if Lexer::is_identifier_character(peek) => {
                                identifier.push(peek);
                                self.discard_char();
                            }

                            Some(_) => break 'capture,
                            None => panic!("expected command identifier"),
                        }
                    }

                    Some(Token::Command(identifier))
                }
            }

            char => {
                self.discard_char();
                Some(Token::Unknown(char))
            }
        }
    }
}
