#[derive(Debug, PartialEq, Eq)]
pub enum TokenKind {
    Discard,

    BlockOpen,
    BlockClose,

    StartCondition,
    NextCondition,

    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ShrAssign,
    ShlAssign,

    Add,
    Sub,
    Mul,
    Div,
    Shr,
    Shl,
    Xor,

    GreaterEq,
    Greater,
    LessEq,
    Less,
    NegEq,
    Eq,

    AndCircuit,
    And,
    OrCircuit,
    Or,

    Boolean(bool),
    String(String),
    Integer(isize),

    Identifier(String),
    Variable(String),
    Command(String),

    Loop,
    Exit,

    Unknown,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    index: usize,
    kind: TokenKind,
}

impl Token {
    #[inline]
    pub const fn new(index: usize, kind: TokenKind) -> Self {
        Self { index, kind }
    }
}
