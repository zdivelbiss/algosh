use std::ops::Add;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Primitive {
    Unit,
    Int(isize),
    UInt(usize),
    Bool(bool),
}

impl Add for Primitive {
    type Output = Option<Self>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Int(lhs), Self::Int(rhs)) => Some(Self::Int(lhs.wrapping_add(rhs))),
            (Self::UInt(lhs), Self::UInt(rhs)) => Some(Self::UInt(lhs.wrapping_add(rhs))),

            _ => None,
        }
    }
}
