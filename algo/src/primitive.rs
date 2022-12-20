use std::ops;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Primitive {
    Unit,
    Int(isize),
    UInt(usize),
    Bool(bool),
}

impl ops::Add for Primitive {
    type Output = Option<Self>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Int(lhs), Self::Int(rhs)) => Some(Self::Int(lhs.wrapping_add(rhs))),
            (Self::UInt(lhs), Self::UInt(rhs)) => Some(Self::UInt(lhs.wrapping_add(rhs))),

            _ => None,
        }
    }
}

impl ops::Sub for Primitive {
    type Output = Option<Self>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Int(lhs), Self::Int(rhs)) => Some(Self::Int(lhs.wrapping_sub(rhs))),
            (Self::UInt(lhs), Self::UInt(rhs)) => Some(Self::UInt(lhs.wrapping_sub(rhs))),

            _ => None,
        }
    }
}
