use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Arbitrary)]
pub struct BoolInterval {
    pub bfalse: bool,
    pub btrue: bool,
}

impl BoolInterval {
    pub const fn everything() -> Self {
        BoolInterval {
            bfalse: true,
            btrue: true,
        }
    }

    pub const fn nothing() -> Self {
        BoolInterval {
            bfalse: false,
            btrue: false,
        }
    }
}

impl Default for BoolInterval {
    fn default() -> Self {
        Self::everything()
    }
}

impl Not for BoolInterval {
    type Output = Self;

    fn not(mut self) -> Self::Output {
        std::mem::swap(&mut self.btrue, &mut self.bfalse);
        self
    }
}

impl BitOrAssign for BoolInterval {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = BoolInterval {
            bfalse: self.bfalse && rhs.bfalse,
            btrue: self.btrue || rhs.btrue,
        };
    }
}

impl BitOr for BoolInterval {
    type Output = Self;

    fn bitor(mut self, rhs: Self) -> Self::Output {
        self |= rhs;
        self
    }
}

impl BitAndAssign for BoolInterval {
    fn bitand_assign(&mut self, rhs: Self) {
        *self = BoolInterval {
            bfalse: self.bfalse || rhs.bfalse,
            btrue: self.btrue && rhs.btrue,
        };
    }
}

impl BitAnd for BoolInterval {
    type Output = Self;

    fn bitand(mut self, rhs: Self) -> Self::Output {
        self &= rhs;
        self
    }
}

impl BitXorAssign for BoolInterval {
    fn bitxor_assign(&mut self, rhs: Self) {
        *self = BoolInterval {
            bfalse: (self.bfalse && rhs.bfalse) || (self.btrue && rhs.btrue),
            btrue: (self.bfalse && rhs.btrue) || (self.btrue && rhs.bfalse),
        };
    }
}

impl BitXor for BoolInterval {
    type Output = Self;

    fn bitxor(mut self, rhs: Self) -> Self::Output {
        self ^= rhs;
        self
    }
}

impl From<bool> for BoolInterval {
    fn from(b: bool) -> Self {
        if b {
            BoolInterval {
                bfalse: false,
                btrue: true,
            }
        } else {
            BoolInterval {
                bfalse: true,
                btrue: false,
            }
        }
    }
}

impl From<Number> for BoolInterval {
    fn from(Number(n): Number) -> Self {
        (n != 0).into()
    }
}

impl Display for BoolInterval {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.write_str(match (self.bfalse, self.btrue) {
            (true, true) => "{⊥, \u{22a4}}",
            (true, false) => "{⊥}",
            (false, true) => "{\u{22a4}}",
            (false, false) => "∅",
        })
    }
}