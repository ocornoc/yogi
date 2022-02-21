use std::convert::TryInto;
use arrayvec::ArrayVec;
use derive_more::{From, Into, Display};
use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, From, Into, Display)]
#[repr(align(8))]
#[display(fmt = "number #{_0}")]
pub(super) struct NumReg(pub usize);

impl PartialEq<AnyReg> for NumReg {
    fn eq(&self, other: &AnyReg) -> bool {
        other == self
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, From, Into, Display)]
#[repr(align(8))]
#[display(fmt = "string #{_0}")]
pub(super) struct StrReg(pub usize);

impl PartialEq<AnyReg> for StrReg {
    fn eq(&self, other: &AnyReg) -> bool {
        other == self
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, From, Into, Display)]
#[repr(align(8))]
#[display(fmt = "value #{_0}")]
pub(super) struct ValReg(pub usize);

impl PartialEq<AnyReg> for ValReg {
    fn eq(&self, other: &AnyReg) -> bool {
        other == self
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, From, Display)]
pub(super) enum AnyReg {
    Num(NumReg),
    Str(StrReg),
    Val(ValReg),
}

impl PartialEq<NumReg> for AnyReg {
    fn eq(&self, other: &NumReg) -> bool {
        if let AnyReg::Num(r) = self {
            r == other
        } else {
            false
        }
    }
}

impl PartialEq<StrReg> for AnyReg {
    fn eq(&self, other: &StrReg) -> bool {
        if let AnyReg::Str(r) = self {
            r == other
        } else {
            false
        }
    }
}

impl PartialEq<ValReg> for AnyReg {
    fn eq(&self, other: &ValReg) -> bool {
        if let AnyReg::Val(r) = self {
            r == other
        } else {
            false
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, From, Into, Display)]
#[repr(align(8))]
#[display(fmt = "section #{_0}")]
pub(super) struct Section(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) enum Instruction {
    JumpSectionIf(Section, NumReg),
    JumpIfError(Section),
    CopyNum(NumReg, NumReg),
    CopyStr(StrReg, StrReg),
    CopyVal(ValReg, ValReg),
    ValueifyNum(NumReg, ValReg),
    ValueifyStr(StrReg, ValReg),
    NumberifyVal(ValReg, NumReg),
    StringifyNum(NumReg, StrReg),
    StringifyVal(ValReg, StrReg),
    IsTruthyNum(NumReg),
    IsTruthyVal(ValReg, NumReg),
    NotNum(NumReg),
    NotVal(ValReg, NumReg),
    AddNum(NumReg, NumReg),
    AddStr(StrReg, StrReg),
    AddVal(ValReg, ValReg),
    SubNum(NumReg, NumReg),
    SubStr(StrReg, StrReg),
    SubVal(ValReg, ValReg),
    Mul(NumReg, NumReg),
    Div(NumReg, NumReg),
    Rem(NumReg, NumReg),
    Pow(NumReg, NumReg),
    Eq(ValReg, ValReg, NumReg),
    Le(ValReg, ValReg, NumReg),
    Lt(ValReg, ValReg, NumReg),
    IncNum(NumReg),
    IncStr(StrReg),
    IncVal(ValReg),
    DecNum(NumReg),
    DecStr(StrReg),
    DecVal(ValReg),
    Abs(NumReg),
    Fact(NumReg),
    Sqrt(NumReg),
    Sin(NumReg),
    Cos(NumReg),
    Tan(NumReg),
    Asin(NumReg),
    Acos(NumReg),
    Atan(NumReg),
    Neg(NumReg),
    And(NumReg, NumReg),
    Or(NumReg, NumReg),
}

impl Instruction {
    pub fn reads(self) -> ArrayVec<AnyReg, 2> {
        use Instruction::*;

        match self {
            JumpIfError(_) => ArrayVec::new_const(),
            JumpSectionIf(_, r) | CopyNum(r, _) | ValueifyNum(r, _) | StringifyNum(r, _)
            | IsTruthyNum(r) | NotNum(r) | IncNum(r) | Abs(r) | Fact(r) | Sqrt(r) | Sin(r) | Cos(r)
            | Tan(r) | Asin(r) | Acos(r) | Atan(r) | Neg(r) | DecNum(r) =>
                [r.into()].as_ref().try_into().unwrap(),
            CopyStr(r, _) | ValueifyStr(r, _) | IncStr(r) | DecStr(r) =>
                [r.into()].as_ref().try_into().unwrap(),
            CopyVal(r, _) | NumberifyVal(r, _) | StringifyVal(r, _) | IsTruthyVal(r, _)
            | NotVal(r, _) | IncVal(r) | DecVal(r) => [r.into()].as_ref().try_into().unwrap(),
            AddNum(r1, r2) | SubNum(r1, r2) | Mul(r1, r2) | Div(r1, r2) | Rem(r1, r2) | Pow(r1, r2)
            | And(r1, r2) | Or(r1, r2) => [r1.into(), r2.into()].into(),
            SubStr(r1, r2) | AddStr(r1, r2) => [r1.into(), r2.into()].into(),
            AddVal(r1, r2) | SubVal(r1, r2) | Eq(r1, r2, _) | Le(r1, r2, _) | Lt(r1, r2, _) =>
                [r1.into(), r2.into()].into(),
        }
    }

    pub fn modifies(self) -> Option<AnyReg> {
        use Instruction::*;

        match self {
            CopyNum(_, r) | IsTruthyNum(r) | NumberifyVal(_, r) | IsTruthyVal(_, r) | NotNum(r)
            | NotVal(_, r) | AddNum(r, _) | SubNum(r, _) | Mul(r, _) | Div(r, _) | Rem(r, _)
            | Pow(r, _) | Eq(.., r) | Le(.., r) | Lt(.., r) | IncNum(r) | Abs(r) | Fact(r) | Sqrt(r)
            | Sin(r) | Cos(r) | Tan(r) | Asin(r) | Acos(r) | Atan(r)
            | Neg(r) | And(r, _) | Or(r, _) | DecNum(r) => Some(r.into()),
            StringifyNum(_, r) | CopyStr(_, r) | StringifyVal(_, r) | AddStr(r, _) | SubStr(r, _)
            | IncStr(r) | DecStr(r) => Some(r.into()),
            CopyVal(_, r) | ValueifyNum(_, r) | ValueifyStr(_, r) | AddVal(r, _) | SubVal(r, _)
            | IncVal(r) | DecVal(r) => Some(r.into()),
            JumpSectionIf(..) | JumpIfError(_) => None,
        }
    }

    pub fn relevant(self) -> ArrayVec<AnyReg, 3> {
        let mut array = ArrayVec::new_const();
        array.extend(self.reads());
        array.extend(self.modifies());
        array
    }

    pub const fn get_section(self) -> Option<Section> {
        if let Instruction::JumpSectionIf(s, _) | Instruction::JumpIfError(s) = self {
            Some(s)
        } else {
            None
        }
    }

    fn get_mut_num_regs(&mut self) -> ArrayVec<&mut NumReg, 2> {
        match self {
            Instruction::JumpSectionIf(_, n) | Instruction::Abs(n) | Instruction::Fact(n)
            | Instruction::Sqrt(n) | Instruction::Sin(n) | Instruction::Cos(n) | Instruction::Tan(n)
            | Instruction::Asin(n) | Instruction::Acos(n) | Instruction::Atan(n)
            | Instruction::Neg(n) | Instruction::IncNum(n) | Instruction::DecNum(n)
            | Instruction::ValueifyNum(n, _) | Instruction::NumberifyVal(_, n)
            | Instruction::StringifyNum(n, _) | Instruction::IsTruthyNum(n)
            | Instruction::IsTruthyVal(_, n) | Instruction::NotNum(n) | Instruction::NotVal(_, n)
            | Instruction::Eq(_, _, n) | Instruction::Le(_, _, n) | Instruction::Lt(_, _, n) =>
                [n].into_iter().collect(),
            Instruction::CopyNum(n1, n2) | Instruction::AddNum(n1, n2) | Instruction::SubNum(n1, n2)
            | Instruction::Mul(n1, n2) | Instruction::Div(n1, n2) | Instruction::Rem(n1, n2)
            | Instruction::Pow(n1, n2) | Instruction::And(n1, n2) | Instruction::Or(n1, n2) =>
                [n1, n2].into(),
            _ => ArrayVec::new_const(),
        }
    }

    fn get_mut_str_regs(&mut self) -> ArrayVec<&mut StrReg, 2> {
        match self {
            Instruction::ValueifyStr(s, _) | Instruction::StringifyNum(_, s)
            | Instruction::StringifyVal(_, s) | Instruction::IncStr(s) | Instruction::DecStr(s) =>
                [s].into_iter().collect(),
            Instruction::AddStr(s1, s2) | Instruction::SubStr(s1, s2)
            | Instruction::CopyStr(s1, s2) => [s1, s2].into(),
            _ => ArrayVec::new_const(),
        }
    }

    fn get_mut_val_regs(&mut self) -> ArrayVec<&mut ValReg, 2> {
        match self {
            Instruction::ValueifyNum(_, v) | Instruction::ValueifyStr(_, v)
            | Instruction::NumberifyVal(v, _) | Instruction::StringifyVal(v, _)
            | Instruction::IsTruthyVal(v, _) | Instruction::NotVal(v, _) | Instruction::IncVal(v)
            | Instruction::DecVal(v) => [v].into_iter().collect(),
            Instruction::CopyVal(v1, v2) | Instruction::AddVal(v1, v2) | Instruction::SubVal(v1, v2)
            | Instruction::Eq(v1, v2, _) | Instruction::Le(v1, v2, _)
            | Instruction::Lt(v1, v2, _) => [v1, v2].into(),
            _ => ArrayVec::new_const(),
        }
    }

    pub fn remove_reg(&mut self, reg: AnyReg) {
        let self_copy = self.clone();
        match reg {
            AnyReg::Num(n) => {
                for r in self.get_mut_num_regs() {
                    debug_assert_ne!(*r, n, "Tried to delete existing reg in {self_copy}");
                    if *r > n {
                        r.0 -= 1;
                    }
                }
            },
            AnyReg::Str(s) => {
                for r in self.get_mut_str_regs() {
                    debug_assert_ne!(*r, s, "Tried to delete existing reg in {self_copy}");
                    if *r > s {
                        r.0 -= 1;
                    }
                }
            },
            AnyReg::Val(v) => {
                for r in self.get_mut_val_regs() {
                    debug_assert_ne!(*r, v, "Tried to delete existing reg in {self_copy}");
                    if r.0 > v.0 {
                        r.0 -= 1;
                    }
                }
            },
        }
    }

    pub fn remove_section(&mut self, section: Section) {
        if let Instruction::JumpSectionIf(s, _) | Instruction::JumpIfError(s) = self {
            debug_assert_ne!(s.clone(), section, "Tried to delete existing section in {self}");
            if s.0 > section.0 {
                s.0 -= 1;
            }
        }
    }

    pub const fn can_runtime_err(self) -> bool {
        matches!(
            self,
            Instruction::NumberifyVal(..)
            | Instruction::Div(..)
            | Instruction::Rem(..),
        )
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Instruction::JumpSectionIf(s, n) =>
                write!(f, "If {} is truthy, jump to {}", n, s),
            Instruction::JumpIfError(s) =>
                write!(f, "If the error flag is set, jump to {}", s),
            Instruction::CopyNum(i, o) =>
                write!(f, "{} = {}", o, i),
            Instruction::CopyStr(i, o) =>
                write!(f, "{} = {}", o, i),
            Instruction::CopyVal(i, o) =>
                write!(f, "{} = {}", o, i),
            Instruction::ValueifyNum(i, o) =>
                write!(f, "{} = {}", o, i),
            Instruction::ValueifyStr(i, o) =>
                write!(f, "{} = {}", o, i),
            Instruction::NumberifyVal(i, o) =>
                write!(f, "If {0:} is a number, {1:} = {0:}. Otherwise, error.", i, o),
            Instruction::StringifyNum(i, o) =>
                write!(f, "{} = {}", o, i),
            Instruction::StringifyVal(i, o) =>
                write!(f, "{} = {}", o, i),
            Instruction::IsTruthyNum(n) =>
                write!(f, "{0:} = {0:} is truthy", n),
            Instruction::IsTruthyVal(v, n) =>
                write!(f, "{} = {} is truthy", n, v),
            Instruction::NotNum(n) =>
                write!(f, "{0:} = !{0:}", n),
            Instruction::NotVal(v, n) =>
                write!(f, "{} = !{}", n, v),
            Instruction::AddNum(l, r) =>
                write!(f, "{} += {}", l, r),
            Instruction::AddStr(l, r) =>
                    write!(f, "{} += {}", l, r),
            Instruction::AddVal(l, r) =>
                    write!(f, "{} += {}", l, r),
            Instruction::SubNum(l, r) =>
                    write!(f, "{} -= {}", l, r),
            Instruction::SubStr(l, r) =>
                    write!(f, "{} -= {}", l, r),
            Instruction::SubVal(l, r) =>
                    write!(f, "{} -= {}", l, r),
            Instruction::Mul(l, r) =>
                write!(f, "{} *= {}", l, r),
            Instruction::Div(l, r) =>
                write!(f, "{} /= {}", l, r),
            Instruction::Rem(l, r) =>
                write!(f, "{} %= {}", l, r),
            Instruction::Pow(l, r) =>
                write!(f, "{} ^= {}", l, r),
            Instruction::Eq(l, r, o) =>
                write!(f, "{} = {} == {}", o, l, r),
            Instruction::Le(l, r, o) =>
                write!(f, "{} = {} <= {}", o, l, r),
            Instruction::Lt(l, r, o) =>
                write!(f, "{} = {} > {}", o, l, r),
            Instruction::IncNum(n) =>
                write!(f, "++({})", n),
            Instruction::IncStr(s) =>
                write!(f, "++({})", s),
            Instruction::IncVal(v) =>
                write!(f, "++({})", v),
            Instruction::DecNum(n) =>
                write!(f, "--({})", n),
            Instruction::DecStr(s) =>
                write!(f, "--({})", s),
            Instruction::DecVal(v) =>
                write!(f, "--({})", v),
            Instruction::Abs(n) =>
                write!(f, "{0:} = abs({0:})", n),
            Instruction::Fact(n) =>
                write!(f, "{0:} = factorial({0:})", n),
            Instruction::Sqrt(n) =>
                write!(f, "{0:} = sqrt({0:})", n),
            Instruction::Sin(n) =>
                write!(f, "{0:} = sin({0:})", n),
            Instruction::Cos(n) =>
                write!(f, "{0:} = cos({0:})", n),
            Instruction::Tan(n) =>
                write!(f, "{0:} = tan({0:})", n),
            Instruction::Asin(n) =>
                write!(f, "{0:} = asin({0:})", n),
            Instruction::Acos(n) =>
                write!(f, "{0:} = acos({0:})", n),
            Instruction::Atan(n) =>
                write!(f, "{0:} = atan({0:})", n),
            Instruction::Neg(n) =>
                write!(f, "{0:} = -({0:})", n),
            Instruction::And(l, r) =>
                write!(f, "{} &= {}", l, r),
            Instruction::Or(l, r) =>
                write!(f, "{} |= {}", l, r),
        }
    }
}
