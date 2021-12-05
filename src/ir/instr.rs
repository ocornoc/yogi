use std::convert::TryInto;
use arrayvec::ArrayVec;
use derive_more::{From, Into};
use super::*;
use arith::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, From, Into)]
#[repr(align(4))]
pub(super) struct NumReg(pub usize);

impl Display for NumReg {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "number #{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, From, Into)]
#[repr(align(4))]
pub(super) struct StrReg(pub usize);

impl Display for StrReg {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "string #{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, From, Into)]
#[repr(align(4))]
pub(super) struct ValReg(pub usize);

impl Display for ValReg {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "value #{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, From, Into)]
pub(super) struct Section(pub usize);

impl Display for Section {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "section #{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
