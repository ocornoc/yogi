use std::convert::TryInto;
use arrayvec::ArrayVec;
use derive_more::{From, Into};
use super::*;
use arith::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, From, Into)]
#[repr(align(4))]
pub(super) struct NumReg(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, From, Into)]
#[repr(align(4))]
pub(super) struct StrReg(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, From, Into)]
#[repr(align(4))]
pub(super) struct ValReg(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, From, Into)]
pub(super) struct Section(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, From)]
pub(super) enum Register {
    Num(NumReg),
    Str(StrReg),
    Val(ValReg),
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
    pub fn reads(self) -> ArrayVec<Register, 2> {
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

    pub fn modifies(self) -> Option<Register> {
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

    pub fn relevant(self) -> ArrayVec<Register, 3> {
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
