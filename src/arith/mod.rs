use std::fmt::{Display, Formatter, Result as FmtResult, Write};
use std::str::FromStr;
use std::ops::*;
use thiserror::Error;
pub mod value;
pub mod ystring;
pub use value::*;
pub use ystring::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
pub struct Number(pub i64);

impl Number {
    const SCALE: i64 = 1000;
    const SCALE_F32: f32 = Self::SCALE as f32;
    const SCALE_F64: f64 = Self::SCALE as f64;
    pub const MAX: Number = Number(i64::MAX);
    pub const MIN: Number = Number(i64::MIN);
    pub const ONE: Number = Number(1 * Self::SCALE);
    pub const ZERO: Number = Number(0);
    const MAX_VAL_F64: f64 = Self::MAX.0 as f64 / Self::SCALE_F64;
    const MIN_VAL_F64: f64 = Self::MIN.0 as f64 / Self::SCALE_F64;
    const MAX_VAL_F32: f32 = Self::MAX_VAL_F64 as f32;
    const MIN_VAL_F32: f32 = Self::MIN_VAL_F64 as f32;

    pub fn new(mut n: f64) -> Self {
        if n.is_finite() && {
            n *= Self::SCALE_F64;
            n <= Self::MAX_VAL_F64 && n >= Self::MIN_VAL_F64
        } {
            Number(n as i64)
        } else {
            Number::MIN
        }
    }

    pub fn new_f32(mut n: f32) -> Self {
        if n.is_finite() && {
            n *= Self::SCALE_F32;
            n <= Self::MAX_VAL_F32 && n >= Self::MIN_VAL_F32
        } {
            Number(n as i64)
        } else {
            Number::MIN
        }
    }

    pub fn as_f64(self) -> f64 {
        self.0 as f64 * Self::SCALE_F64.recip()
    }

    pub fn as_f32(self) -> f32 {
        self.0 as f32 * Self::SCALE_F32.recip()
    }

    pub fn as_bool(self) -> bool {
        self == Self::ZERO
    }

    pub fn stringify<'a>(&self, buffer: &'a mut String) -> &'a mut YString {
        buffer.clear();
        let out = write!(buffer, "{}", self);
        if cfg!(debug_assertions) {
            out.unwrap();
        }
        buffer.into()
    }

    pub fn div_assign(&mut self, other: Self) -> ValueResult<()> {
        *self = (*self / other)?;
        Ok(())
    }

    fn round_to_new(mut v: f64) -> Self {
        v += 5e-5_f64.copysign(v);
        Self::new(v)
    }

    pub fn pow(self, other: Self) -> Self {
        let v = self.as_f64().powf(other.as_f64());
        Self::round_to_new(v)
    }

    pub fn pow_assign(&mut self, other: Self) {
        *self = self.pow(other);
    }

    pub fn rem_assign(&mut self, other: Self) -> ValueResult<()> {
        *self = (*self % other)?;
        Ok(())
    }

    pub fn pre_inc(&mut self) {
        *self += Number::ONE;
    }

    pub fn post_inc(&mut self) -> Self {
        let new = self.clone();
        self.pre_inc();
        new
    }

    pub fn pre_dec(&mut self) {
        *self -= Number::ONE;
    }

    pub fn post_dec(&mut self) -> Self {
        let new = self.clone();
        self.pre_dec();
        new
    }

    pub fn abs(self) -> Self {
        Number(self.0.overflowing_abs().0)
    }

    pub fn sqrt(self) -> Self {
        if self.0 < 0 || self.0 > 9223372036854775000 {
            Self::MIN
        } else {
            let v = self.as_f64().sqrt();
            Self::round_to_new(v)
        }
    }

    pub fn sin(self) -> Self {
        let n = self.as_f32().to_radians().sin();
        // is this needed? also in cos
        /*n *= Self::SCALE_F32;
        n = n.round();
        n *= Self::SCALE_F32.recip();*/
        Self::new_f32(n)
    }

    pub fn cos(self) -> Self {
        let n = self.as_f32().to_radians().cos();
        Self::new_f32(n)
    }

    pub fn tan(self) -> Self {
        Self::new_f32(self.as_f32().to_radians().tan())
    }

    pub fn asin(self) -> Self {
        Number::new_f32(self.as_f32().asin().to_degrees())
    }

    pub fn acos(self) -> Self {
        Number::new_f32(self.as_f32().acos().to_degrees())
    }

    pub fn atan(self) -> Self {
        Number::new_f32(self.as_f32().atan().to_degrees())
    }
}

impl From<bool> for Number {
    fn from(b: bool) -> Self {
        if b {
            Number::ONE
        } else {
            Number::ZERO
        }
    }
}

impl From<i64> for Number {
    fn from(n: i64) -> Self {
        Number(n * Self::SCALE)
    }
}

impl From<f64> for Number {
    fn from(n: f64) -> Self {
        Number::new(n)
    }
}

impl From<Number> for bool {
    fn from(n: Number) -> Self {
        n != Number::ZERO
    }
}

impl From<Number> for f64 {
    fn from(n: Number) -> Self {
        n.as_f64()
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{:.3}", self.as_f64())
    }
}

pub type NumberParseErr = core::num::ParseFloatError;

impl FromStr for Number {
    type Err = NumberParseErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let val: f64 = s.parse()?;
        if val > Self::MAX_VAL_F64 {
            Ok(Number::MAX)
        } else if val < Self::MIN_VAL_F64 {
            Ok(Number::MIN)
        } else {
            Ok(Number::new(val))
        }
    }
}

impl Add for Number {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Number(self.0.wrapping_add(rhs.0))
    }
}

impl AddAssign for Number {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl Sub for Number {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Number(self.0.wrapping_sub(rhs.0))
    }
}

impl SubAssign for Number {
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs;
    }
}

impl Neg for Number {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Number(self.0.overflowing_neg().0)
    }
}

impl Mul for Number {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Number(self.0.wrapping_mul(rhs.0) / Self::SCALE)
    }
}

impl MulAssign for Number {
    fn mul_assign(&mut self, rhs: Self) {
        *self = *self * rhs;
    }
}

impl Not for Number {
    type Output = Self;

    fn not(self) -> Self::Output {
        if self == Number::ZERO {
            Number::ONE
        } else {
            Number::ZERO
        }
    }
}

impl Div for Number {
    type Output = ValueResult<Number>;

    fn div(self, rhs: Self) -> Self::Output {
        if rhs.0 == 0 {
            Err(RuntimeErr::DivZero)
        } else {
            Ok(Number(self.0.wrapping_mul(Number::SCALE).wrapping_div(rhs.0)))
        }
    }
}

impl Rem for Number {
    type Output = ValueResult<Number>;

    fn rem(self, rhs: Self) -> Self::Output {
        if rhs.0 == 0 {
            Err(RuntimeErr::ModZero)
        } else {
            Ok(Number(self.0.wrapping_rem(rhs.0)))
        }
    }
}
