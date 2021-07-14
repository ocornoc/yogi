use super::*;

#[derive(Debug)]
pub enum Value {
    Number(Number),
    Str(YString),
}

impl Value {

    pub unsafe fn as_number_unchecked(&self) -> &Number {
        if let Value::Number(n) = self {
            n
        } else if cfg!(debug_assertions) {
            std::unreachable!()
        } else {
            std::hint::unreachable_unchecked()
        }
    }

    pub unsafe fn as_number_unchecked_mut(&mut self) -> &mut Number {
        if let Value::Number(n) = self {
            n
        } else if cfg!(debug_assertions) {
            std::unreachable!()
        } else {
            std::hint::unreachable_unchecked()
        }
    }

    pub unsafe fn as_ystring_unchecked(&self) -> &YString {
        if let Value::Str(s) = self {
            s
        } else if cfg!(debug_assertions) {
            std::unreachable!()
        } else {
            std::hint::unreachable_unchecked()
        }
    }

    pub unsafe fn as_ystring_unchecked_mut(&mut self) -> &mut YString {
        if let Value::Str(s) = self {
            s
        } else if cfg!(debug_assertions) {
            std::unreachable!()
        } else {
            std::hint::unreachable_unchecked()
        }
    }

    fn both_number(&mut self, other: Self) -> ValueResult<(&mut Number, Number)> {
        Err(RuntimeErr::Expected(ExpectedTy::Number, match (self, other) {
            (Value::Number(l), Value::Number(r)) => {
                return Ok((l, r));
            },
            (Value::Number(_), Value::Str(_)) => WrongArgType::Right,
            (Value::Str(_), Value::Number(_)) => WrongArgType::Left,
            (Value::Str(_), Value::Str(_)) => WrongArgType::Both,
        }))
    }

    fn as_number(&self) -> ValueResult<&Number> {
        match self {
            Value::Number(n) => Ok(n),
            Value::Str(_) => Err(RuntimeErr::Expected(ExpectedTy::Str, WrongArgType::Only)),
        }
    }

    pub fn eq(&self, other: &Self, buffer: &mut YString) -> bool {
        let (l, r) = match (self, other) {
            (Value::Number(l), Value::Number(r)) => { return l == r; },
            (Value::Number(l), Value::Str(r)) | (Value::Str(r), Value::Number(l)) =>
                (l.stringify(buffer) as &_, r),
            (Value::Str(l), Value::Str(r)) => (l, r),
        };
        l == r
    }

    pub fn le(&self, other: &Self, buffer: &mut YString) -> bool {
        let (l, r) = match (self, other) {
            (Value::Number(l), Value::Number(r)) => { return l <= r; },
            (Value::Number(l), Value::Str(r)) => (l.stringify(buffer) as &_, r),
            (Value::Str(l), Value::Number(r)) => (l, r.stringify(buffer) as &_),
            (Value::Str(l), Value::Str(r)) => (l, r),
        };
        l <= r
    }

    pub fn lt(&self, other: &Self, buffer: &mut YString) -> bool {
        let (l, r) = match (self, other) {
            (Value::Number(l), Value::Number(r)) => { return l < r; },
            (Value::Number(l), Value::Str(r)) => (l.stringify(buffer) as &_, r),
            (Value::Str(l), Value::Number(r)) => (l, r.stringify(buffer) as &_),
            (Value::Str(l), Value::Str(r)) => (l, r),
        };
        l < r
    }

    pub fn add_assign(&mut self, other: &Self, buffer: &mut YString) {
        match (&mut *self, other) {
            (Value::Number(l), &Value::Number(r)) => { *l += r; },
            (Value::Number(l), Value::Str(r)) => {
                let l: &mut YString = l.stringify(buffer);
                *l += r;
                *self = Value::Str(l.clone());
            },
            (Value::Str(l), Value::Number(r)) => {
                *l += r.stringify(buffer);
            },
            (Value::Str(l), Value::Str(r)) => {
                *l += r;
            },
        }
    }

    pub fn sub_assign(&mut self, other: &Self, buffer: &mut YString) {
        match (&mut *self, other) {
            (Value::Number(l), &Value::Number(r)) => { *l -= r; },
            (Value::Number(l), Value::Str(r)) => {
                let l: &mut YString = l.stringify(buffer).into();
                *l -= r;
                *self = Value::Str(l.clone());
            },
            (Value::Str(l), Value::Number(r)) => {
                *l -= r.stringify(buffer);
            },
            (Value::Str(l), Value::Str(r)) => {
                *l -= r;
            },
        }
    }

    pub fn mul_assign(&mut self, other: Self) -> ValueResult<()> {
        let (l, r) = self.both_number(other)?;
        *l *= r;
        Ok(())
    }

    pub fn div_assign(&mut self, other: Self) -> ValueResult<()> {
        let (l, r) = self.both_number(other)?;
        l.div_assign(r)?;
        Ok(())
    }

    pub fn rem_assign(&mut self, other: Self) -> ValueResult<()> {
        let (l, r) = self.both_number(other)?;
        l.rem_assign(r)?;
        Ok(())
    }

    pub fn pow_assign(&mut self, other: Self) -> ValueResult<()> {
        let (l, r) = self.both_number(other)?;
        l.pow_assign(r);
        Ok(())
    }

    pub fn pre_inc(&mut self) {
        match self {
            Value::Number(n) => n.pre_inc(),
            Value::Str(s) => s.insert(0, ' '),
        }
    }

    pub(super) fn as_mut_str_or_new(&mut self, cap: usize) -> &mut YString {
        if let Value::Str(s) = self {
            s.clear();
            s
        } else {
            *self = Value::Str(YString(String::with_capacity(cap)));
            // SAFE: we just made it a string
            unsafe { self.as_ystring_unchecked_mut() }
        }
    }

    pub fn post_inc(&mut self, out: &mut Value) {
        match self {
            Value::Number(n) => {
                *out = Value::Number(n.post_inc());
            },
            Value::Str(s) => {
                s.post_inc_v(out);
            },
        }
    }

    pub fn pre_dec(&mut self) -> ValueResult<()> {
        match self {
            Value::Number(n) => {
                n.pre_dec();
                Ok(())
            },
            Value::Str(s) => {
                s.pre_dec()
            },
        }
    }

    pub fn post_dec(&mut self, out: &mut Value) -> ValueResult<()> {
        match self {
            Value::Number(n) => {
                *out = Value::Number(n.post_dec());
                Ok(())
            },
            Value::Str(s) => {
                s.post_dec_v(out)
            },
        }
    }

    pub fn abs(&self) -> ValueResult<Number> {
        Ok(self.as_number()?.abs())
    }

    pub fn sqrt(&self) -> ValueResult<Number> {
        Ok(self.as_number()?.sqrt())
    }

    pub fn sin(&self) -> ValueResult<Number> {
        Ok(self.as_number()?.sin())
    }

    pub fn cos(&self) -> ValueResult<Number> {
        Ok(self.as_number()?.cos())
    }

    pub fn tan(&self) -> ValueResult<Number> {
        Ok(self.as_number()?.tan())
    }

    pub fn asin(&self) -> ValueResult<Number> {
        Ok(self.as_number()?.asin())
    }

    pub fn acos(&self) -> ValueResult<Number> {
        Ok(self.as_number()?.acos())
    }

    pub fn atan(&self) -> ValueResult<Number> {
        Ok(self.as_number()?.atan())
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Value::Number(n) => Value::Number(n.clone()),
            Value::Str(s) => Value::Str(s.clone()),
        }
    }

    fn clone_from(&mut self, source: &Self) {
        match (self, source) {
            (Value::Str(l), Value::Str(r)) => {
                l.clone_from(r);
            },
            (l, r) => {
                *l = r.clone();
            },
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Str(s) => write!(f, "{}", s.0),
        }
    }
}

impl Default for Value {
    fn default() -> Self {
        Value::Number(Number::ZERO)
    }
}

impl Not for Value {
    type Output = Number;

    fn not(self) -> Self::Output {
        if let Value::Number(n) = self {
            !n
        } else {
            Number::ZERO
        }
    }
}

impl From<YString> for Value {
    fn from(s: YString) -> Self {
        Value::Str(s)
    }
}

impl From<Number> for Value {
    fn from(n: Number) -> Self {
        Value::Number(n)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ExpectedTy {
    Number,
    Str,
}

impl Display for ExpectedTy {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.write_str(match self {
            ExpectedTy::Number => "number",
            ExpectedTy::Str => "string",
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub enum WrongArgType {
    Left,
    Right,
    Both,
    Only,
}

impl Display for WrongArgType {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.write_str(match self {
            WrongArgType::Left => "left arg",
            WrongArgType::Right => "right arg",
            WrongArgType::Both => "both args",
            WrongArgType::Only => "the arg",
        })
    }
}

#[derive(Debug, Clone, Copy, Error)]
pub enum RuntimeErr {
    #[error("expected a {0} in {1}")]
    Expected(ExpectedTy, WrongArgType),
    #[error("string was empty")]
    EmptyStr,
    #[error("division by zero")]
    DivZero,
    #[error("mod by zero")]
    ModZero,
}

pub type ValueResult<T> = Result<T, RuntimeErr>;