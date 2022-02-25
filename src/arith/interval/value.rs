use super::*;

macro_rules! num_binop_assign {
    ($trait:tt, $fn:ident, $traitassign:tt, $fn_assign:ident) => {
        impl $traitassign<&ValueInterval> for ValueInterval {
            fn $fn_assign(&mut self, rhs: &Self) {
                let type_error = self.can_be_string() || rhs.can_be_string();
                self.strings = None;
                self.numbers.$fn_assign(&rhs.numbers);
                self.numbers.runtime_error |= type_error;
            }
        }

        impl $trait<&ValueInterval> for ValueInterval {
            type Output = Self;

            fn $fn(mut self, rhs: &Self) -> Self {
                self.$fn_assign(rhs);
                self
            }
        }
    };
}

macro_rules! num_unop {
    ($fn:ident) => {
        pub fn $fn(&mut self) {
            let type_error = self.can_be_string();
            self.strings = None;
            self.numbers.$fn();
            self.numbers.runtime_error |= type_error;
        }
    };
}

#[derive(Debug, Clone, PartialEq, Eq, AsRef)]
pub struct ValueInterval {
    #[as_ref]
    numbers: NumberIntervals,
    strings: Option<StringInterval>,
}

impl ValueInterval {
    pub fn everything() -> Self {
        ValueInterval {
            numbers: NumberIntervals::everything(),
            strings: Some(StringInterval::everything()),
        }
    }

    pub fn uninitialized() -> Self {
        ValueInterval {
            numbers: Number::ZERO.into(),
            strings: None,
        }
    }

    pub fn nothing() -> Self {
        NumberIntervals::nothing().into()
    }

    pub fn reset_runtime_err(&mut self) {
        self.numbers.reset_runtime_err();
        if let Some(ref mut string_intervals) = self.strings {
            string_intervals.reset_runtime_err();
        }
    }

    pub fn could_runtime_err(&self) -> bool {
        self.numbers.runtime_error || if let Some(ref string_intervals) = self.strings {
            string_intervals.runtime_error
        } else {
            false
        }
    }

    pub fn disable_strings(&mut self) {
        self.strings = None;
    }

    pub fn disable_numbers(&mut self) {
        self.numbers = NumberIntervals {
            runtime_error: self.numbers.runtime_error,
            ..NumberIntervals::nothing()
        };
    }

    pub fn can_be_number(&self) -> bool {
        !self.numbers.intervals.is_empty()
    }

    pub fn can_be_string(&self) -> bool {
        self.strings.is_some()
    }

    pub fn can_type_mismatch(&self, other: &Self) -> bool {
        (self.can_be_number() && other.can_be_string())
            || (self.can_be_string() && other.can_be_number())
    }

    pub fn stringify(&self) -> Option<StringInterval> {
        if let Some(mut strings) = self.numbers.stringify() {
            if let Some(ref real_strings) = self.strings {
                strings.union(real_strings);
            }
            Some(strings)
        } else {
            self.strings.clone()
        }
    }

    fn try_stringify_binop(&self, other: &Self) -> Option<(StringInterval, StringInterval)> {
        Some((self.stringify()?, other.stringify()?))
    }

    pub fn pow_assign(&mut self, rhs: &Self) {
        let type_error = self.can_be_string() || rhs.can_be_string();
        self.strings = None;
        self.numbers.pow_assign(&rhs.numbers);
        self.numbers.runtime_error |= type_error;
    }

    pub fn pre_inc(&mut self) {
        self.numbers.pre_inc();
        if let Some(ref mut strings) = self.strings {
            strings.pre_inc();
        }
    }

    pub fn pre_dec(&mut self) {
        self.numbers.pre_dec();
        if let Some(ref mut strings) = self.strings {
            strings.pre_dec();
        }
    }

    num_unop!(abs);
    num_unop!(fact);
    num_unop!(sqrt);
    num_unop!(sin);
    num_unop!(cos);
    num_unop!(tan);
    num_unop!(asin);
    num_unop!(acos);
    num_unop!(atan);

    pub fn int_eq(&self, other: &Self) -> NumberIntervals {
        !self.int_ne(other)
    }

    pub fn int_ne(&self, other: &Self) -> NumberIntervals {
        let mut possibilities = NumberIntervals::nothing();
        if self.can_be_number() && other.can_be_number() {
            possibilities |= &self.numbers.int_ne(&other.numbers);
        }
        if let (Some(lhs), Some(rhs)) = (&self.strings, &other.strings) {
            possibilities |= &lhs.int_ne(rhs);
        }
        if self.can_type_mismatch(other) {
            possibilities |= &Number::from(true).into();
        }
        possibilities
    }

    pub fn int_le(&self, other: &Self) -> NumberIntervals {
        let mut possibilities = NumberIntervals::nothing();
        if self.can_be_number() && other.can_be_number() {
            possibilities |= &self.numbers.int_le(&other.numbers);
        }
        if let Some((lhs, rhs)) = self.try_stringify_binop(other) {
            possibilities |= &lhs.int_le(&rhs);
        }
        possibilities
    }

    pub fn int_lt(&self, other: &Self) -> NumberIntervals {
        let mut possibilities = NumberIntervals::nothing();
        if self.can_be_number() && other.can_be_number() {
            possibilities |= &self.numbers.int_lt(&other.numbers);
        }
        if let Some((lhs, rhs)) = self.try_stringify_binop(other) {
            possibilities |= &lhs.int_lt(&rhs);
        }
        possibilities
    }

    pub fn int_ge(&self, other: &Self) -> NumberIntervals {
        let mut possibilities = NumberIntervals::nothing();
        if self.can_be_number() && other.can_be_number() {
            possibilities |= &self.numbers.int_ge(&other.numbers);
        }
        if let Some((lhs, rhs)) = self.try_stringify_binop(other) {
            possibilities |= &lhs.int_ge(&rhs);
        }
        possibilities
    }

    pub fn int_gt(&self, other: &Self) -> NumberIntervals {
        let mut possibilities = NumberIntervals::nothing();
        if self.can_be_number() && other.can_be_number() {
            possibilities |= &self.numbers.int_gt(&other.numbers);
        }
        if let Some((lhs, rhs)) = self.try_stringify_binop(other) {
            possibilities |= &lhs.int_gt(&rhs);
        }
        possibilities
    }

    pub fn as_bool(&self) -> BoolInterval {
        let mut bools = self.numbers.as_bool();
        if self.can_be_string() {
            bools |= false.into();
        }
        bools
    }

    pub fn set_to_bool(&mut self, bools: BoolInterval) {
        self.strings = None;
        self.numbers.set_to_bool(bools);
    }
}

impl Default for ValueInterval {
    fn default() -> Self {
        Self::everything()
    }
}

impl Display for ValueInterval {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        let re = self.could_runtime_err();
        if let Some(ref string_intervals) = self.strings {
            write!(f, "Numbers: {}, Strings: {string_intervals}, RE: {re}", self.numbers)
        } else {
            write!(f, "Numbers: {}, Strings: ∅, RE: {re}", self.numbers)
        }
    }
}

impl From<NumberIntervals> for ValueInterval {
    fn from(numbers: NumberIntervals) -> Self {
        ValueInterval {
            numbers,
            strings: None,
        }
    }
}

impl From<StringInterval> for ValueInterval {
    fn from(strings: StringInterval) -> Self {
        ValueInterval {
            numbers: NumberIntervals::nothing(),
            strings: Some(strings),
        }
    }
}

impl From<BoolInterval> for ValueInterval {
    fn from(bools: BoolInterval) -> Self {
        ValueInterval {
            numbers: bools.into(),
            strings: None,
        }
    }
}

impl From<Number> for ValueInterval {
    fn from(n: Number) -> Self {
        let numbers: NumberIntervals = n.into();
        numbers.into()
    }
}

impl From<YString> for ValueInterval {
    fn from(ys: YString) -> Self {
        StringInterval::from(ys).into()
    }
}

impl From<Value> for ValueInterval {
    fn from(v: Value) -> Self {
        match v {
            Value::Num(n) => n.into(),
            Value::Str(s) => s.into(),
        }
    }
}

impl From<&str> for ValueInterval {
    fn from(s: &str) -> Self {
        let strings: StringInterval = s.into();
        strings.into()
    }
}

impl AddAssign<&ValueInterval> for ValueInterval {
    fn add_assign(&mut self, rhs: &ValueInterval) {
        if let Some((lhs, rhs)) = self.try_stringify_binop(rhs) {
            self.strings = Some(lhs + &rhs);
        }
        if self.can_be_number() && rhs.can_be_number() {
            self.numbers += &rhs.numbers;
        }
    }
}

impl Add<&ValueInterval> for ValueInterval {
    type Output = Self;

    fn add(mut self, rhs: &ValueInterval) -> Self::Output {
        self += rhs;
        self
    }
}

impl SubAssign<&ValueInterval> for ValueInterval {
    fn sub_assign(&mut self, rhs: &ValueInterval) {
        if let Some((lhs, rhs)) = self.try_stringify_binop(rhs) {
            self.strings = Some(lhs + &rhs);
        }
        if self.can_be_number() && rhs.can_be_number() {
            self.numbers += &rhs.numbers;
        }
    }
}

impl Sub<&ValueInterval> for ValueInterval {
    type Output = Self;

    fn sub(mut self, rhs: &ValueInterval) -> Self::Output {
        self -= rhs;
        self
    }
}

num_binop_assign!(Mul, mul, MulAssign, mul_assign);
num_binop_assign!(Div, div, DivAssign, div_assign);
num_binop_assign!(Rem, rem, RemAssign, rem_assign);

impl Neg for &mut ValueInterval {
    type Output = ();

    fn neg(self) -> Self::Output {
        let type_error = self.can_be_string();
        self.strings = None;
        -&mut self.numbers;
        self.numbers.runtime_error |= type_error;
    }
}

impl Neg for ValueInterval {
    type Output = Self;

    fn neg(mut self) -> Self::Output {
        -&mut self;
        self
    }
}

impl Not for &mut ValueInterval {
    type Output = ();

    fn not(self) -> Self::Output {
        self.set_to_bool(!self.as_bool());
    }
}

impl Not for ValueInterval {
    type Output = Self;

    fn not(self) -> Self::Output {
        (!self.as_bool()).into()
    }
}

impl BitAndAssign<&ValueInterval> for ValueInterval {
    fn bitand_assign(&mut self, rhs: &ValueInterval) {
        let bools = self.as_bool() & rhs.as_bool();
        self.set_to_bool(bools);
    }
}

impl BitAnd<&ValueInterval> for ValueInterval {
    type Output = Self;

    fn bitand(self, rhs: &ValueInterval) -> Self::Output {
        (self.as_bool() & rhs.as_bool()).into()
    }
}

impl BitAndAssign<Number> for ValueInterval {
    fn bitand_assign(&mut self, rhs: Number) {
        let bools = self.as_bool() & rhs.into();
        self.set_to_bool(bools);
    }
}

impl BitAnd<Number> for ValueInterval {
    type Output = Self;

    fn bitand(self, rhs: Number) -> Self::Output {
        (self.as_bool() & rhs.into()).into()
    }
}

impl BitOrAssign<&ValueInterval> for ValueInterval {
    fn bitor_assign(&mut self, rhs: &ValueInterval) {
        let bools = self.as_bool() | rhs.as_bool();
        self.set_to_bool(bools);
    }
}

impl BitOr<&ValueInterval> for ValueInterval {
    type Output = Self;

    fn bitor(self, rhs: &ValueInterval) -> Self::Output {
        (self.as_bool() | rhs.as_bool()).into()
    }
}

impl BitOrAssign<Number> for ValueInterval {
    fn bitor_assign(&mut self, rhs: Number) {
        let bools = self.as_bool() | rhs.into();
        self.set_to_bool(bools);
    }
}

impl BitOr<Number> for ValueInterval {
    type Output = Self;

    fn bitor(self, rhs: Number) -> Self::Output {
        (self.as_bool() | rhs.into()).into()
    }
}
