use super::*;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[repr(transparent)]
pub struct YString(pub String);

impl YString {
    pub fn pre_inc(&mut self) {
        self.push(' ');
    }

    pub fn pre_dec(&mut self) -> ValueResult<()> {
        if self.pop().is_some() {
            Ok(())
        } else {
            Err(RuntimeErr::EmptyStr)
        }
    }
}

impl Clone for YString {
    fn clone(&self) -> Self {
        YString(self.0.clone())
    }

    fn clone_from(&mut self, source: &Self) {
        self.0.clone_from(source);
    }
}

impl Display for YString {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "\"{}\"", self.0.escape_debug())
    }
}

impl<T: Into<String>> From<T> for YString {
    fn from(s: T) -> Self {
        YString(s.into())
    }
}

impl<'a> From<&'a String> for &'a YString {
    fn from(s: &'a String) -> Self {
        // SAFE: Ystring is repr(transparent)
        unsafe { std::mem::transmute(s) }
    }
}

impl<'a> From<&'a mut String> for &'a mut YString {
    fn from(s: &'a mut String) -> Self {
        // SAFE: Ystring is repr(transparent)
        unsafe { std::mem::transmute(s) }
    }
}

impl Deref for YString {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for YString {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl AddAssign<&'_ Self> for YString {
    fn add_assign(&mut self, rhs: &Self) {
        self.0 += rhs;
    }
}

impl SubAssign<&'_ Self> for YString {
    fn sub_assign(&mut self, rhs: &Self) {
        if let Some(i) = self.rfind(rhs.as_str()) {
            self.drain(i..i + rhs.len());
        }
    }
}
