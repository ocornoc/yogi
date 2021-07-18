use super::*;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[repr(transparent)]
pub struct YString(pub String);

impl YString {
    pub fn pre_inc(&mut self) {
        self.push(' ');
    }

    pub fn post_inc_v(&mut self, out: &mut Value) {
        let buffer = out.as_mut_str_or_new(self.len() + 1);
        buffer.0 += self.as_str();
        buffer.push(' ');
        std::mem::swap(buffer, self);
    }

    pub fn post_inc_s(&mut self, out: &mut YString) {
        out.clone_from(self);
        out.push(' ');
        std::mem::swap(out, self);
    }

    pub fn pre_dec(&mut self) -> ValueResult<()> {
        if self.pop().is_some() {
            Ok(())
        } else {
            Err(RuntimeErr::EmptyStr)
        }
    }

    pub fn post_dec_v(&mut self, out: &mut Value) -> ValueResult<()> {
        if self.is_empty() {
            Err(RuntimeErr::EmptyStr)
        } else {
            let buffer = out.as_mut_str_or_new(self.len() - 1);
            buffer.0 += self.as_str();
            buffer.pop();
            std::mem::swap(buffer, self);
            Ok(())
        }
    }

    pub fn post_dec_s(&mut self, out: &mut YString) -> ValueResult<()> {
        if self.is_empty() {
            Err(RuntimeErr::EmptyStr)
        } else {
            out.clone_from(self);
            out.pop();
            std::mem::swap(out, self);
            Ok(())
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

impl From<String> for YString {
    fn from(s: String) -> Self {
        YString(s)
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
