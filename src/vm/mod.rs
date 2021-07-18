use super::*;
use arith::*;
use crate::parser::{NUM_LINES, ast::*};
use std::array::IntoIter;
use std::fmt::Write;
use ahash::AHashMap;
use std::cell::{Cell, UnsafeCell};
use instr::*;
use std::mem::MaybeUninit;

const STRING_CAP: usize = 4096;

macro_rules! unop {
    ($self:ident, $arg:ident, $out:ident, $f:ident, $e1:expr, $e2:expr $(, )?) => { {
        if $arg == $out {
            // SAFE: only one mut ref is taken
            let $arg = unsafe { $self.$f($arg) };
            $e1
        } else {
            // SAFE: the two registers are different
            let $arg = unsafe { $self.$f($arg) };
            let $out = unsafe { $self.$f($out) };
            $e2
        }
        $self.set_next_instr();
    } };
}

macro_rules! unop_num {
    ($self:ident, $arg:ident, $out:ident, $e:expr $(, )?) => { {
        // SAFE: only one ref is taken
        let $arg = unsafe { $self.num_ref($arg) }.clone();
        // SAFE: still only one ref: we no longer use the ref from above by cloning
        *unsafe { $self.num_mut($out) } = $e;
        $self.set_next_instr();
    } };
}

macro_rules! binop {
    ($self:ident, $arg1:ident, $arg2:ident, $out:ident, $f:ident,
        $e1:expr, $e2:expr, $e3:expr, $e4:expr $(, )?) => { {
        if $arg1 == $out {
            if $arg1 == $arg2 {
                // SAFE: only one register taken
                let $arg1 = unsafe { $self.$f($arg1) };
                $e1
            } else {
                // SAFE: the two registers are different
                let $arg1 = unsafe { $self.$f($arg1) };
                let $arg2 = unsafe { $self.$f($arg2) };
                $e2
            }
        } else if $arg1 == $arg2 {
            // SAFE: the two registers are different
            let $arg1 = unsafe { $self.$f($arg1) };
            let $out = unsafe { $self.$f($out) };
            $e3
        } else {
            // SAFE: the three registers are different
            let $arg1 = unsafe { $self.$f($arg1) };
            let $arg2 = unsafe { $self.$f($arg2) };
            let $out = unsafe { $self.$f($out) };
            $e4
        }
        $self.set_next_instr();
    } };
}

macro_rules! cmp {
    ($self:ident, $arg1:ident, $arg2:ident, $out:ident, $e1:expr, $e2:expr $(, )?) => { {
        let result = if $arg1 == $arg2 {
            $e1
        } else {
            // SAFE: the two registers are different
            let $arg1 = unsafe { $self.val_ref($arg1) };
            let $arg2 = unsafe { $self.val_ref($arg2) };
            $e2
        };
        // SAFE: mut ref taken only after all other refs have been used
        *unsafe { $self.num_mut($out) } = result.into();
        $self.set_next_instr();
    } };
}

mod codegen;
mod instr;
pub mod analysis;

type Reg = u16;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
struct NumberReg(Reg);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
struct StringReg(Reg);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
struct ValueReg(Reg);

type Line = u8;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum AnyReg {
    Number(NumberReg),
    String(StringReg),
    Value(ValueReg),
}

impl From<NumberReg> for AnyReg {
    fn from(reg: NumberReg) -> Self {
        AnyReg::Number(reg)
    }
}

impl From<StringReg> for AnyReg {
    fn from(reg: StringReg) -> Self {
        AnyReg::String(reg)
    }
}

impl From<ValueReg> for AnyReg {
    fn from(reg: ValueReg) -> Self {
        AnyReg::Value(reg)
    }
}

impl AnyReg {
    fn into_val(self, vm: &mut VMExec) -> ValueReg {
        match self {
            AnyReg::Number(arg) => {
                let out = vm.new_val_reg(Value::Number(Number::ZERO));
                vm.code.push(Instr::move_nv(arg, out));
                out
            },
            AnyReg::String(arg) => {
                let out = vm.new_val_reg(Value::Number(Number::ZERO));
                vm.code.push(Instr::move_sv(arg, out));
                out
            },
            AnyReg::Value(arg) => arg,
        }
    }

    fn into_num(self, vm: &mut VMExec) -> NumberReg {
        let arg = match self {
            AnyReg::Number(arg) => return arg,
            AnyReg::String(_) => self.into_val(vm),
            AnyReg::Value(arg) => arg,
        };
        let out = vm.new_num_reg(Number::ZERO);
        vm.code.push(Instr::move_vn(arg, out));
        vm.code.push(Instr::jump_err());
        out
    }

    fn into_bool(self, vm: &mut VMExec) -> NumberReg {
        match self {
            AnyReg::Number(arg) => {
                let out = vm.new_num_reg(Number::ZERO);
                vm.code.push(Instr::bool_n(arg, out));
                out
            },
            AnyReg::String(_) => vm.new_num_reg(Number::ONE),
            AnyReg::Value(arg) => {
                let out = vm.new_num_reg(Number::ZERO);
                vm.code.push(Instr::bool_v(arg, out));
                out
            },
        }
    }
}

#[derive(Debug, Default)]
pub struct VMExec {
    code: Vec<Instr>,
    numbers: Vec<UnsafeCell<Number>>,
    strings: Vec<UnsafeCell<YString>>,
    values: Vec<UnsafeCell<Value>>,
    cur_line: Line,
    line_starts: [usize; NUM_LINES],
    next_instr: usize,
    globals: AHashMap<String, AnyReg>,
    string_buffer: UnsafeCell<YString>,
    runtime_err_flag: Cell<bool>,
    halt_flag: bool,
}

impl VMExec {
    fn new_num_reg(&mut self, val: Number) -> NumberReg {
        let reg = NumberReg(self.numbers.len() as u16);
        self.numbers.push(UnsafeCell::new(val));
        reg
    }

    fn new_string_reg(&mut self, val: YString) -> StringReg {
        let reg = StringReg(self.strings.len() as u16);
        self.strings.push(UnsafeCell::new(val));
        reg
    }

    fn new_val_reg(&mut self, val: Value) -> ValueReg {
        let reg = ValueReg(self.values.len() as u16);
        self.values.push(UnsafeCell::new(val));
        reg
    }

    #[inline]
    fn set_next_line(&mut self, line: usize) {
        self.next_instr = self.line_starts[line];
        self.halt_flag = true;
    }

    fn set_next_instr(&mut self) {
        if self.next_instr >= self.code.len() - 1 {
            self.set_next_line(0);
        } else {
            self.next_instr += 1;
        }
    }

    fn runtime_err(&mut self) {
        firestorm::profile_method!("runtime_err");
        let line = if self.cur_line == NUM_LINES as u8 - 1 {
            0
        } else {
            self.cur_line += 1;
            self.cur_line as usize
        };
        self.runtime_err_flag.set(false);
        self.set_next_line(line);
    }

    unsafe fn num_ref<'a>(&'a self, reg: NumberReg) -> &'a Number {
        let cell = if cfg!(debug_assertions) {
            &self.numbers[reg.0 as usize]
        } else {
            self.numbers.get_unchecked(reg.0 as usize)
        };
        &*cell.get()
    }

    unsafe fn num_mut<'a>(&'a self, reg: NumberReg) -> &'a mut Number {
        let cell = if cfg!(debug_assertions) {
            &self.numbers[reg.0 as usize]
        } else {
            self.numbers.get_unchecked(reg.0 as usize)
        };
        &mut *cell.get()
    }

    unsafe fn str_ref<'a>(&'a self, reg: StringReg) -> &'a YString {
        let cell = if cfg!(debug_assertions) {
            &self.strings[reg.0 as usize]
        } else {
            self.strings.get_unchecked(reg.0 as usize)
        };
        &*cell.get()
    }

    unsafe fn str_mut<'a>(&'a self, reg: StringReg) -> &'a mut YString {
        let cell = if cfg!(debug_assertions) {
            &self.strings[reg.0 as usize]
        } else {
            self.strings.get_unchecked(reg.0 as usize)
        };
        &mut *cell.get()
    }

    unsafe fn val_ref<'a>(&'a self, reg: ValueReg) -> &'a Value {
        let cell = if cfg!(debug_assertions) {
            &self.values[reg.0 as usize]
        } else {
            self.values.get_unchecked(reg.0 as usize)
        };
        &*cell.get()
    }

    unsafe fn val_mut<'a>(&'a self, reg: ValueReg) -> &'a mut Value {
        let cell = if cfg!(debug_assertions) {
            &self.values[reg.0 as usize]
        } else {
            self.values.get_unchecked(reg.0 as usize)
        };
        &mut *cell.get()
    }

    unsafe fn get_buffer<'a>(&'a self) -> &'a mut YString {
        &mut *self.string_buffer.get()
    }

    pub fn globals<'a>(&'a self) -> impl Iterator<Item=(&'a str, Value)> + 'a {
        self.globals
            .iter()
            .map(move |(s, &r)| (s.as_str(), match r {
                // SAFE: we only take immutable ref
                AnyReg::Number(reg) => unsafe { Value::Number(self.num_ref(reg).clone()) },
                // SAFE: we only take immutable ref
                AnyReg::String(reg) => unsafe { Value::Str(self.str_ref(reg).clone()) },
                // SAFE: we only take immutable ref
                AnyReg::Value(reg) => unsafe { self.val_ref(reg).clone() },
            }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_script(src: &str) -> VMExec {
        let script: parser::raw::Script = src.parse().unwrap();
        let script: parser::cst::Script = script.try_into().unwrap();
        let script: parser::pre_ast::Script = script.try_into().unwrap();
        let script: parser::ast::Script = script.try_into().unwrap();
        script.into()
    }

    #[test]
    fn string_test() {
        static TEST: &str = "\
            num=1 if \"\" then goto 19 end num++\n\
            if \"abc\" then goto 19 end num++\n\
            if \"1\" then goto 19 end num++\n\
            if \"0\" then goto 19 end num++\n\
            if not \"\" then goto 19 end num++\n\
            if not \"1\" then goto 19 end num++\n\
            if not \"0\" then goto 19 end num++\n\
            if 1 and \"\" then goto 19 end num++\n\
            if 1 and \"1\" then goto 19 end num++\n\
            if 1 and \"0\" then goto 19 end num++\n\
            if not (1 or \"\") then goto 19 end num++\n\
            if not (1 or \"1\") then goto 19 end num++\n\
            if not (1 or \"0\") then goto 19 end num++\n\
            if 0 or \"\" then goto 19 end num++\n\
            if 0 or \"1\" then goto 19 end num++\n\
            if 0 or \"0\" then goto 19 end num++\n\
            if num != 17 then :OUTPUT=\"Skipped: \"+(17-num)+\" tests\" goto 20 end\n\
            :OUTPUT=\"ok\" goto20\n\
            :output=\"Failed test #\"+num\n\
            goto20\n\
        ";
        let mut vm = make_script(TEST);
        for _ in 0..30 {
            vm.step();
        }
        let (name, output) = vm.globals().next().unwrap();
        debug_assert_eq!(name.to_ascii_lowercase(), "output");
        debug_assert_eq!(output, Value::Str("ok".to_string().into()));
    }
}
