use super::*;
use arith::*;
use crate::parser::{NUM_LINES, ast::*};
use std::array::IntoIter;
use ahash::AHashMap;
use std::cell::UnsafeCell;

const STRING_CAP: usize = 2_usize.pow(16);

mod codegen;

type Reg = u16;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct NumberReg(Reg);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct StringReg(Reg);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ValueReg(Reg);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Label(Reg);

type Line = u8;

#[derive(Debug, Clone)]
enum Instr {
    LineStart(Line),
    JumpRel(isize),
    JumpLine(NumberReg),
    MoveSV { arg: StringReg, out: ValueReg },
    MoveNV { arg: NumberReg, out: ValueReg },
    MoveVV { arg: ValueReg, out: ValueReg },
    MoveVS { arg: ValueReg, out: StringReg },
    MoveVN { arg: ValueReg, out: NumberReg },
    StringifyN { arg: NumberReg, out: StringReg },
    StringifyV { val: ValueReg, out: StringReg },
    AddS { arg1: StringReg, arg2: StringReg, out: StringReg },
    AddN { arg1: NumberReg, arg2: NumberReg, out: NumberReg },
    AddV { arg1: ValueReg, arg2: ValueReg, out: ValueReg },
    SubS { arg1: StringReg, arg2: StringReg, out: StringReg },
    SubN { arg1: NumberReg, arg2: NumberReg, out: NumberReg },
    SubV { arg1: ValueReg, arg2: ValueReg, out: ValueReg },
    IncS { arg: StringReg, out: StringReg },
    IncN { arg: NumberReg, out: NumberReg },
    IncV { arg: ValueReg, out: ValueReg },
    DecS { arg: StringReg, out: StringReg },
    DecN { arg: NumberReg, out: NumberReg },
    DecV { arg: ValueReg, out: ValueReg },
    IsTruthyS { arg: StringReg, out: NumberReg },
    IsTruthyN { arg: NumberReg, out: NumberReg },
}

#[derive(Debug, Clone, Copy)]
enum AnyReg {
    Number(NumberReg),
    String(StringReg),
    Value(ValueReg),
}

impl AnyReg {
    fn into_val(self, vm: &mut VMExec) -> ValueReg {
        match self {
            AnyReg::Number(arg) => {
                let out = vm.new_val_reg(Value::Number(Number::ZERO));
                vm.code.push(Instr::MoveNV { arg, out });
                out
            },
            AnyReg::String(arg) => {
                let out = vm.new_val_reg(Value::Number(Number::ZERO));
                vm.code.push(Instr::MoveSV { arg, out });
                out
            },
            AnyReg::Value(arg) => arg,
        }
    }
}

macro_rules! inc_dec {
    ($self:ident, $arg:ident, $out:ident, $f:ident, $e:expr) => {
        {
            if $arg == $out {
                // SAFE: only one mut ref is taken
                unsafe { $self.$f($arg).pre_inc() };
            } else {
                // SAFE: the two registers are different
                let $arg = unsafe { $self.$f($arg) };
                let $out = unsafe { $self.$f($out) };
                $e;
            }
            $self.set_next_instr();
        }
    };
}

macro_rules! binop {
    ($self:ident, $arg1:ident, $arg2:ident, $out:ident, $f:ident,
        $e1:expr, $e2:expr, $e3:expr, $e4:expr $(, )?) => {
        {
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
        }
    };
}

#[derive(Debug, Default)]
pub struct VMExec {
    code: Vec<Instr>,
    numbers: Vec<UnsafeCell<Number>>,
    strings: Vec<UnsafeCell<YString>>,
    values: Vec<UnsafeCell<Value>>,
    cur_line: Line,
    line_starts: [usize; NUM_LINES],
    line_stats: [u32; NUM_LINES],
    next_instr: usize,
    globals: AHashMap<String, AnyReg>,
    string_buffer: UnsafeCell<YString>,
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
    }

    fn set_next_instr(&mut self) {
        if self.next_instr >= self.code.len() - 1 {
            self.set_next_line(0);
        } else {
            self.next_instr += 1;
        }
    }

    fn runtime_err(&mut self) {
        let line = if self.cur_line == NUM_LINES as u8 - 1 {
            0
        } else {
            self.cur_line += 1;
            self.cur_line as usize
        };
        self.set_next_line(line);
    }

    unsafe fn num_mut<'a>(&'a self, reg: NumberReg) -> &'a mut Number {
        let cell = if cfg!(debug_assertions) {
            &self.numbers[reg.0 as usize]
        } else {
            self.numbers.get_unchecked(reg.0 as usize)
        };
        &mut *cell.get()
    }

    unsafe fn str_mut<'a>(&'a self, reg: StringReg) -> &'a mut YString {
        let cell = if cfg!(debug_assertions) {
            &self.strings[reg.0 as usize]
        } else {
            self.strings.get_unchecked(reg.0 as usize)
        };
        &mut *cell.get()
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

    fn step_aux(&mut self) -> Option<bool> {
        match self.code[self.next_instr] {
            Instr::LineStart(line) => {
                self.cur_line = line;
                self.line_stats[line as usize] += 1;
                self.set_next_instr();
            },
            Instr::JumpRel(i) => if i.is_negative() {
                self.next_instr -= i.abs() as usize;
            } else {
                self.next_instr += i as usize;
            },
            Instr::JumpLine(reg) => {
                // SAFE: only one ref taken
                let result = unsafe { self.num_mut(reg).as_f32() };
                let next_line = (result.floor() as usize).clamp(1, NUM_LINES) - 1;
                self.set_next_line(next_line);
                return Some(true);
            },
            Instr::MoveSV { arg, out } => unsafe {
                let s = self.str_mut(arg);
                let value = self.val_mut(out);
                if let Value::Str(old) = value {
                    old.clone_from(s);
                } else {
                    *value = Value::Str(s.clone());
                }
                self.set_next_instr();
            },
            Instr::MoveNV { arg, out } => unsafe {
                *self.val_mut(out) = Value::Number(*self.num_mut(arg));
                self.set_next_instr();
            },
            Instr::MoveVV { arg, out } => todo!(),
            Instr::MoveVS { arg, out } => todo!(),
            Instr::MoveVN { arg, out } => if let Value::Number(n) = unsafe { self.val_mut(arg) } {
                unsafe { *self.num_mut(out) = *n };
                self.set_next_instr();
            } else {
                return None;
            },
            Instr::StringifyN { arg, out } => todo!(),
            Instr::StringifyV { val, out } => todo!(),
            Instr::AddS { arg1, arg2, out } => binop!(self, arg1, arg2, out, str_mut,
                *arg1 = YString(arg1.repeat(2)),
                *arg1 += arg2,
                { out.clone_from(arg1); *out += arg1 },
                { out.clone_from(arg1); *out += arg2 },
            ),
            Instr::AddN { arg1, arg2, out } => binop!(self, arg1, arg2, out, num_mut,
                *arg1 += *arg1,
                *arg1 += *arg2,
                *out = *arg1 + *arg1,
                *out = *arg1 + *arg2,
            ),
            Instr::AddV { arg1, arg2, out } => binop!(self, arg1, arg2, out, val_mut,
                match arg1 {
                    Value::Number(n) => *n += *n,
                    Value::Str(s) => *s = YString(s.repeat(2)),
                },
                arg1.add_assign(arg2, unsafe { self.get_buffer() }),
                { out.clone_from(arg1); out.add_assign(arg1, unsafe { self.get_buffer() }) },
                { out.clone_from(arg1); out.add_assign(arg2, unsafe { self.get_buffer() }) },
            ),
            Instr::SubS { arg1, arg2, out } => todo!(),
            Instr::SubN { arg1, arg2, out } => todo!(),
            Instr::SubV { arg1, arg2, out } => todo!(),
            Instr::IncS { arg, out } => 
                inc_dec!(self, arg, out, str_mut, arg.post_inc_s(out)),
            Instr::IncN { arg, out } =>
                inc_dec!(self, arg, out, num_mut, *out = arg.post_inc()),
            Instr::IncV { arg, out } =>
                inc_dec!(self, arg, out, val_mut, arg.post_inc(out)),
            Instr::DecS { arg, out } =>
                inc_dec!(self, arg, out, str_mut, arg.post_dec_s(out).ok()?),
            Instr::DecN { arg, out } =>
                inc_dec!(self, arg, out, num_mut, *out = arg.post_dec()),
            Instr::DecV { arg, out } =>
                inc_dec!(self, arg, out, val_mut, arg.post_dec(out).ok()?),
            Instr::IsTruthyS { arg, out } => todo!(),
            Instr::IsTruthyN { arg, out } => todo!(),
        };
        Some(false)
    }

    pub fn step(&mut self) {

        let init_line = self.cur_line;
        while self.cur_line == init_line {
            match self.step_aux() {
                Some(true) => return,
                Some(false) => (),
                None => self.runtime_err(),
            }
        }
    }

    pub fn globals<'a>(&'a self) -> impl Iterator<Item=(&'a str, Value)> + 'a {
        self.globals
            .iter()
            .map(move |(s, &r)| (s.as_str(), match r {
                AnyReg::Number(reg) => unsafe { Value::Number(self.num_mut(reg).clone()) },
                AnyReg::String(reg) => unsafe { Value::Str(self.str_mut(reg).clone()) },
                AnyReg::Value(reg) => unsafe { self.val_mut(reg).clone() },
            }))
    }
}
