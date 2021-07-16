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
    JumpRel { amount: usize, condition: Option<NumberReg> },
    JumpLine(NumberReg),
    MoveSV { arg: StringReg, out: ValueReg },
    MoveNV { arg: NumberReg, out: ValueReg },
    MoveVV { arg: ValueReg, out: ValueReg },
    //MoveVS { arg: ValueReg, out: StringReg },
    MoveVN { arg: ValueReg, out: NumberReg },
    //StringifyN { arg: NumberReg, out: StringReg },
    //StringifyV { val: ValueReg, out: StringReg },
    AddS { arg1: StringReg, arg2: StringReg, out: StringReg },
    AddN { arg1: NumberReg, arg2: NumberReg, out: NumberReg },
    AddV { arg1: ValueReg, arg2: ValueReg, out: ValueReg },
    SubS { arg1: StringReg, arg2: StringReg, out: StringReg },
    SubN { arg1: NumberReg, arg2: NumberReg, out: NumberReg },
    SubV { arg1: ValueReg, arg2: ValueReg, out: ValueReg },
    Mul { arg1: NumberReg, arg2: NumberReg, out: NumberReg },
    Div { arg1: NumberReg, arg2: NumberReg, out: NumberReg },
    Mod { arg1: NumberReg, arg2: NumberReg, out: NumberReg },
    Pow { arg1: NumberReg, arg2: NumberReg, out: NumberReg },
    Eq { arg1: ValueReg, arg2: ValueReg, out: NumberReg },
    Le { arg1: ValueReg, arg2: ValueReg, out: NumberReg },
    Lt { arg1: ValueReg, arg2: ValueReg, out: NumberReg },
    IncS { arg: StringReg, out: StringReg },
    IncN { arg: NumberReg, out: NumberReg },
    IncV { arg: ValueReg, out: ValueReg },
    DecS { arg: StringReg, out: StringReg },
    DecN { arg: NumberReg, out: NumberReg },
    DecV { arg: ValueReg, out: ValueReg },
    Abs { arg: NumberReg, out: NumberReg },
    Fact { arg: NumberReg, out: NumberReg },
    Sqrt { arg: NumberReg, out: NumberReg },
    Sin { arg: NumberReg, out: NumberReg },
    Cos { arg: NumberReg, out: NumberReg },
    Tan { arg: NumberReg, out: NumberReg },
    Asin { arg: NumberReg, out: NumberReg },
    Acos { arg: NumberReg, out: NumberReg },
    Atan { arg: NumberReg, out: NumberReg },
    Neg { arg: NumberReg, out: NumberReg },
    And { arg1: NumberReg, arg2: NumberReg, out: NumberReg },
    Or { arg1: NumberReg, arg2: NumberReg, out: NumberReg },
    Not { arg: NumberReg, out: NumberReg },
    BoolN { arg: NumberReg, out: NumberReg },
    BoolV { arg: ValueReg, out: NumberReg },
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

    fn into_num(self, vm: &mut VMExec) -> NumberReg {
        let arg = match self {
            AnyReg::Number(arg) => return arg,
            AnyReg::String(_) => self.into_val(vm),
            AnyReg::Value(arg) => arg,
        };
        let out = vm.new_num_reg(Number::ZERO);
        vm.code.push(Instr::MoveVN { arg, out });
        out
    }

    fn into_bool(self, vm: &mut VMExec) -> NumberReg {
        match self {
            AnyReg::Number(arg) => {
                let out = vm.new_num_reg(Number::ZERO);
                vm.code.push(Instr::BoolN { arg, out });
                out
            },
            AnyReg::String(_) => vm.new_num_reg(Number::ONE),
            AnyReg::Value(arg) => {
                let out = vm.new_num_reg(Number::ZERO);
                vm.code.push(Instr::BoolV { arg, out });
                out
            },
        }
    }
}

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
            $e2;
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
    } };
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

    fn step_aux(&mut self) -> Option<bool> {
        match self.code[self.next_instr] {
            Instr::LineStart(line) => {
                self.cur_line = line;
                self.line_stats[line as usize] += 1;
                self.set_next_instr();
            },
            Instr::JumpRel { amount, condition: Some(condition) } =>
                // SAFE: only one ref taken
                if unsafe { self.num_mut(condition).as_bool() } {
                    self.next_instr += amount;
                },
            Instr::JumpRel { amount, condition: None } => {
                self.next_instr += amount;
            },
            Instr::JumpLine(reg) => {
                // SAFE: only one ref taken
                let result = unsafe { self.num_mut(reg).as_f32() };
                let next_line = (result.floor() as usize).clamp(1, NUM_LINES) - 1;
                self.set_next_line(next_line);
                return Some(true);
            },
            // SAFE: all registers are guaranteed not to alias
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
            // SAFE: all registers are guaranteed not to alias
            Instr::MoveNV { arg, out } => unsafe {
                *self.val_mut(out) = Value::Number(*self.num_mut(arg));
                self.set_next_instr();
            },
            Instr::MoveVV { arg, out } => if arg != out {
                // SAFE: all registers are guaranteed not to alias
                unsafe { self.val_mut(out).clone_from(self.val_mut(arg)) };
            },
            /*// SAFE: all registers are guaranteed not to alias
            Instr::MoveVS { arg, out } => if let Value::Str(s) = unsafe { self.val_mut(arg) } {
                unsafe { self.str_mut(out).clone_from(s) };
                self.set_next_instr();
            } else {
                return None;
            },*/
            // SAFE: all registers are guaranteed not to alias
            Instr::MoveVN { arg, out } => if let Value::Number(n) = unsafe { self.val_mut(arg) } {
                unsafe { *self.num_mut(out) = *n };
                self.set_next_instr();
            } else {
                return None;
            },
            //Instr::StringifyN { arg, out } => todo!(),
            //Instr::StringifyV { val, out } => todo!(),
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
            Instr::SubS { arg1, arg2, out } => binop!(self, arg1, arg2, out, str_mut,
                *arg1 -= {
                    let buffer = unsafe { self.get_buffer() };
                    buffer.clone_from(arg1);
                    buffer
                },
                *arg1 -= arg2,
                { out.clone_from(arg1); *out -= arg1 },
                { out.clone_from(arg1); *out -= arg2 },
            ),
            Instr::SubN { arg1, arg2, out } => binop!(self, arg1, arg2, out, num_mut,
                *arg1 -= *arg1,
                *arg1 -= *arg2,
                *out = *arg1 - *arg1,
                *out = *arg1 - *arg2,
            ),
            Instr::SubV { arg1, arg2, out } => binop!(self, arg1, arg2, out, val_mut,
                match arg1 {
                    Value::Number(n) => *n -= *n,
                    Value::Str(s) => *s -= {
                        let buffer = unsafe { self.get_buffer() };
                        buffer.clone_from(s);
                        buffer
                    },
                },
                arg1.sub_assign(arg2, unsafe { self.get_buffer() }),
                { out.clone_from(arg1); out.sub_assign(arg1, unsafe { self.get_buffer() }) },
                { out.clone_from(arg1); out.sub_assign(arg2, unsafe { self.get_buffer() }) },
            ),
            Instr::IncS { arg, out } => 
                unop!(self, arg, out, str_mut, arg.pre_inc(), arg.post_inc_s(out)),
            Instr::IncN { arg, out } =>
                unop!(self, arg, out, num_mut, arg.pre_inc(), *out = arg.post_inc()),
            Instr::IncV { arg, out } =>
                unop!(self, arg, out, val_mut, arg.pre_inc(), arg.post_inc(out)),
            Instr::DecS { arg, out } =>
                unop!(self, arg, out, str_mut, arg.pre_dec().ok()?, arg.post_dec_s(out).ok()?),
            Instr::DecN { arg, out } =>
                unop!(self, arg, out, num_mut, arg.pre_dec(), *out = arg.post_dec()),
            Instr::DecV { arg, out } =>
                unop!(self, arg, out, val_mut, arg.pre_dec().ok()?, arg.post_dec(out).ok()?),
            Instr::Mul { arg1, arg2, out } => binop!(self, arg1, arg2, out, num_mut,
                *arg1 *= *arg1,
                *arg1 *= *arg2,
                *out = *arg1 * *arg1,
                *out = *arg1 * *arg2,
            ),
            Instr::Div { arg1, arg2, out } => binop!(self, arg1, arg2, out, num_mut,
                arg1.div_assign(arg1.clone()).ok()?,
                arg1.div_assign(arg2.clone()).ok()?,
                *out = (*arg1 / *arg1).ok()?,
                *out = (*arg1 / *arg2).ok()?,
            ),
            Instr::Mod { arg1, arg2, out } => binop!(self, arg1, arg2, out, num_mut,
                arg1.rem_assign(arg1.clone()).ok()?,
                arg1.rem_assign(arg2.clone()).ok()?,
                *out = (*arg1 % *arg1).ok()?,
                *out = (*arg1 % *arg2).ok()?,
            ),
            Instr::Pow { arg1, arg2, out } => binop!(self, arg1, arg2, out, num_mut,
                arg1.pow_assign(arg1.clone()),
                arg1.pow_assign(arg2.clone()),
                *out = *arg1 * *arg1,
                *out = *arg1 * *arg2,
            ),
            Instr::Abs { arg, out } => unop_num!(self, arg, out, arg.abs()),
            Instr::Fact { arg, out } => unop_num!(self, arg, out, arg.fact()),
            Instr::Sqrt { arg, out } => unop_num!(self, arg, out, arg.sqrt()),
            Instr::Sin { arg, out } => unop_num!(self, arg, out, arg.sin()),
            Instr::Cos { arg, out } => unop_num!(self, arg, out, arg.cos()),
            Instr::Tan { arg, out } => unop_num!(self, arg, out, arg.tan()),
            Instr::Asin { arg, out } => unop_num!(self, arg, out, arg.asin()),
            Instr::Acos { arg, out } => unop_num!(self, arg, out, arg.acos()),
            Instr::Atan { arg, out } => unop_num!(self, arg, out, arg.atan()),
            Instr::Neg { arg, out } => unop_num!(self, arg, out, -arg),
            Instr::Not { arg, out } => unop_num!(self, arg, out, !arg),
            Instr::And { arg1, arg2, out } => {
                let arg1 = unsafe { self.num_ref(arg1).as_bool() };
                let arg2 = unsafe { self.num_ref(arg2).as_bool() };
                *unsafe { self.num_mut(out) } = (arg1 && arg2).into();
                self.set_next_instr();
            },
            Instr::Or { arg1, arg2, out } => {
                let arg1 = unsafe { self.num_ref(arg1).as_bool() };
                let arg2 = unsafe { self.num_ref(arg2).as_bool() };
                *unsafe { self.num_mut(out) } = (arg1 || arg2).into();
                self.set_next_instr();
            },
            Instr::Eq { arg1, arg2, out } => cmp!(self, arg1, arg2, out, true, arg1 == arg2),
            Instr::Le { arg1, arg2, out } => cmp!(
                self,
                arg1,
                arg2,
                out,
                true,
                arg1.le(arg2, unsafe { self.get_buffer() }),
            ),
            Instr::Lt { arg1, arg2, out } => cmp!(
                self,
                arg1,
                arg2,
                out,
                false,
                arg1.lt(arg2, unsafe { self.get_buffer() }),
            ),
            Instr::BoolN { arg, out } => unop_num!(self, arg, out, arg.as_bool().into()),
            Instr::BoolV { arg, out } => {
                let result = unsafe { self.val_ref(arg).as_bool() };
                *unsafe { self.num_mut(out) } = result.into();
                self.set_next_instr();
            },
        };
        Some(false)
    }

    pub fn step(&mut self) {
        loop {
            match self.step_aux() {
                Some(true) => return,
                None => self.runtime_err(),
                Some(false) => if matches!(self.code[self.next_instr], Instr::LineStart(_)) {
                    return;
                },
            }
        }
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
