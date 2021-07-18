use super::*;
use arith::*;
use crate::parser::{NUM_LINES, ast::*};
use std::array::IntoIter;
use std::fmt::Write;
use ahash::AHashMap;
use std::cell::{Cell, UnsafeCell};

const STRING_CAP: usize = 4096;

mod codegen;
pub mod analysis;

type Reg = u16;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct NumberReg(Reg);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct StringReg(Reg);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct ValueReg(Reg);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Label(Reg);

type Line = u8;

#[derive(Debug, Clone)]
enum Instr {
    LineStart(Line),
    JumpRel { amount: usize, condition: Option<NumberReg> },
    JumpErr,
    JumpLine(NumberReg),
    MoveSV { arg: StringReg, out: ValueReg },
    MoveNV { arg: NumberReg, out: ValueReg },
    MoveVV { arg: ValueReg, out: ValueReg },
    MoveVS { arg: ValueReg, out: StringReg },
    MoveVN { arg: ValueReg, out: NumberReg },
    StringifyN { arg: NumberReg, out: StringReg },
    StringifyV { arg: ValueReg, out: StringReg },
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
        vm.code.push(Instr::JumpErr);
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
    runtime_err_flag: Cell<bool>,
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

    fn line_start(&mut self, line: Line) {
        firestorm::profile_method!("line_start");
        self.cur_line = line;
        self.line_stats[line as usize] += 1;
        self.set_next_instr();
    }

    fn jump_rel(&mut self, amount: usize, condition: Option<NumberReg>) {
        firestorm::profile_method!("jump_rel");
        if let Some(condition) = condition {
            // SAFE: only one ref taken
            if unsafe { self.num_mut(condition).as_bool() } {
                self.next_instr += amount;
            } else {
                self.set_next_instr();
            }
        } else {
            self.next_instr += amount;
        }
    }

    fn jump_line(&mut self, reg: NumberReg) {
        firestorm::profile_method!("jump_line");
        // SAFE: only one ref taken
        let result = unsafe { self.num_mut(reg).as_f32() };
        let next_line = (result.floor() as usize).clamp(1, NUM_LINES) - 1;
        self.set_next_line(next_line);
    }

    fn move_sv(&mut self, arg: StringReg, out: ValueReg) {
        firestorm::profile_method!("move_sv");
        // SAFE: all registers are guaranteed not to alias
        let s = unsafe { self.str_mut(arg) };
        let value = unsafe { self.val_mut(out) };
        if let Value::Str(old) = value {
            old.clone_from(s);
        } else {
            *value = Value::Str(s.clone());
        }
        self.set_next_instr();
    }

    fn move_nv(&mut self, arg: NumberReg, out: ValueReg) {
        firestorm::profile_method!("move_nv");
        // SAFE: all registers are guaranteed not to alias
        *unsafe { self.val_mut(out) } = Value::Number(*unsafe { self.num_mut(arg) });
        self.set_next_instr();
    }

    fn move_vv(&mut self, arg: ValueReg, out: ValueReg) {
        firestorm::profile_method!("move_vv");
        if arg != out {
            // SAFE: all registers are guaranteed not to alias
            unsafe { self.val_mut(out).clone_from(self.val_mut(arg)) };
        }
        self.set_next_instr();
    }

    fn move_vs(&mut self, arg: ValueReg, out: StringReg) {
        firestorm::profile_method!("move_vs");
        let out = if let Value::Str(s) = unsafe { self.val_mut(arg) } {
            unsafe { self.str_mut(out).clone_from(s) };
            false
        } else {
            true
        };
        self.set_next_instr();
        self.runtime_err_flag.set(out);
    }

    fn move_vn(&mut self, arg: ValueReg, out: NumberReg) {
        firestorm::profile_method!("move_vn");
        let out = if let Value::Number(n) = unsafe { self.val_mut(arg) } {
            unsafe { *self.num_mut(out) = *n };
            false
        } else {
            true
        };
        self.set_next_instr();
        self.runtime_err_flag.set(out);
    }

    fn add_s(&mut self, arg1: StringReg, arg2: StringReg, out: StringReg) {
        firestorm::profile_method!("add_s");
        binop!(self, arg1, arg2, out, str_mut,
            *arg1 = YString(arg1.repeat(2)),
            *arg1 += arg2,
            { out.clone_from(arg1); *out += arg1 },
            { out.clone_from(arg1); *out += arg2 },
        );
    }

    fn add_n(&mut self, arg1: NumberReg, arg2: NumberReg, out: NumberReg) {
        firestorm::profile_method!("add_n");
        binop!(self, arg1, arg2, out, num_mut,
            *arg1 += *arg1,
            *arg1 += *arg2,
            *out = *arg1 + *arg1,
            *out = *arg1 + *arg2,
        );
    }

    fn add_v(&mut self, arg1: ValueReg, arg2: ValueReg, out: ValueReg) {
        firestorm::profile_method!("add_v");
        binop!(self, arg1, arg2, out, val_mut,
            match arg1 {
                Value::Number(n) => *n += *n,
                Value::Str(s) => *s = YString(s.repeat(2)),
            },
            arg1.add_assign(arg2, unsafe { self.get_buffer() }),
            { out.clone_from(arg1); out.add_assign(arg1, unsafe { self.get_buffer() }) },
            { out.clone_from(arg1); out.add_assign(arg2, unsafe { self.get_buffer() }) },
        );
    }

    fn sub_s(&mut self, arg1: StringReg, arg2: StringReg, out: StringReg) {
        firestorm::profile_method!("sub_s");
        binop!(self, arg1, arg2, out, str_mut,
            *arg1 -= {
                let buffer = unsafe { self.get_buffer() };
                buffer.clone_from(arg1);
                buffer
            },
            *arg1 -= arg2,
            { out.clone_from(arg1); *out -= arg1 },
            { out.clone_from(arg1); *out -= arg2 },
        );
    }

    fn sub_n(&mut self, arg1: NumberReg, arg2: NumberReg, out: NumberReg) {
        firestorm::profile_method!("sub_n");
        binop!(self, arg1, arg2, out, num_mut,
            *arg1 -= *arg1,
            *arg1 -= *arg2,
            *out = *arg1 - *arg1,
            *out = *arg1 - *arg2,
        );
    }

    fn sub_v(&mut self, arg1: ValueReg, arg2: ValueReg, out: ValueReg) {
        firestorm::profile_method!("sub_v");
        binop!(self, arg1, arg2, out, val_mut,
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
        );
    }

    fn inc_s(&mut self, arg: StringReg, out: StringReg) {
        firestorm::profile_method!("inc_s");
        unop!(self, arg, out, str_mut, arg.pre_inc(), arg.post_inc_s(out));
    }

    fn inc_n(&mut self, arg: NumberReg, out: NumberReg) {
        firestorm::profile_method!("inc_n");
        unop!(self, arg, out, num_mut, arg.pre_inc(), *out = arg.post_inc());
    }

    fn inc_v(&mut self, arg: ValueReg, out: ValueReg) {
        firestorm::profile_method!("inc_v");
        unop!(self, arg, out, val_mut, arg.pre_inc(), arg.post_inc(out));
    }

    fn dec_s(&mut self, arg: StringReg, out: StringReg) {
        firestorm::profile_method!("dec_s");
        unop!(self, arg, out, str_mut,
            self.set_flag_err_res(arg.pre_dec()),
            self.set_flag_err_res(arg.post_dec_s(out)),
        );
    }

    fn dec_n(&mut self, arg: NumberReg, out: NumberReg) {
        firestorm::profile_method!("dec_n");
        unop!(self, arg, out, num_mut, arg.pre_dec(), *out = arg.post_dec());
    }

    fn dec_v(&mut self, arg: ValueReg, out: ValueReg) {
        firestorm::profile_method!("dec_v");
        unop!(self, arg, out, val_mut,
            self.set_flag_err_res(arg.pre_dec()),
            self.set_flag_err_res(arg.post_dec(out)),
        );
    }

    fn mul(&mut self, arg1: NumberReg, arg2: NumberReg, out: NumberReg) {
        firestorm::profile_method!("mul");
        binop!(self, arg1, arg2, out, num_mut,
            *arg1 *= *arg1,
            *arg1 *= *arg2,
            *out = *arg1 * *arg1,
            *out = *arg1 * *arg2,
        );
    }

    fn div(&mut self, arg1: NumberReg, arg2: NumberReg, out: NumberReg) {
        firestorm::profile_method!("div");
        binop!(self, arg1, arg2, out, num_mut,
            self.set_flag_err_res(arg1.div_assign(arg1.clone())),
            self.set_flag_err_res(arg1.div_assign(arg2.clone())),
            self.set_flag_err_res_set(*arg1 / *arg1, out),
            self.set_flag_err_res_set(*arg1 / *arg2, out),
        );
    }

    fn mod_instr(&mut self, arg1: NumberReg, arg2: NumberReg, out: NumberReg) {
        firestorm::profile_method!("mod");
        binop!(self, arg1, arg2, out, num_mut,
            self.set_flag_err_res(arg1.rem_assign(arg1.clone())),
            self.set_flag_err_res(arg1.rem_assign(arg2.clone())),
            self.set_flag_err_res_set(*arg1 % *arg1, out),
            self.set_flag_err_res_set(*arg1 % *arg2, out),
        );
    }

    fn pow(&mut self, arg1: NumberReg, arg2: NumberReg, out: NumberReg) {
        firestorm::profile_method!("pow");
        binop!(self, arg1, arg2, out, num_mut,
            arg1.pow_assign(arg1.clone()),
            arg1.pow_assign(arg2.clone()),
            *out = *arg1 * *arg1,
            *out = *arg1 * *arg2,
        );
    }

    fn abs(&mut self, arg: NumberReg, out: NumberReg) {
        firestorm::profile_method!("abs");
        unop_num!(self, arg, out, arg.abs());
    }

    fn fact(&mut self, arg: NumberReg, out: NumberReg) {
        firestorm::profile_method!("fact");
        unop_num!(self, arg, out, arg.fact());
    }

    fn sqrt(&mut self, arg: NumberReg, out: NumberReg) {
        firestorm::profile_method!("sqrt");
        unop_num!(self, arg, out, arg.sqrt());
    }

    fn sin(&mut self, arg: NumberReg, out: NumberReg) {
        firestorm::profile_method!("sin");
        unop_num!(self, arg, out, arg.sin());
    }

    fn cos(&mut self, arg: NumberReg, out: NumberReg) {
        firestorm::profile_method!("cos");
        unop_num!(self, arg, out, arg.cos());
    }

    fn tan(&mut self, arg: NumberReg, out: NumberReg) {
        firestorm::profile_method!("tan");
        unop_num!(self, arg, out, arg.tan());
    }

    fn asin(&mut self, arg: NumberReg, out: NumberReg) {
        firestorm::profile_method!("asin");
        unop_num!(self, arg, out, arg.asin());
    }

    fn acos(&mut self, arg: NumberReg, out: NumberReg) {
        firestorm::profile_method!("acos");
        unop_num!(self, arg, out, arg.acos());
    }

    fn atan(&mut self, arg: NumberReg, out: NumberReg) {
        firestorm::profile_method!("atan");
        unop_num!(self, arg, out, arg.atan());
    }

    fn neg(&mut self, arg: NumberReg, out: NumberReg) {
        firestorm::profile_method!("neg");
        unop_num!(self, arg, out, -arg);
    }

    fn not(&mut self, arg: NumberReg, out: NumberReg) {
        firestorm::profile_method!("not");
        unop_num!(self, arg, out, !arg);
    }

    fn and(&mut self, arg1: NumberReg, arg2: NumberReg, out: NumberReg) {
        firestorm::profile_method!("and");
        let arg1 = unsafe { self.num_ref(arg1).as_bool() };
        let arg2 = unsafe { self.num_ref(arg2).as_bool() };
        *unsafe { self.num_mut(out) } = (arg1 && arg2).into();
        self.set_next_instr();
    }

    fn or(&mut self, arg1: NumberReg, arg2: NumberReg, out: NumberReg) {
        firestorm::profile_method!("or");
        let arg1 = unsafe { self.num_ref(arg1).as_bool() };
        let arg2 = unsafe { self.num_ref(arg2).as_bool() };
        *unsafe { self.num_mut(out) } = (arg1 || arg2).into();
        self.set_next_instr();
    }

    fn eq(&mut self, arg1: ValueReg, arg2: ValueReg, out: NumberReg) {
        firestorm::profile_method!("eq");
        cmp!(self, arg1, arg2, out, true, arg1 == arg2);
    }

    fn le(&mut self, arg1: ValueReg, arg2: ValueReg, out: NumberReg) {
        firestorm::profile_method!("le");
        cmp!( self, arg1, arg2, out, true, arg1.le(arg2, unsafe { self.get_buffer() }));
    }

    fn lt(&mut self, arg1: ValueReg, arg2: ValueReg, out: NumberReg) {
        firestorm::profile_method!("lt");
        cmp!( self, arg1, arg2, out, false, arg1.lt(arg2, unsafe { self.get_buffer() }));
    }

    fn bool_n(&mut self, arg: NumberReg, out: NumberReg) {
        firestorm::profile_method!("bool_n");
        unop_num!(self, arg, out, arg.as_bool().into());
    }

    fn bool_v(&mut self, arg: ValueReg, out: NumberReg) {
        firestorm::profile_method!("bool_v");
        let result = unsafe { self.val_ref(arg).as_bool() };
        *unsafe { self.num_mut(out) } = result.into();
        self.set_next_instr();
    }

    fn stringify_n(&mut self, arg: NumberReg, out: StringReg) {
        firestorm::profile_method!("stringify_n");
        let arg = unsafe { self.num_ref(arg) };
        let out = unsafe { self.str_mut(out) };
        out.clear();
        write!(out, "{}", arg).unwrap();
        self.set_next_instr();
    }

    fn stringify_v(&mut self, arg: ValueReg, out: StringReg) {
        firestorm::profile_method!("stringify_v");
        let out = unsafe { self.str_mut(out) };
        match unsafe { self.val_ref(arg) } {
            Value::Number(arg) => { 
                out.clear();
                write!(out, "{}", arg).unwrap();
            },
            Value::Str(arg) => {
                out.clone_from(arg);
            },
        }
        self.set_next_instr();
    }

    #[inline]
    fn set_flag_err_res<U>(&self, res: Result<(), U>) {
        self.runtime_err_flag.set(res.is_err());
    }

    #[inline]
    fn set_flag_err_res_set<T, U>(&self, res: Result<T, U>, out: &mut T) {
        self.runtime_err_flag.set(if let Ok(t) = res {
            *out = t;
            false
        } else {
            true
        });
    }

    fn step_aux(&mut self) -> bool {
        firestorm::profile_method!("step_aux");
        match self.code[self.next_instr] {
            Instr::LineStart(line) => self.line_start(line),
            Instr::JumpRel { amount, condition } => self.jump_rel(amount, condition),
            Instr::JumpErr => if self.runtime_err_flag.get() {
                self.runtime_err();
                return true;
            } else {
                self.set_next_instr()
            },
            Instr::JumpLine(reg) => {
                self.jump_line(reg);
                return true;
            },
            Instr::MoveSV { arg, out } => self.move_sv(arg, out),
            Instr::MoveNV { arg, out } => self.move_nv(arg, out),
            Instr::MoveVV { arg, out } => self.move_vv(arg, out),
            Instr::MoveVS { arg, out } => self.move_vs(arg, out),
            Instr::MoveVN { arg, out } => self.move_vn(arg, out),
            Instr::StringifyN { arg, out } => self.stringify_n(arg, out),
            Instr::StringifyV { arg, out } => self.stringify_v(arg, out),
            Instr::AddS { arg1, arg2, out } => self.add_s(arg1, arg2, out),
            Instr::AddN { arg1, arg2, out } => self.add_n(arg1, arg2, out),
            Instr::AddV { arg1, arg2, out } => self.add_v(arg1, arg2, out),
            Instr::SubS { arg1, arg2, out } => self.sub_s(arg1, arg2, out),
            Instr::SubN { arg1, arg2, out } => self.sub_n(arg1, arg2, out),
            Instr::SubV { arg1, arg2, out } => self.sub_v(arg1, arg2, out),
            Instr::IncS { arg, out } => self.inc_s(arg, out),
            Instr::IncN { arg, out } => self.inc_n(arg, out),
            Instr::IncV { arg, out } => self.inc_v(arg, out),
            Instr::DecS { arg, out } => self.dec_s(arg, out),
            Instr::DecN { arg, out } => self.dec_n(arg, out),
            Instr::DecV { arg, out } => self.dec_v(arg, out),
            Instr::Mul { arg1, arg2, out } => self.mul(arg1, arg2, out),
            Instr::Div { arg1, arg2, out } => self.div(arg1, arg2, out),
            Instr::Mod { arg1, arg2, out } => self.mod_instr(arg1, arg2, out),
            Instr::Pow { arg1, arg2, out } => self.pow(arg1, arg2, out),
            Instr::Abs { arg, out } => self.abs(arg, out),
            Instr::Fact { arg, out } => self.fact(arg, out),
            Instr::Sqrt { arg, out } => self.sqrt(arg, out),
            Instr::Sin { arg, out } => self.sin(arg, out),
            Instr::Cos { arg, out } => self.cos(arg, out),
            Instr::Tan { arg, out } => self.tan(arg, out),
            Instr::Asin { arg, out } => self.asin(arg, out),
            Instr::Acos { arg, out } => self.acos(arg, out),
            Instr::Atan { arg, out } => self.atan(arg, out),
            Instr::Neg { arg, out } => self.neg(arg, out),
            Instr::Not { arg, out } => self.not(arg, out),
            Instr::And { arg1, arg2, out } => self.and(arg1, arg2, out),
            Instr::Or { arg1, arg2, out } => self.or(arg1, arg2, out),
            Instr::Eq { arg1, arg2, out } => self.eq(arg1, arg2, out),
            Instr::Le { arg1, arg2, out } => self.le(arg1, arg2, out),
            Instr::Lt { arg1, arg2, out } => self.lt(arg1, arg2, out),
            Instr::BoolN { arg, out } => self.bool_n(arg, out),
            Instr::BoolV { arg, out } => self.bool_v(arg, out),
        };
        false
    }

    pub fn step(&mut self) {
        firestorm::profile_method!("step");
        loop {
            let end_of_line = self.step_aux();
            if end_of_line || matches!(self.code[self.next_instr], Instr::LineStart(_)) {
                break;
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
