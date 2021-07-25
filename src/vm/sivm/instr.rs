use super::*;

macro_rules! unop {
    ($self:ident, $arg:ident, $out:ident, $f:ident, $e1:expr, $e2:expr $(, )?) => { paste! { {
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
    } } };
}

macro_rules! unop_num {
    ($self:ident, $arg:ident, $out:ident, $e:expr $(, )?) => { {
        // SAFE: only one ref is taken
        let $arg = unsafe { $self.num_ref($arg) }.clone();
        // SAFE: still only one ref: we no longer use the ref from above by cloning
        *unsafe { $self.num_mut($out) } = $e;
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

#[derive(Clone, Copy)]
pub union UnsafeArg {
    number: NumberReg,
    string: StringReg,
    value: ValueReg,
    sinstr: SuperInstrReg,
}

static_assertions::assert_eq_size!(UnsafeArg, super::UnsafeArg);
static_assertions::assert_eq_align!(UnsafeArg, super::UnsafeArg);

pub type MUArg = MaybeUninit<UnsafeArg>;

#[inline(always)]
fn muiarg_to_muarg(muiarg: MaybeUninit<super::UnsafeArg>) -> MUArg {
    unsafe { std::mem::transmute(muiarg) }
}

pub type SubInstrCall = fn(&mut SIExec, reg0: MUArg, reg1: MUArg, reg2: MUArg);

#[derive(Clone, Copy)]
pub struct SubInstr {
    reg0: MUArg,
    reg1: MUArg,
    reg2: MUArg,
    data: u8,
    call: SubInstrCall,
}

impl SubInstr {
    #[inline(always)]
    pub fn sub_call(&self, vm: &mut SIExec) {
        (self.call)(vm, self.reg0, self.reg1, self.reg2);
    }

    #[inline]
    pub fn set_line_end(&mut self, last: bool) {
        if last {
            self.data |= 0b0000_0001;
        } else {
            self.data &= !0b0000_0001;
        }
    }

    #[inline(always)]
    pub const fn is_line_end(&self) -> bool {
        self.data & 0b0000_0001 != 0
    }

    #[inline]
    pub fn set_jumprel_cond(&mut self, start: bool) {
        if start {
            self.data |= 0b0000_0010;
        } else {
            self.data &= !0b0000_0010;
        }
    }

    #[inline(always)]
    pub const fn get_jumprel_cond(&self) -> bool {
        self.data & 0b0000_0010 != 0
    }
}

impl Debug for SubInstr {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f
            .debug_struct("SubInstr")
            .field("data", &self.data)
            .field("call", &format!("{:p}", &self.call))
            .finish_non_exhaustive()
    }
}

#[derive(Debug, Clone)]
pub struct SuperInstr {
    out_success: SuperInstrReg,
    out_err: SuperInstrReg,
    instrs: Box<[SubInstr]>,
}

impl SuperInstr {
    pub(in crate::vm) fn new(vm: &mut SIExec, section: &[Instr]) -> (Self, bool) {
        let mut fixup_last = false;
        let mut instrs = Vec::with_capacity(section.len());
        let mut iter = section.iter().copied();
        let last = iter.next_back().unwrap();
        match last.tag {
            InstrTag::LineStart => unreachable!("line starts are supposed to be stripped!"),
            InstrTag::JumpRel => {
                fixup_last = true;
                instrs.push(SubInstr {
                    reg0: muiarg_to_muarg(last.reg0),
                    reg1: muiarg_to_muarg(last.reg1),
                    reg2: muiarg_to_muarg(last.reg2),
                    data: last.data,
                    call: JUMP_TABLE[last.tag as usize],
                });
            },
            InstrTag::JumpErr => {
                fixup_last = true;
            },
            InstrTag::JumpLine => todo!(),
            _ => { instrs.push(SubInstr {
                reg0: muiarg_to_muarg(last.reg0),
                reg1: muiarg_to_muarg(last.reg1),
                reg2: muiarg_to_muarg(last.reg2),
                data: last.data,
                call: JUMP_TABLE[last.tag as usize],
            }) },
        };
        todo!()
    }
}

impl SIExec {
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
    }

    fn move_nv(&mut self, arg: NumberReg, out: ValueReg) {
        firestorm::profile_method!("move_nv");
        // SAFE: all registers are guaranteed not to alias
        *unsafe { self.val_mut(out) } = Value::Number(*unsafe { self.num_mut(arg) });
    }

    fn move_vv(&mut self, arg: ValueReg, out: ValueReg) {
        firestorm::profile_method!("move_vv");
        if arg != out {
            // SAFE: all registers are guaranteed not to alias
            unsafe { self.val_mut(out).clone_from(self.val_mut(arg)) };
        }
    }

    fn move_vs(&mut self, arg: ValueReg, out: StringReg) {
        firestorm::profile_method!("move_vs");
        let out = if let Value::Str(s) = unsafe { self.val_mut(arg) } {
            unsafe { self.str_mut(out).clone_from(s) };
            false
        } else {
            true
        };
    }

    fn move_vn(&mut self, arg: ValueReg, out: NumberReg) {
        firestorm::profile_method!("move_vn");
        let out = if let Value::Number(n) = unsafe { self.val_mut(arg) } {
            unsafe { *self.num_mut(out) = *n };
            false
        } else {
            true
        };
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

    fn rem(&mut self, arg1: NumberReg, arg2: NumberReg, out: NumberReg) {
        firestorm::profile_method!("rem");
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

    fn not_n(&mut self, arg: NumberReg, out: NumberReg) {
        firestorm::profile_method!("not_n");
        unop_num!(self, arg, out, !arg);
    }

    fn not_v(&mut self, arg: ValueReg, out: NumberReg) {
        firestorm::profile_method!("not_v");
        unsafe { *self.num_mut(out) = !self.val_ref(arg) };
    }

    fn and(&mut self, arg1: NumberReg, arg2: NumberReg, out: NumberReg) {
        firestorm::profile_method!("and");
        let arg1 = unsafe { self.num_ref(arg1).as_bool() };
        let arg2 = unsafe { self.num_ref(arg2).as_bool() };
        *unsafe { self.num_mut(out) } = (arg1 && arg2).into();
    }

    fn or(&mut self, arg1: NumberReg, arg2: NumberReg, out: NumberReg) {
        firestorm::profile_method!("or");
        let arg1 = unsafe { self.num_ref(arg1).as_bool() };
        let arg2 = unsafe { self.num_ref(arg2).as_bool() };
        *unsafe { self.num_mut(out) } = (arg1 || arg2).into();
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
    }

    fn stringify_n(&mut self, arg: NumberReg, out: StringReg) {
        firestorm::profile_method!("stringify_n");
        let arg = unsafe { self.num_ref(arg) };
        let out = unsafe { self.str_mut(out) };
        out.clear();
        write!(out, "{}", arg).unwrap();
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
    }

    #[inline]
    fn step_aux(&mut self) -> bool {
        let sinstr = unsafe { self.get_sinstr(self.next_sinstr) };
        for instr in sinstr.instrs.iter() {
            instr.sub_call(self);
        }
        if self.runtime_err_flag.get() {
            self.next_sinstr = sinstr.out_err;
            true
        } else {
            self.next_sinstr = sinstr.out_success;
            sinstr.instrs
                .last()
                .unwrap_or_else(|| unsafe { unreachable() })
                .is_line_end()
        }
    }

    pub fn step(&mut self) {
        *self.runtime_err_flag.get_mut() = false;
        loop {
            if self.step_aux() {
                break;
            }
        }
    }
}

macro_rules! jump_table_fn {
    ($table:ident, $var:ident, $name:ident $(, )? $($if:ident . $un:ident),*) => { paste! {
        $table[InstrTag::$var as usize] = {
            #[allow(unused_variables)]
            fn f(vm: &mut SIExec, reg0: MUArg, reg1: MUArg, reg2: MUArg) {
                vm.$name($(unsafe { [<$if>].assume_init().$un }),*);
            }
            f as _
    } };
    };
}

pub(super) static JUMP_TABLE: [SubInstrCall; u8::MAX as usize] = {
    let mut table = [death as _; u8::MAX as usize];
    jump_table_fn!(table, MoveSV, move_sv, reg0.string, reg1.value);
    jump_table_fn!(table, MoveNV, move_nv, reg0.number, reg1.value);
    jump_table_fn!(table, MoveVV, move_vv, reg0.value, reg1.value);
    jump_table_fn!(table, MoveVS, move_vs, reg0.value, reg1.string);
    jump_table_fn!(table, MoveVN, move_vn, reg0.value, reg1.number);
    jump_table_fn!(table, StringifyN, stringify_n, reg0.number, reg1.string);
    jump_table_fn!(table, StringifyV, stringify_v, reg0.value, reg1.string);
    jump_table_fn!(table, AddS, add_s, reg0.string, reg1.string, reg2.string);
    jump_table_fn!(table, AddN, add_n, reg0.number, reg1.number, reg2.number);
    jump_table_fn!(table, AddV, add_v, reg0.value, reg1.value, reg2.value);
    jump_table_fn!(table, SubS, sub_s, reg0.string, reg1.string, reg2.string);
    jump_table_fn!(table, SubN, sub_n, reg0.number, reg1.number, reg2.number);
    jump_table_fn!(table, SubV, sub_v, reg0.value, reg1.value, reg2.value);
    jump_table_fn!(table, Mul, mul, reg0.number, reg1.number, reg2.number);
    jump_table_fn!(table, Div, div, reg0.number, reg1.number, reg2.number);
    jump_table_fn!(table, Rem, rem, reg0.number, reg1.number, reg2.number);
    jump_table_fn!(table, Pow, pow, reg0.number, reg1.number, reg2.number);
    jump_table_fn!(table, Eq, eq, reg0.value, reg1.value, reg2.number);
    jump_table_fn!(table, Le, le, reg0.value, reg1.value, reg2.number);
    jump_table_fn!(table, Lt, lt, reg0.value, reg1.value, reg2.number);
    jump_table_fn!(table, IncS, inc_s, reg0.string, reg1.string);
    jump_table_fn!(table, IncN, inc_n, reg0.number, reg1.number);
    jump_table_fn!(table, IncV, inc_v, reg0.value, reg1.value);
    jump_table_fn!(table, DecS, dec_s, reg0.string, reg1.string);
    jump_table_fn!(table, DecN, dec_n, reg0.number, reg1.number);
    jump_table_fn!(table, DecV, dec_v, reg0.value, reg1.value);
    jump_table_fn!(table, And, and, reg0.number, reg1.number, reg2.number);
    jump_table_fn!(table, Or, or, reg0.number, reg1.number, reg2.number);
    jump_table_fn!(table, Abs, abs, reg0.number, reg1.number);
    jump_table_fn!(table, Fact, fact, reg0.number, reg1.number);
    jump_table_fn!(table, Sqrt, sqrt, reg0.number, reg1.number);
    jump_table_fn!(table, Sin, sin, reg0.number, reg1.number);
    jump_table_fn!(table, Cos, cos, reg0.number, reg1.number);
    jump_table_fn!(table, Tan, tan, reg0.number, reg1.number);
    jump_table_fn!(table, Asin, asin, reg0.number, reg1.number);
    jump_table_fn!(table, Acos, acos, reg0.number, reg1.number);
    jump_table_fn!(table, Atan, atan, reg0.number, reg1.number);
    jump_table_fn!(table, Neg, neg, reg0.number, reg1.number);
    jump_table_fn!(table, NotN, not_n, reg0.number, reg1.number);
    jump_table_fn!(table, NotV, not_v, reg0.value, reg1.number);
    jump_table_fn!(table, BoolN, bool_n, reg0.number, reg1.number);
    jump_table_fn!(table, BoolV, bool_v, reg0.value, reg1.number);
    table
};

#[cold]
fn death(_: &mut SIExec, _: MUArg, _: MUArg, _: MUArg) {
    panic!("Death! VM called an invalid instruction");
}