use std::fmt::Debug;
use super::*;

impl VMExec {
    fn line_start(&mut self, line: Line) {
        firestorm::profile_method!("line_start");
        self.cur_line = line;
        self.set_next_instr();
    }

    fn jump_rel(&mut self, flag: bool, amount: usize, condition: MaybeUninit<UnsafeArg>) {
        firestorm::profile_method!("jump_rel");
        if flag {
            // SAFE: only one ref taken
            if unsafe { self.num_mut(condition.assume_init().number).as_bool() } {
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
        self.set_next_instr();
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

    fn step_aux(&mut self) {
        firestorm::profile_method!("step_aux");
        let instr = self.code[self.next_instr].clone();
        JUMP_TABLE[instr.tag as usize](self, instr);
        self.halt_flag |= instr.is_line_end();
    }

    pub fn step(&mut self) {
        firestorm::profile_method!("step");
        self.runtime_err_flag.set(false);
        self.halt_flag = false;
        while !self.runtime_err_flag.get() && !self.halt_flag {
            self.step_aux();
        }
        if self.runtime_err_flag.get() {
            self.runtime_err();
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub(super) enum InstrTag {
    LineStart = 0,
    JumpRel, 
    JumpErr,
    JumpLine,
    MoveSV,
    MoveNV,
    MoveVV,
    MoveVS,
    MoveVN,
    StringifyN,
    StringifyV,
    AddS,
    AddN,
    AddV,
    SubS,
    SubN,
    SubV,
    Mul,
    Div,
    Rem,
    Pow,
    Eq, 
    Le, 
    Lt, 
    IncS,
    IncN,
    IncV,
    DecS,
    DecN,
    DecV,
    Abs,
    Fact,
    Sqrt,
    Sin,
    Cos,
    Tan,
    Asin,
    Acos,
    Atan,
    Neg,
    And,
    Or,
    NotN,
    NotV,
    BoolN,
    BoolV,
    Death = u8::MAX,
}

#[derive(Clone, Copy)]
pub(super) union UnsafeArg {
    pub number: NumberReg,
    pub string: StringReg,
    pub value: ValueReg,
    pub line: u8,
    pub ip_offset: u16,
}

#[derive(Clone, Copy)]
#[repr(packed(8))]
pub(super) struct Instr {
    pub tag: InstrTag,
    pub data: u8,
    pub reg0: MaybeUninit<UnsafeArg>,
    pub reg1: MaybeUninit<UnsafeArg>,
    pub reg2: MaybeUninit<UnsafeArg>,
}

macro_rules! ctors {
    ($($name:ident ( $tag:ident ) $(, )? $($arg:ident : $t:ty),* $(=> $($field:ident : $un:ident : $e:expr),* $(, )?)?);* $(; )?) => {
        impl Instr {
            $(
                #[allow(unused_unsafe, dead_code)]
                pub const fn $name($($arg : $t),*) -> Self {
                    unsafe { Instr {
                        tag: InstrTag::$tag,
                        $($($field : MaybeUninit::new(UnsafeArg { $un: $e }),)*)?
                        ..Self::DEATH
                    } }
                }
            )*
        }
    };
}

ctors! {
    line_start (LineStart), line: Line => reg0: line: line;
    jump_err (JumpErr);
    move_sv (MoveSV), string: StringReg, value: ValueReg =>
        reg0: string: string, reg1: value: value;
    move_nv (MoveNV), number: NumberReg, value: ValueReg =>
        reg0: number: number, reg1: value: value;
    move_vv (MoveVV), arg: ValueReg, out: ValueReg =>
        reg0: value: arg, reg1: value: out;
    move_vs (MoveVS), value: ValueReg, string: StringReg =>
        reg0: value: value, reg1: string: string;
    move_vn (MoveVN), value: ValueReg, number: NumberReg =>
        reg0: value: value, reg1: number: number;
    stringify_n (StringifyN), number: NumberReg, string: StringReg =>
        reg0: number: number, reg1: string: string;
    stringify_v (StringifyV), value: ValueReg, string: StringReg =>
        reg0: value: value, reg1: string: string;
    add_s (AddS), reg0: StringReg, reg1: StringReg, out: StringReg =>
        reg0: string: reg0, reg1: string: reg1, reg2: string: out;
    add_n (AddN), reg0: NumberReg, reg1: NumberReg, out: NumberReg =>
        reg0: number: reg0, reg1: number: reg1, reg2: number: out;
    add_v (AddV), reg0: ValueReg, reg1: ValueReg, out: ValueReg =>
        reg0: value: reg0, reg1: value: reg1, reg2: value: out;
    sub_s (SubS), reg0: StringReg, reg1: StringReg, out: StringReg =>
        reg0: string: reg0, reg1: string: reg1, reg2: string: out;
    sub_n (SubN), reg0: NumberReg, reg1: NumberReg, out: NumberReg =>
        reg0: number: reg0, reg1: number: reg1, reg2: number: out;
    sub_v (SubV), reg0: ValueReg, reg1: ValueReg, out: ValueReg =>
        reg0: value: reg0, reg1: value: reg1, reg2: value: out;
    mul (Mul), reg0: NumberReg, reg1: NumberReg, out: NumberReg =>
        reg0: number: reg0, reg1: number: reg1, reg2: number: out;
    div (Div), reg0: NumberReg, reg1: NumberReg, out: NumberReg =>
        reg0: number: reg0, reg1: number: reg1, reg2: number: out;
    rem (Rem), reg0: NumberReg, reg1: NumberReg, out: NumberReg =>
        reg0: number: reg0, reg1: number: reg1, reg2: number: out;
    pow (Pow), reg0: NumberReg, reg1: NumberReg, out: NumberReg =>
        reg0: number: reg0, reg1: number: reg1, reg2: number: out;
    eq (Eq), reg0: ValueReg, reg1: ValueReg, out: NumberReg =>
        reg0: value: reg0, reg1: value: reg1, reg2: number: out;
    le (Le), reg0: ValueReg, reg1: ValueReg, out: NumberReg =>
        reg0: value: reg0, reg1: value: reg1, reg2: number: out;
    lt (Lt), reg0: ValueReg, reg1: ValueReg, out: NumberReg =>
        reg0: value: reg0, reg1: value: reg1, reg2: number: out;
    inc_s (IncS), reg0: StringReg, out: StringReg =>
        reg0: string: reg0, reg1: string: out;
    inc_n (IncN), reg0: NumberReg, out: NumberReg =>
        reg0: number: reg0, reg1: number: out;
    inc_v (IncV), reg0: ValueReg, out: ValueReg =>
        reg0: value: reg0, reg1: value: out;
    dec_s (DecS), reg0: StringReg, out: StringReg =>
        reg0: string: reg0, reg1: string: out;
    dec_n (DecN), reg0: NumberReg, out: NumberReg =>
        reg0: number: reg0, reg1: number: out;
    dec_v (DecV), reg0: ValueReg, out: ValueReg =>
        reg0: value: reg0, reg1: value: out;
    abs (Abs), reg0: NumberReg, out: NumberReg =>
        reg0: number: reg0, reg1: number: out;
    fact (Fact), reg0: NumberReg, out: NumberReg =>
        reg0: number: reg0, reg1: number: out;
    sqrt (Sqrt), reg0: NumberReg, out: NumberReg =>
        reg0: number: reg0, reg1: number: out;
    sin (Sin), reg0: NumberReg, out: NumberReg =>
        reg0: number: reg0, reg1: number: out;
    cos (Cos), reg0: NumberReg, out: NumberReg =>
        reg0: number: reg0, reg1: number: out;
    tan (Tan), reg0: NumberReg, out: NumberReg =>
        reg0: number: reg0, reg1: number: out;
    asin (Asin), reg0: NumberReg, out: NumberReg =>
        reg0: number: reg0, reg1: number: out;
    acos (Acos), reg0: NumberReg, out: NumberReg =>
        reg0: number: reg0, reg1: number: out;
    atan (Atan), reg0: NumberReg, out: NumberReg =>
        reg0: number: reg0, reg1: number: out;
    neg (Neg), reg0: NumberReg, out: NumberReg =>
        reg0: number: reg0, reg1: number: out;
    and (And), reg0: NumberReg, reg1: NumberReg, out: NumberReg =>
        reg0: number: reg0, reg1: number: reg1, reg2: number: out;
    or (Or), reg0: NumberReg, reg1: NumberReg, out: NumberReg =>
        reg0: number: reg0, reg1: number: reg1, reg2: number: out;
    not_n (NotN), reg0: NumberReg, out: NumberReg =>
        reg0: number: reg0, reg1: number: out;
    not_v (NotV), reg0: ValueReg, out: NumberReg =>
        reg0: value: reg0, reg1: number: out;
    bool_n (BoolN), reg0: NumberReg, out: NumberReg =>
        reg0: number: reg0, reg1: number: out;
    bool_v (BoolV), reg0: ValueReg, out: NumberReg =>
        reg0: value: reg0, reg1: number: out;
}

static_assertions::assert_eq_size!(u64, Instr);

impl Instr {
    pub const DEATH: Self = Instr {
        tag: InstrTag::Death,
        data: 0,
        reg0: MaybeUninit::uninit(),
        reg1: MaybeUninit::uninit(),
        reg2: MaybeUninit::uninit(),
    };

    pub fn jump_line(line: NumberReg) -> Self {
        let mut instr = Instr {
            tag: InstrTag::JumpLine,
            reg0: MaybeUninit::new(UnsafeArg { number: line }),
            ..Self::DEATH
        };
        instr.set_jumprel_cond(true);
        instr
    }

    pub fn jump_rel(amount: usize, condition: Option<NumberReg>) -> Self {
        let mut instr = Instr {
            tag: InstrTag::JumpRel,
            reg0: MaybeUninit::new(UnsafeArg { ip_offset: amount as u16 }),
            reg1: if let Some(condition) = condition {
                MaybeUninit::new(UnsafeArg { number: condition })
            } else {
                MaybeUninit::uninit()
            },
            ..Self::DEATH
        };
        instr.set_jumprel_cond(condition.is_some());
        instr
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

impl Default for Instr {
    fn default() -> Self {
        Instr::DEATH
    }
}

impl Debug for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.tag.fmt(f)
    }
}

macro_rules! jump_table_fn {
    ($table:ident, $var:ident, $name:ident $(, )? $($if:ident . $un:ident),*) => {
        $table[InstrTag::$var as usize] = {
            fn f(vm: &mut VMExec, instr: Instr) {
                vm.$name($(unsafe { instr.$if.assume_init().$un }),*);
            }
            f as _
        };
    };
}

static JUMP_TABLE: [fn(&mut VMExec, Instr); u8::MAX as usize] = {
    let mut table = [death as _; u8::MAX as usize];
    jump_table_fn!(table, LineStart, line_start, reg0.line);
    table[InstrTag::JumpRel as usize] = {
        fn f(vm: &mut VMExec, instr: Instr) {
            vm.jump_rel(
                instr.get_jumprel_cond(),
                unsafe { instr.reg0.assume_init().ip_offset } as usize,
                instr.reg1,
            );
        }
        f as _
    };
    table[InstrTag::JumpErr as usize] = {
        fn f(vm: &mut VMExec, _: Instr) {
            vm.runtime_err_flag.set(true);
            vm.halt_flag = true;
        }
        f as _
    };
    jump_table_fn!(table, JumpLine, jump_line, reg0.number);
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
fn death(_: &mut VMExec, _: Instr) {
    panic!("Death! VM called an invalid instruction");
}
