use super::*;

#[derive(Clone, Copy)]
pub union UnsafeArg {
    number: NumberReg,
    string: StringReg,
    value: ValueReg,
}

static_assertions::assert_eq_size!(UnsafeArg, super::UnsafeArg);
static_assertions::assert_eq_align!(UnsafeArg, super::UnsafeArg);

pub type MUArg = MaybeUninit<UnsafeArg>;

#[inline(always)]
fn muiarg_to_muarg(muiarg: MaybeUninit<super::UnsafeArg>) -> MUArg {
    unsafe { std::mem::transmute(muiarg) }
}

pub type SubInstrCall = fn(&mut SIExec, data: u8, reg0: MUArg, reg1: MUArg, reg2: MUArg);

#[derive(Clone, Copy)]
pub struct SubInstr {
    reg0: MUArg,
    reg1: MUArg,
    reg2: MUArg,
    data: u8,
    call: SubInstrCall,
}

impl SubInstr {
    //#[inline(always)]
    pub fn sub_call(&self, vm: &mut SIExec) {
        (self.call)(vm, self.data, self.reg0, self.reg1, self.reg2);
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
    pub fn new(vm: &mut SIExec, section: &[Instr]) -> Self {
        let mut instrs = Vec::with_capacity(section.len());
        let mut iter = section.iter().copied();
        let last = iter.next_back().unwrap();
        match last.tag {
            InstrTag::LineStart => unreachable!("line starts are supposed to be stripped!"),
            InstrTag::JumpRel => todo!(),
            InstrTag::JumpErr => todo!(),
            InstrTag::JumpLine => todo!(),
            _ => { instrs.push(SubInstr {
                reg0: muiarg_to_muarg(last.reg0),
                reg1: muiarg_to_muarg(last.reg0),
                reg2: muiarg_to_muarg(last.reg0),
                data: last.data,
                call: todo!(),
            }) },
        };
        todo!()
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

static JUMP_TABLE: [SubInstrCall; u8::MAX as usize] = {
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
fn death(_: &mut SIExec, _: u8, _: MUArg, _: MUArg, _: MUArg) {
    panic!("Death! VM called an invalid instruction");
}