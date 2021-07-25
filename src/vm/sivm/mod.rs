use std::fmt::{Debug, Formatter, Result as FmtResult};
use std::mem::MaybeUninit;
use super::*;
use instr::*;

pub mod instr;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
struct SuperInstrReg(Reg);

impl SuperInstrReg {
    const INVALID: Self = SuperInstrReg(Reg::MAX);

    #[inline]
    const fn invalid(self) -> bool {
        self.0 == Reg::MAX
    }
}

#[derive(Debug)]
pub struct SIExec {
    sinstrs: Vec<SuperInstr>,
    numbers: Vec<UnsafeCell<Number>>,
    strings: Vec<UnsafeCell<YString>>,
    values: Vec<UnsafeCell<Value>>,
    next_sinstr: SuperInstrReg,
    globals: AHashMap<String, AnyReg>,
    string_buffer: UnsafeCell<YString>,
    runtime_err_flag: Cell<bool>,
}

impl SIExec {
    pub fn new(vm: VMExec) -> Self {
        let mut cfg = vm.control_flow_graph();
        cfg.clean_up();
        let mut map = AHashMap::with_capacity(cfg.node_count());
        let mut si = SIExec {
            sinstrs: Vec::with_capacity(20),
            numbers: vm.numbers,
            strings: vm.strings,
            values: vm.values,
            next_sinstr: SuperInstrReg(0),
            globals: vm.globals,
            string_buffer: vm.string_buffer,
            runtime_err_flag: Cell::new(false),
        };
        for section in cfg.node_indices() {
            map.insert(section, SuperInstr::new(&mut si, &vm.code[cfg[section].0.clone()]));
        }
        todo!()
    }

    regs!(num, NumberReg, numbers, Number);
    regs!(str, StringReg, strings, YString);
    regs!(val, ValueReg, values, Value);

    /// Get a superinstruction from a superinstruction register.
    ///
    /// # Safety
    ///
    /// This function is safe iff the following are true:
    /// * `self.sinstrs` is never mutated while one of the returned references exists
    /// * `self.sinstrs` isn't dropped while a reference is being held
    #[inline]
    unsafe fn get_sinstr<'a, 'b>(&'a self, reg: SuperInstrReg) -> &'b SuperInstr {
        let sinstr = if cfg!(debug_assertions) {
            &self.sinstrs[reg.0 as usize]
        } else {
            self.sinstrs.get_unchecked(reg.0 as usize)
        };
        std::mem::transmute(sinstr)
    }

    #[inline]
    unsafe fn get_buffer<'a>(&'a self) -> &'a mut YString {
        &mut *self.string_buffer.get()
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

    fn make_script(src: &str) -> SIExec {
        let script: parser::raw::Script = src.parse().unwrap();
        let script: parser::cst::Script = script.try_into().unwrap();
        let script: parser::pre_ast::Script = script.try_into().unwrap();
        let script: parser::ast::Script = script.try_into().unwrap();
        SIExec::new(script.into())
    }

    fn test(src: &str, lines: usize) {
        let mut vm = make_script(src);
        for _ in 0..lines {
            vm.step();
        }
        let (name, output) = vm.globals().next().unwrap();
        debug_assert_eq!(name.to_ascii_lowercase(), "output");
        debug_assert_eq!(output, Value::Str("ok".to_string().into()));
    }

    #[test]
    fn string_test() {
        test("\
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
        ", 30);
    }

    #[test]
    fn prec_test_0() {
        test("\
            num=1 x=(0 and 0 or 1 ) y=0 if x!=y then goto19 end num++\n\
            x=((0 and 0) or 1 ) y= 1 if x!=y then goto19 end num++\n\
            x=(0 and (0 or 1) ) y= 0 if x!=y then goto19 end num++\n\
            x=(5+5-6 ) y= 4 if x!=y then goto19 end num++\n\
            x=(-6+5+5 ) y= 4 if x!=y then goto19 end num++\n\
            x=(5-6+5 ) y= 4 if x!=y then goto19 end num++\n\
            x=(2*5/4 ) y=2.5 if x!=y then goto19 end num++\n\
            x=(10/(2*4) ) y= 1.25 if x!=y then goto19 end num++\n\
            x=(10/2*4 ) y= 20 if x!=y then goto19 end num++\n\
            x=(2+2*2 ) y= 6 if x!=y then goto19 end num++\n\
            a=1 x=(5*a++ ) y= 10 if x!=y then goto19 end num++\n\
            a=2 x=(5*a-- ) y= 5 if x!=y then goto19 end num++\n\
            a=2 x=(-a++ ) y= -3 if x!=y then goto19 end num++\n\
            a=2 x=(-a! ) y= -2  if x!=y then goto19 end num++\n\
            a=2 x=(-(a!) ) y= -2 if x!=y then goto19 end num++\n\
            if num != 16 then :OUTPUT=\"Skipped: \"+(16-num)+\" tests\" goto 20 end\n\
            :OUTPUT=\"ok\" goto20\n\
            :OUTPUT=\"Failed test #\"+num+\" got: \"+x+\" but wanted: \"+y\n\
        ", 20);
    }

    #[test]
    fn prec_test_1() {
        test("\
            num=1 x=(sqrt 3! ) y=2.449 if x!=y then goto19 end num++\n\
            x=(sqrt (3!) ) y=2.449 if x!=y then goto19 end num++\n\
            x=((sqrt 9) ) y=3 if x!=y then goto19 end num++\n\
            x=((abs 3) ) y=3 if x!=y then goto19 end num++\n\
            a=2+2 x=(a! ) y=24  if x!=y then goto19 end num++\n\
            x=(2+3! ) y=8 if x!=y then goto19 end num++\n\
            x=(2*3! ) y=12 if x!=y then goto19 end num++\n\
            a=-3 x=(a! ) y=-9223372036854775.808 if x!=y then goto19 end num++\n\
            a=-3 x=(abs a! ) y=-9223372036854775.808 if x!=y then goto19 end num++\n\
            a=-3 x=(abs (a!) ) if x!=y then goto19 end num++\n\
            \n\
            \n\
            \n\
            \n\
            \n\
            \n\
            if num != 11 then :OUTPUT=\"Skipped: \"+(11-num)+\" tests\" goto 20 end\n\
            :OUTPUT=\"ok\" goto20\n\
            :OUTPUT=\"Failed test #\"+num+\" got: \"+x+\" but wanted: \"+y\n\
        ", 20);
    }

    #[test]
    fn prec_test_2() {
        test("\
        num=1 x=(2*2^2 ) y= 8 if x!=y then goto19 end num++\n\
        x=(2+2^2 ) y= 6 if x!=y then goto19 end num++\n\
        x=(-2^2 ) y= 4 if x!=y then goto19 end num++\n\
        x=(-(2^2) ) y= -4 if x!=y then goto19 end num++\n\
        x=(sqrt 3+6 ) y= 7.732 if x!=y then goto19 end num++\n\
        x=(sqrt (3+6) ) y= 3 if x!=y then goto19 end num++\n\
        x=(sqrt 3*3 ) y= 5.196 if x!=y then goto19 end num++\n\
        x=(abs -5+5 ) y= 10 if x!=y then goto19 end num++\n\
        x=(abs (-5+5) ) y= 0 if x!=y then goto19 end num++\n\
        x=(sin (1^2) ) y= 0.017 if x!=y then goto19 end num++\n\
        x=((sin 1)^2 ) y= 0 if x!=y then goto19 end num++\n\
        x=(sin 1^2 ) y= 0 if x!=y then goto19 end num++\n\
        x=(2+2>1+1 ) y= 4 if x!=y then goto19 end num++\n\
        x=(2+2>=1+1 ) y= 4 if x!=y then goto19 end num++\n\
        x=(2*2>1*1 ) y= 1 if x!=y then goto19 end num++\n\
        x=(2*2>=1*1 ) y= 1 if x!=y then goto19 end num++\n\
        if num != 17 then :OUTPUT=\"Skipped: \"+(17-num)+\" tests\" goto 20 end\n\
        :OUTPUT=\"ok\" goto20\n\
        :OUTPUT=\"Failed test #\"+num+\" got: \"+x+\" but wanted: \"+y\n\
        ", 20);
    }

    #[test]
    fn prec_test_3() {
        test("\
        num=1 x=(2*(2>1)*1 ) y= 2 if x!=y then goto19 end num++\n\
        x=(2^2>1^1 ) y= 1 if x!=y then goto19 end num++\n\
        x=(2+1==1+2 ) y= 5 if x!=y then goto19 end num++\n\
        x=(2*1==1*2 ) y= 1 if x!=y then goto19 end num++\n\
        x=(0==1>1==1 ) y= 0 if x!=y then goto19 end num++\n\
        x=((0==1)>(1==1) ) y= 0 if x!=y then goto19 end num++\n\
        x=(0==(1>1)==1 ) y= 1 if x!=y then goto19 end num++\n\
        x=((((0==1)>1)==1) ) y= 0 if x!=y then goto19 end num++\n\
        x=(0>1==0 ) y= 1 if x!=y then goto19 end num++\n\
        x=((0>1)==0 ) y= 1 if x!=y then goto19 end num++\n\
        x=(0>(1==0) ) y= 0 if x!=y then goto19 end num++\n\
        x=(0==(0 or 1)==1 ) y= 0 if x!=y then goto19 end num++\n\
        x=(0==0 or 1==1 ) y= 1 if x!=y then goto19 end num++\n\
        x=(1 or 0 == 0 ) y= 1 if x!=y then goto19 end num++\n\
        x=((1 or 0) == 0 ) y= 0 if x!=y then goto19 end num++\n\
        x=(1 or (0 == 0) ) y= 1 if x!=y then goto19 end num++\n\
        if num != 17 then :OUTPUT=\"Skipped: \"+(17-num)+\" tests\" goto 20 end\n\
        :OUTPUT=\"ok\" goto20\n\
        :OUTPUT=\"Failed test #\"+num+\" got: \"+x+\" but wanted: \"+y\n\
        ", 20);
    }

    #[test]
    fn prec_test_4() {
        test("\
        num=1 x=(not 1+1 ) y=0 if x!=y then goto19 end num++\n\
        x=(not 0+1 ) y=0 if x!=y then goto19 end num++\n\
        x=(not 0+0 ) y=1 if x!=y then goto19 end num++\n\
        x=(not (1+1) ) y=0 if x!=y then goto19 end num++\n\
        x=((not 1)+1 ) y=1 if x!=y then goto19 end num++\n\
        x=((not 0)+1 ) y=2 if x!=y then goto19 end num++\n\
        x=(not (1 and 1) ) y=0 if x!=y then goto19 end num++\n\
        x=(not (1 and 0) ) y=1 if x!=y then goto19 end num++\n\
        x=((not 1) and 1 )  y=0 if x!=y then goto19 end num++\n\
        x=((not 0) and 1 ) y=1 if x!=y then goto19 end num++\n\
        x=((not 0) and 0 ) y=0 if x!=y then goto19 end num++\n\
        x=(1 and not 0 and 1) y=1 if x!=y then goto19 end num++\n\
        x=(1 and not 1 and 1) y=0 if x!=y then goto19 end num++\n\
        x=(1 and not 0 and 0) y=0 if x!=y then goto19 end num++\n\
        x=(1 and not (0 and 0)) y=1 if x!=y then goto19 end num++\n\
        x=(1 and not 0) y=1 if x!=y then goto19 end num++\n\
        if num != 17 then :OUTPUT=\"Skipped: \"+(17-num)+\" tests\" goto 20 end\n\
        :OUTPUT=\"ok\" goto20\n\
        :OUTPUT=\"Failed test #\"+num+\" got: \"+x+\" but wanted: \"+y\n\
        ", 20);
    }

    //#[test]
    fn prec_test_5() {
        test("\
        num=1 x=not(not 0) y=0 ifx!=y thengoto19end num++\n\
        x=not(not 1) y=1 ifx!=y thengoto19end num++\n\
        x=(not 0) and not 0 y=1 ifx!=y thengoto19end num++\n\
        x=(not 0) and not 1 y=0 ifx!=y thengoto19end num++\n\
        x=1+(not 1) y=1 ifx!=y thengoto19end num++\n\
        x=1+(not 0) y=2 ifx!=y thengoto19end num++\n\
        x=not 1+1 y=0 ifx!=y thengoto19end num++\n\
        x=not 0+1 y=0 ifx!=y thengoto19end num++\n\
        x=not 0+0 y=1 ifx!=y thengoto19end num++\n\
        \n\
        \n\
        \n\
        \n\
        \n\
        \n\
        \n\
        if num != 17 then :OUTPUT=\"Skipped: \"+(10-num)+\" tests\" goto 20 end\n\
        :OUTPUT=\"ok\" goto20\n\
        :OUTPUT=\"Failed test #\"+num+\" got: \"+x+\" but wanted: \"+y\n\
        ", 20);
    }
}
