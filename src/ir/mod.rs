use std::ops::{Deref, DerefMut};
use std::sync::atomic::{AtomicBool, Ordering};
use std::io::Write;
use std::fmt::{Formatter, Display, Result as FmtResult};
use derive_more::From;
use ahash::AHashMap;
use arith::*;
use parser::Ident;
use super::*;
use instr::*;
pub use codegen::CodegenOptions;

mod instr;
mod codegen;
mod optimize;

const SUCCESS_NEEDS_FIXING: SectionOrLine = SectionOrLine::Section(Section(!0));

#[derive(Debug, Clone, Copy, PartialEq, Eq, From)]
enum SectionOrLine {
    Section(Section),
    Line(NumReg),
}

impl Display for SectionOrLine {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "goto {}", match self {
            SectionOrLine::Section(s) => s as &dyn Display,
            SectionOrLine::Line(n) => n,
        })
    }
}

#[derive(Debug, Clone)]
pub struct SectionCode {
    instrs: Vec<Instruction>,
    line_start: bool,
    success: SectionOrLine,
}

impl SectionCode {
    fn replace_duplicates(&mut self, nums: &mut Numbers, strs: &mut Strings) {
        let mut i = 0;
        while i < self.instrs.len() {
            match self.instrs[i].dup_replace_with(nums, strs) {
                Some(Some(splice)) => {
                    self.instrs[i] = splice;
                },
                Some(None) => {
                    self.instrs.remove(i);
                    continue;
                },
                None => (),
            }

            i += 1;
        }
    }
}

type Sections = Vec<SectionCode>;
type Lines = Vec<Section>;
type CurrentSect = Section;
type Numbers = Vec<DebugCell<Number>>;
type Strings = Vec<DebugCell<YString>>;
type Values = Vec<DebugCell<Value>>;
type Idents = AHashMap<Ident, AnyReg>;

#[derive(Debug)]
pub struct IRMachine {
    sections: Sections,
    lines: Lines,
    current_sect: CurrentSect,
    runtime_err: AtomicBool,
    numbers: Numbers,
    strings: Strings,
    values: Values,
    idents: Idents,
}

macro_rules! reg_fns {
    ($new_name:ident, $ref_name:ident, $mut_name:ident, $reg:tt, $val:ty, $field:ident) => {
        #[allow(dead_code)]
        fn $new_name(&mut self, val: $val) -> $reg {
            let len = self.$field.len();
            self.$field.push(val.into());
            $reg(len)
        }

        fn $ref_name(&self, reg: $reg) -> impl Deref<Target = $val> + '_ {
            self.$field[reg.0].get()
        }

        fn $mut_name(&self, reg: $reg) -> impl DerefMut<Target = $val> + '_ {
            self.$field[reg.0].get()
        }
    };
}

impl IRMachine {
    reg_fns!(new_num_reg, num_ref, num_mut, NumReg, Number, numbers);
    reg_fns!(new_str_reg, str_ref, str_mut, StrReg, YString, strings);
    reg_fns!(new_val_reg, val_ref, val_mut, ValReg, Value, values);

    #[profiling::function]
    fn execute_instr(&self, instr: Instruction) -> Option<Section> {
        match instr {
            Instruction::JumpSectionIf(sect, condition) => {
                profiling::scope!("execute_jump_section_if");
                debug_assert_ne!(
                    sect,
                    Section(!0),
                    "Failed to fix section {} when trying to JumpSectionIf.\nSection: {:#?}",
                    self.current_sect.0,
                    self.sections[self.current_sect.0],
                );
                return if self.num_ref(condition).as_bool() {
                    Some(sect)
                } else {
                    None
                };
            },
            Instruction::JumpIfError(sect) => {
                profiling::scope!("execute_jump_if_error");
                debug_assert_ne!(
                    sect,
                    Section(!0),
                    "Failed to fix section {} when trying to JumpIfError.\nSection: {:#?}",
                    self.current_sect.0,
                    self.sections[self.current_sect.0],
                );
                let runtime_err = self.runtime_err.swap(false, Ordering::Relaxed);
                if runtime_err {
                    return Some(sect);
                }
            },
            Instruction::CopyNum(from, to) => {
                profiling::scope!("execute_copy_num");
                debug_assert_ne!(from, to);
                *self.num_mut(to) = *self.num_ref(from);
            },
            Instruction::CopyStr(from, to) => {
                profiling::scope!("execute_copy_str");
                debug_assert_ne!(from, to);
                self.str_mut(to).clone_from(&self.str_ref(from));
            },
            Instruction::CopyVal(from, to) => {
                profiling::scope!("execute_copy_val");
                debug_assert_ne!(from, to);
                self.val_mut(to).clone_from(&self.val_ref(from));
            },
            Instruction::ValueifyNum(n, v) => {
                profiling::scope!("execute_valueify_num");
                *self.val_mut(v) = Value::Num(*self.num_ref(n));
            },
            Instruction::ValueifyStr(s, v) => {
                profiling::scope!("execute_valueify_str");
                let mut val = self.val_mut(v);
                if let Some(s) = val.as_ystring_mut() {
                    s
                } else {
                    *val = YString::default().into();
                    val.as_ystring_mut().unwrap()
                }.clone_from(&self.str_ref(s));
            },
            Instruction::NumberifyVal(v, n) => {
                profiling::scope!("execute_numberify_val");
                if let Some(vn) = self.val_ref(v).as_number() {
                    *self.num_mut(n) = vn;
                } else {
                    self.runtime_err.store(true, Ordering::Relaxed);
                }
            }, 
            Instruction::StringifyNum(n, s) => {
                profiling::scope!("execute_stringify_num");
                let mut s = self.str_mut(s);
                s.clear();
                self.num_ref(n).stringify_with_buffer(&mut s);
            },
            Instruction::StringifyVal(v, s) => {
                profiling::scope!("execute_stringify_val");
                let mut s = self.str_mut(s);
                s.clear();
                match &*self.val_ref(v) {
                    Value::Num(v) => {
                        v.stringify_with_buffer(&mut s);
                    },
                    Value::Str(v) => {
                        s.clone_from(v);
                    },
                }
            },
            Instruction::IsTruthyNum(n) => {
                profiling::scope!("execute_truthy_num");
                let mut n = self.num_mut(n);
                *n = n.as_bool().into();
            },
            Instruction::IsTruthyVal(v, n) => {
                profiling::scope!("execute_truthy_val");
                *self.num_mut(n) = self.val_ref(v).as_bool().into();
            },
            Instruction::NotNum(n) => {
                profiling::scope!("execute_not_num");
                let mut n = self.num_mut(n);
                *n = !*n;
            },
            Instruction::NotVal(v, n) => {
                profiling::scope!("execute_not_val");
                *self.num_mut(n) = !&*self.val_ref(v);
            },
            Instruction::AddNum(n1, n2) => {
                debug_assert_ne!(n1, n2);
                profiling::scope!("execute_add_num");
                *self.num_mut(n1) += *self.num_ref(n2);
            },
            Instruction::AddSelfNum(n) => {
                profiling::scope!("execute_add_self_num");
                let mut n = self.num_mut(n);
                let n2 = n.clone();
                *n += n2;
            },
            Instruction::AddStr(s1, s2) => {
                profiling::scope!("execute_add_str");
                debug_assert_ne!(s1, s2);
                *self.str_mut(s1) += &self.str_ref(s2);
            },
            Instruction::AddSelfStr(s) => {
                profiling::scope!("execute_add_self_str");
                self.str_mut(s).duplicate();
            },
            Instruction::AddVal(v1, v2) => {
                profiling::scope!("execute_add_val");
                debug_assert_ne!(v1, v2);
                *self.val_mut(v1) += &self.val_ref(v2);
            },
            Instruction::AddSelfVal(v) => {
                profiling::scope!("execute_add_self_val");
                match *self.val_mut(v) {
                    Value::Num(ref mut n) => {
                        let n2 = n.clone();
                        *n += n2;
                    },
                    Value::Str(ref mut s) => {
                        s.duplicate();
                    },
                }
            },
            Instruction::SubNum(n1, n2) => {
                profiling::scope!("execute_sub_num");
                debug_assert_ne!(n1, n2);
                *self.num_mut(n1) -= *self.num_ref(n2);
            },
            Instruction::SubStr(s1, s2) => {
                profiling::scope!("execute_sub_str");
                debug_assert_ne!(s1, s2);
                *self.str_mut(s1) -= &self.str_ref(s2);
            },
            Instruction::SubVal(v1, v2) => {
                profiling::scope!("execute_sub_val");
                debug_assert_ne!(v1, v2);
                *self.val_mut(v1) -= &self.val_ref(v2);
            },
            Instruction::SubSelfVal(v) => {
                profiling::scope!("execute_sub_self_val");
                match *self.val_mut(v) {
                    Value::Num(ref mut n) => {
                        *n = Number::ZERO;
                    },
                    Value::Str(ref mut s) => {
                        s.clear();
                    },
                }
            },
            Instruction::Mul(n1, n2) => {
                profiling::scope!("execute_mul");
                debug_assert_ne!(n1, n2);
                let mut n = self.num_mut(n1);
                let n2 = self.num_ref(n2).clone();
                *n *= n2;
            },
            Instruction::MulSelf(n) => {
                profiling::scope!("execute_mul_self");
                let mut n = self.num_mut(n);
                let n2 = n.clone();
                *n *= n2;
            },
            Instruction::Div(n1, n2) => {
                profiling::scope!("execute_div");
                debug_assert_ne!(n1, n2);
                let mut n = self.num_mut(n1);
                let n2 = self.num_ref(n2).clone();
                if let Ok(v) = *n / n2 {
                    *n = v;
                } else {
                    self.runtime_err.store(true, Ordering::Relaxed);
                }
            },
            Instruction::DivSelf(n) => {
                profiling::scope!("execute_div_self");
                let mut n = self.num_mut(n);
                let n2 = n.clone();
                if let Ok(v) = *n / n2 {
                    *n = v;
                } else {
                    self.runtime_err.store(true, Ordering::Relaxed);
                }
            },
            Instruction::Rem(n1, n2) => {
                profiling::scope!("execute_rem");
                debug_assert_ne!(n1, n2);
                let mut n = self.num_mut(n1);
                let n2 = self.num_ref(n2).clone();
                if let Ok(v) = *n % n2 {
                    *n = v;
                } else {
                    self.runtime_err.store(true, Ordering::Relaxed);
                }
            },
            Instruction::RemSelf(n) => {
                profiling::scope!("execute_rem_self");
                let mut n = self.num_mut(n);
                let n2 = n.clone();
                if let Ok(v) = *n % n2 {
                    *n = v;
                } else {
                    self.runtime_err.store(true, Ordering::Relaxed);
                }
            },
            Instruction::Pow(n1, n2) => {
                profiling::scope!("execute_pow");
                debug_assert_ne!(n1, n2);
                let mut n = self.num_mut(n1);
                let n2 = self.num_ref(n2).clone();
                n.pow_assign(n2);
            },
            Instruction::PowSelf(n) => {
                profiling::scope!("execute_pow_self");
                let mut n = self.num_mut(n);
                let n2 = n.clone();
                n.pow_assign(n2);
            },
            Instruction::Eq(l, r, out) => {
                profiling::scope!("execute_eq_val");
                debug_assert_ne!(l, r);
                let l = &*self.val_ref(l);
                let r = &*self.val_ref(r);
                *self.num_mut(out) = (l == r).into();
            },
            Instruction::Le(l, r, out) => {
                profiling::scope!("execute_le_val");
                debug_assert_ne!(l, r);
                let l = &*self.val_ref(l);
                let r = &*self.val_ref(r);
                *self.num_mut(out) = (l <= r).into();
            },
            Instruction::Lt(l, r, out) => {
                profiling::scope!("execute_lt_val");
                debug_assert_ne!(l, r);
                let l = &*self.val_ref(l);
                let r = &*self.val_ref(r);
                *self.num_mut(out) = (l < r).into();
            },
            Instruction::IncNum(n) => {
                profiling::scope!("execute_inc_num");
                self.num_mut(n).pre_inc();
            },
            Instruction::IncStr(s) => {
                profiling::scope!("execute_inc_str");
                self.str_mut(s).pre_inc();
            },
            Instruction::IncVal(v) => {
                profiling::scope!("execute_inc_val");
                self.val_mut(v).pre_inc();
            },
            Instruction::DecNum(n) => {
                profiling::scope!("execute_dec_num");
                self.num_mut(n).pre_dec();
            },
            Instruction::DecStr(s) => {
                profiling::scope!("execute_dec_str");
                let out = self.str_mut(s).pre_dec();
                self.runtime_err.store(out.is_err(), Ordering::Relaxed);
            },
            Instruction::DecVal(v) => {
                profiling::scope!("execute_dec_val");
                let out = self.val_mut(v).pre_dec();
                self.runtime_err.store(out.is_err(), Ordering::Relaxed);
            },
            Instruction::Abs(n) => {
                profiling::scope!("execute_abs");
                let mut n = self.num_mut(n);
                *n = n.abs();
            },
            Instruction::Fact(n) => {
                profiling::scope!("execute_fact");
                let mut n = self.num_mut(n);
                *n = n.fact();
            },
            Instruction::Sqrt(n) => {
                profiling::scope!("execute_sqrt");
                let mut n = self.num_mut(n);
                *n = n.sqrt();
            },
            Instruction::Sin(n) => {
                profiling::scope!("execute_sin");
                let mut n = self.num_mut(n);
                *n = n.sin();
            },
            Instruction::Cos(n) => {
                profiling::scope!("execute_cos");
                let mut n = self.num_mut(n);
                *n = n.cos();
            },
            Instruction::Tan(n) => {
                profiling::scope!("execute_tan");
                let mut n = self.num_mut(n);
                *n = n.tan();
            },
            Instruction::Asin(n) => {
                profiling::scope!("execute_asin");
                let mut n = self.num_mut(n);
                *n = n.asin();
            },
            Instruction::Acos(n) => {
                profiling::scope!("execute_acos");
                let mut n = self.num_mut(n);
                *n = n.acos();
            },
            Instruction::Atan(n) => {
                profiling::scope!("execute_atan");
                let mut n = self.num_mut(n);
                *n = n.atan();
            },
            Instruction::Neg(n) => {
                profiling::scope!("execute_neg");
                let mut n = self.num_mut(n);
                *n = -*n;
            },
            Instruction::And(n1, n2) => {
                profiling::scope!("execute_and");
                debug_assert_ne!(n1, n2);
                let mut n = self.num_mut(n1);
                let n2 = if n1 == n2 {
                    n.clone()
                } else {
                    self.num_ref(n2).clone()
                };
                *n = (n.as_bool() && n2.as_bool()).into();
            },
            Instruction::Or(n1, n2) => {
                profiling::scope!("execute_or");
                debug_assert_ne!(n1, n2);
                let mut n = self.num_mut(n1);
                let n2 = if n1 == n2 {
                    n.clone()
                } else {
                    self.num_ref(n2).clone()
                };
                *n = (n.as_bool() || n2.as_bool()).into();
            },
        };
        None
    }

    fn replace_duplicates(&mut self) {
        for code in self.sections.iter_mut() {
            code.replace_duplicates(&mut self.numbers, &mut self.strings);
        }
    }

    #[profiling::function]
    fn execute_sect<const FIRST: bool>(&mut self) -> bool {
        let sect = &self.sections[self.current_sect.0];
        if !FIRST && sect.line_start {
            return false;
        }
        for &instr in sect.instrs.iter() {
            if let Some(new_sect) = self.execute_instr(instr) {
                debug_assert_ne!(
                    new_sect,
                    Section(!0),
                    "Failed to fix section {}.\nSection: {:#?}",
                    self.current_sect.0,
                    sect,
                );
                self.current_sect = new_sect;
                return false;
            }
        }
        match sect.success {
            SectionOrLine::Section(s) => {
                debug_assert_ne!(
                    s,
                    Section(!0),
                    "Failed to fix section {}.\nSection: {:#?}",
                    self.current_sect.0,
                    sect,
                );
                self.current_sect = s;
                true
            },
            SectionOrLine::Line(l) => {
                let line = self.num_ref(l).as_f32() as usize;
                self.current_sect = self.lines[line.clamp(1, self.lines.len()) - 1];
                false
            },
        }
    }

    #[profiling::function]
    pub fn step(&mut self) {
        let mut running = true;
        self.execute_sect::<true>();

        while running {
            running &= self.execute_sect::<false>();
            if std::mem::take(self.runtime_err.get_mut()) {
                panic!(
                    "After stepping through section {}, failed to handle runtime error.",
                    self.current_sect.0,
                );
            }
        }

        profiling::finish_frame!();
    }

    pub fn step_repeat(&mut self, reps: usize) {
        for _ in 0..reps {
            self.step();
        }
    }

    pub fn get_ident_value(&self, ident: &Ident) -> Value {
        match self.idents.get(ident) {
            Some(&AnyReg::Num(n)) => self.num_ref(n).deref().clone().into(),
            Some(&AnyReg::Str(s)) => self.str_ref(s).deref().clone().into(),
            Some(&AnyReg::Val(v)) => self.val_ref(v).deref().clone(),
            None => Value::Num(0.into()),
        }
    }

    pub fn idents(&self) -> impl IntoIterator<Item = (&Ident, Value)> + '_ {
        self.idents
            .iter()
            .map(|(s, _)| (s, self.get_ident_value(&s)))
    }

    pub fn set_ident(&mut self, ident: &Ident, val: Value) {
        if let Some(&reg) = self.idents.get(ident) {
            match (reg, val) {
                (AnyReg::Num(r), Value::Num(n)) => {
                    *self.num_mut(r) = n;
                },
                (AnyReg::Str(r), Value::Str(s)) => {
                    *self.str_mut(r) = s;
                },
                (AnyReg::Val(r), val) => {
                    *self.val_mut(r) = val;
                },
                (_, _) => panic!("Tried to set '{}' to incorrect type", ident),
            }
        }
    }

    pub fn print_bytecode(&self, mut sink: impl Write) -> std::io::Result<()> {
        fn print_val(vm: &IRMachine, r: AnyReg) -> String {
            match r {
                AnyReg::Num(n) => format!("{}", vm.num_ref(n).deref()),
                AnyReg::Str(s) => format!("\"{}\"", vm.str_ref(s).deref()),
                AnyReg::Val(v) => format!("{}", vm.val_ref(v).deref()),
            }
        }

        writeln!(sink, "Globals:")?;

        for (ident, &reg) in self.idents.iter() {
            writeln!(sink, "`{}` is {}", ident, reg)?;
        }

        writeln!(sink)?;

        for (i, section) in self.sections.iter().enumerate() {
            writeln!(sink, "Section #{}:", i)?;

            for instr in section.instrs.iter() {
                write!(sink, "\t{}", instr)?;
                let relevant = instr
                    .relevant()
                    .into_iter()
                    .filter(|&r| match r {
                        AnyReg::Num(n) => *self.num_ref(n) != Default::default(),
                        AnyReg::Str(_) => true,
                        AnyReg::Val(v) => *self.val_ref(v) != Default::default(),
                    })
                    .collect::<Vec<_>>();
                match relevant.as_slice() {
                    [] => (),
                    &[first] => write!(sink, "      ({} = {})", first, print_val(self, first))?,
                    [first, mid@..] => {
                        write!(sink, "      ({} = {}", first, print_val(self, *first))?;

                        for &reg in mid {
                            write!(sink, ", {} = {}", reg, print_val(self, reg))?;
                        }

                        write!(sink, ")")?;
                    },
                }
                writeln!(sink)?;
            }

            writeln!(sink, "\t{}", if section.success == SUCCESS_NEEDS_FIXING {
                &"Section end not expected to be reached." as &dyn Display
            } else {
                &section.success
            })?;

            writeln!(sink)?;
        }

        Ok(())
    }

    pub fn get_current_line(&self) -> Option<usize> {
        self.lines.iter().enumerate().find(|(_, &s)| s == self.current_sect).map(|(i, _)| i)
    }

    pub fn set_next_line(&mut self, line: usize) {
        self.current_sect = self.lines[line];
    }

    pub fn optimize(&mut self) {
        optimize::optimize(self);
    }
}

impl Display for IRMachine {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        let mut data = String::with_capacity(100_000);
        unsafe {
            self.print_bytecode(data.as_mut_vec() ).unwrap()
        };
        f.write_str(&data)
    }
}

impl Clone for IRMachine {
    fn clone(&self) -> Self {
        Self {
            sections: self.sections.clone(),
            lines: self.lines.clone(),
            current_sect: self.current_sect.clone(),
            runtime_err: self.runtime_err.load(Ordering::Relaxed).into(),
            numbers: self.numbers.clone(),
            strings: self.strings.clone(),
            values: self.values.clone(),
            idents: self.idents.clone(),
        }
    }

    fn clone_from(&mut self, source: &Self) {
        self.sections.clone_from(&source.sections);
        self.lines.clone_from(&source.lines);
        self.current_sect = source.current_sect;
        *self.runtime_err.get_mut() = source.runtime_err.load(Ordering::Relaxed);
        self.numbers.clone_from(&source.numbers);
        self.strings.clone_from(&source.strings);
        self.values.clone_from(&source.values);
        self.idents.clone_from(&source.idents);
    }
}

#[cfg(test)]
mod tests {
    use crate::simple_interp::SimpleInterp;
    use crate::parser::*;
    use super::*;

    fn tester(steps: usize, src: &(impl AsRef<str> + ?Sized)) {
        let program = YololParser::unrestricted().parse(src.as_ref()).unwrap();
        let mut simple_interp = SimpleInterp::new(program.clone());
        let mut ir_machine = IRMachine::from_ast(
            CodegenOptions {
                protect_locals: true,
                protect_globals: true,
            },
            program,
        );
        let mut optimized_ir_machine = ir_machine.clone();
        optimized_ir_machine.optimize();
        simple_interp.step_lines(steps);
        ir_machine.step_repeat(steps);
        optimized_ir_machine.step_repeat(steps);
        let values = simple_interp.values();
        assert_eq!(
            values[&Ident::global("output")],
            ir_machine.get_ident_value(&Ident::global("output")),
        );
        assert_eq!(
            values[&Ident::global("output")],
            optimized_ir_machine.get_ident_value(&Ident::global("output")),
            "unoptimized:\n{ir_machine}\noptimized:\n{optimized_ir_machine}",
        );
        for (ident, value) in values {
            assert_eq!(
                *value,
                ir_machine.get_ident_value(ident),
            );
            assert_eq!(
                *value,
                optimized_ir_machine.get_ident_value(ident),
                "unoptimized:\n{ir_machine}\noptimized:\n{optimized_ir_machine}",
            );
        }
    }

    #[test]
    fn multiply_huge()
    {
        tester(100, r#"x=asin999 c=1*x if c==0then:output="ok"else:output=":("end goto1"#);
    }

    #[test]
    fn acid_acos() {
        tester(100,
r#"x=acos(0)
u/=x!=90 :OUTPUT="Failed #1 : " + x goto10
x=acos(1)
u/=x!=0 :OUTPUT="Failed #2 : " + x goto10
x=acos(0.5)
u/=x!=60 :OUTPUT="Failed #3 : " + x goto10
x=acos(17)
u/=x!=-9223372036854775.808 :OUTPUT="Failed #4 : " + x goto10
:OUTPUT="ok"
goto10"#
        );
    }

    #[test]
    fn acid_asin() {
        tester(100,
r#"x=asin(0)
u/=x!=0 :OUTPUT="Failed #1 : " + x goto10
x=asin(1)
u/=x!=90 :OUTPUT="Failed #2 : " + x goto10
x=asin(0.5)
u/=x!=30 :OUTPUT="Failed #3 : " + x goto10
x=asin(17)
u/=x!=-9223372036854775.808 :OUTPUT="Failed #4 : " + x goto10
:OUTPUT="ok"
goto10"#
        );
    }

    #[test]
    fn acid_atan() {
        tester(100,
r#"x=atan(0)
u/=x!=0 :OUTPUT="Failed #1 : " + x goto10
x=atan(1)
u/=x!=45 :OUTPUT="Failed #2 : " + x goto10
x=atan(0.5)
u/=x!=26.565 :OUTPUT="Failed #3 : " + x goto10
x=atan(998877665544332)
u/=x!=90 :OUTPUT="Failed #4 : " + x goto10
:OUTPUT="ok"
goto10"#
        );
    }

    #[test]
    fn acid_exponents() {
        tester(100,
r#"x=2^4
u/=x!=16 :OUTPUT="Failed #1 : " + x goto12
x=2^40
u/=x!=1099511627776 :OUTPUT="Failed #2 : " + x goto12
x=2^70
u/=x!=-9223372036854775.808 :OUTPUT="Failed #3 : " + x goto12
x=2^71
u/=x!=-9223372036854775.808 :OUTPUT="Failed #4 : " + x goto12
x=17^17
u/=x!=-9223372036854775.808 :OUTPUT="Failed #5 : " + x goto12
:OUTPUT="ok"
goto12"#
        );
    }

    #[test]
    fn acid_modulus() {
        tester(100,
r#"n=1 x=10%7 y=3 if x!=y then goto19 end n++ 
x=10%3 y=1 if x!=y then goto19 end n++ 
x=10%3.1 y=0.7 if x!=y then goto19 end n++ 
x=10%-3 y=1 if x!=y then goto19 end n++ 
x=10%(-3) y=1 if x!=y then goto19 end n++ 
x=10%-3.1 y=0.7 if x!=y then goto19 end n++
x=10%0.7 y=0.2 if x!=y then goto19 end n++
x=10%-0.7 y=0.2 if x!=y then goto19 end n++








if n != 9 then :OUTPUT="Skipped: "+(9-n)+" tests" goto 20 end
:OUTPUT="ok" goto20
:OUTPUT="Failed test #"+n+" got: "+x+" but wanted: "+y
goto20"#
        );
    }

    #[test]
    fn acid_multiply() {
        tester(100,
r#"x=1 i=24 j=38 x*=3 x*=3 x*=3 x*=3 x*=3 
u/=x!=243 :OUTPUT="Failed #1 : " + x goto8
u/=i>0 i-- x*=3 goto3
u/=x!=-5156598929955.207 :OUTPUT="Failed #2 : " + x goto8
u/=j>0 j-- x*=3 goto5
u/=x!=-113000154446.553 :OUTPUT="Failed #2 : " + x goto8
:OUTPUT="ok"
goto8"#
        );
    }

    #[test]
    fn acid_precedence1() {
        tester(100,
r#"num=1 x=(0 and 0 or 1 ) y=0 if x!=y then goto19 end num++
x=((0 and 0) or 1 ) y= 1 if x!=y then goto19 end num++ 
x=(0 and (0 or 1) ) y= 0 if x!=y then goto19 end num++ 
x=(5+5-6 ) y= 4 if x!=y then goto19 end num++ 
x=(-6+5+5 ) y= 4 if x!=y then goto19 end num++ 
x=(5-6+5 ) y= 4 if x!=y then goto19 end num++ 
x=(2*5/4 ) y=2.5 if x!=y then goto19 end num++ 
x=(10/(2*4) ) y= 1.25 if x!=y then goto19 end num++ 
x=(10/2*4 ) y= 20 if x!=y then goto19 end num++ 
x=(2+2*2 ) y= 6 if x!=y then goto19 end num++ 
a=1 x=(5*a++ ) y= 10 if x!=y then goto19 end num++ 
a=2 x=(5*a-- ) y= 5 if x!=y then goto19 end num++ 
a=2 x=(-a++ ) y= -3 if x!=y then goto19 end num++ 
a=2 x=(-a! ) y= -2  if x!=y then goto19 end num++ 
a=2 x=(-(a!) ) y= -2 if x!=y then goto19 end num++ 

if num != 16 then :OUTPUT="Skipped: "+(16-num)+" tests" goto 20 end
:OUTPUT="ok" goto20
:OUTPUT="Failed test #"+num+" got: "+x+" but wanted: "+y
goto20"#
        );
    }

    #[test]
    fn acid_precedence2() {
        tester(100,
r#"num=1 x=(sqrt 3! ) y=2.449 if x!=y then goto19 end num++ 
x=(sqrt (3!) ) y=2.449 if x!=y then goto19 end num++ 
x=((sqrt 9) ) y=3 if x!=y then goto19 end num++ 
x=((abs 3) ) y=3 if x!=y then goto19 end num++ 
a=2+2 x=(a! ) y=24  if x!=y then goto19 end num++ 
x=(2+3! ) y=8 if x!=y then goto19 end num++ 
x=(2*3! ) y=12 if x!=y then goto19 end num++ 
a=-3 x=(a! ) y=-9223372036854775.808 if x!=y then goto19 end num++ 
a=-3 x=(abs a! ) y=-9223372036854775.808 if x!=y then goto19 end num++ 
a=-3 x=(abs (a!) ) if x!=y then goto19 end num++ 






if num != 11 then :OUTPUT="Skipped: "+(11-num)+" tests" goto 20 end
:OUTPUT="ok" goto20
:OUTPUT="Failed test #"+num+" got: "+x+" but wanted: "+y
goto20"#
        );
    }

    #[test]
    fn acid_precedence3() {
        tester(100,
r#"num=1 x=(2*2^2 ) y= 8 if x!=y then goto19 end num++ 
x=(2+2^2 ) y= 6 if x!=y then goto19 end num++ 
x=(-2^2 ) y= 4 if x!=y then goto19 end num++ 
x=(-(2^2) ) y= -4 if x!=y then goto19 end num++ 
x=(sqrt 3+6 ) y= 7.732 if x!=y then goto19 end num++ 
x=(sqrt (3+6) ) y= 3 if x!=y then goto19 end num++ 
x=(sqrt 3*3 ) y= 5.196 if x!=y then goto19 end num++ 
x=(abs -5+5 ) y= 10 if x!=y then goto19 end num++ 
x=(abs (-5+5) ) y= 0 if x!=y then goto19 end num++ 
x=(sin (1^2) ) y= 0.017 if x!=y then goto19 end num++ 
x=((sin 1)^2 ) y= 0 if x!=y then goto19 end num++ 
x=(sin 1^2 ) y= 0 if x!=y then goto19 end num++ 
x=(2+2>1+1 ) y= 4 if x!=y then goto19 end num++ 
x=(2+2>=1+1 ) y= 4 if x!=y then goto19 end num++ 
x=(2*2>1*1 ) y= 1 if x!=y then goto19 end num++ 
x=(2*2>=1*1 ) y= 1 if x!=y then goto19 end num++ 
if num != 17 then :OUTPUT="Skipped: "+(17-num)+" tests" goto 20 end
:OUTPUT="ok" goto20
:OUTPUT="Failed test #"+num+" got: "+x+" but wanted: "+y
goto20"#
        );
    }

    #[test]
    fn acid_precedence4() {
        tester(100,
r#"num=1 x=(2*(2>1)*1 ) y= 2 if x!=y then goto19 end num++ 
x=(2^2>1^1 ) y= 1 if x!=y then goto19 end num++ 
x=(2+1==1+2 ) y= 5 if x!=y then goto19 end num++ 
x=(2*1==1*2 ) y= 1 if x!=y then goto19 end num++ 
x=(0==1>1==1 ) y= 0 if x!=y then goto19 end num++ 
x=((0==1)>(1==1) ) y= 0 if x!=y then goto19 end num++ 
x=(0==(1>1)==1 ) y= 1 if x!=y then goto19 end num++ 
x=((((0==1)>1)==1) ) y= 0 if x!=y then goto19 end num++ 
x=(0>1==0 ) y= 1 if x!=y then goto19 end num++ 
x=((0>1)==0 ) y= 1 if x!=y then goto19 end num++ 
x=(0>(1==0) ) y= 0 if x!=y then goto19 end num++ 
x=(0==(0 or 1)==1 ) y= 0 if x!=y then goto19 end num++ 
x=(0==0 or 1==1 ) y= 1 if x!=y then goto19 end num++ 
x=(1 or 0 == 0 ) y= 1 if x!=y then goto19 end num++ 
x=((1 or 0) == 0 ) y= 0 if x!=y then goto19 end num++ 
x=(1 or (0 == 0) ) y= 1 if x!=y then goto19 end num++ 
if num != 17 then :OUTPUT="Skipped: "+(17-num)+" tests" goto 20 end
:OUTPUT="ok" goto20
:OUTPUT="Failed test #"+num+" got: "+x+" but wanted: "+y
goto20"#
        );
    }

    #[test]
    fn acid_precedence5() {
        tester(100,
r#"num=1 x=(not 1+1 ) y=0 if x!=y then goto19 end num++ 
x=(not 0+1 ) y=0 if x!=y then goto19 end num++ 
x=(not 0+0 ) y=1 if x!=y then goto19 end num++ 
x=(not (1+1) ) y=0 if x!=y then goto19 end num++ 
x=((not 1)+1 ) y=1 if x!=y then goto19 end num++ 
x=((not 0)+1 ) y=2 if x!=y then goto19 end num++ 
x=(not (1 and 1) ) y=0 if x!=y then goto19 end num++ 
x=(not (1 and 0) ) y=1 if x!=y then goto19 end num++ 
x=((not 1) and 1 )  y=0 if x!=y then goto19 end num++ 
x=((not 0) and 1 ) y=1 if x!=y then goto19 end num++ 
x=((not 0) and 0 ) y=0 if x!=y then goto19 end num++ 
x=(1 and not 0 and 1) y=1 if x!=y then goto19 end num++ 
x=(1 and not 1 and 1) y=0 if x!=y then goto19 end num++ 
x=(1 and not 0 and 0) y=0 if x!=y then goto19 end num++ 
x=(1 and not (0 and 0)) y=1 if x!=y then goto19 end num++ 
x=(1 and not 0) y=1 if x!=y then goto19 end num++ 
if num != 17 then :OUTPUT="Skipped: "+(17-num)+" tests" goto 20 end
:OUTPUT="ok" goto20
:OUTPUT="Failed test #"+num+" got: "+x+" but wanted: "+y
goto20"#
        );
    }

    #[test]
    fn acid_precedence6() {
        tester(100,
r#"num=1 x=not(not 0) y=0 ifx!=y thengoto19end num++ 
x=not(not 1) y=1 ifx!=y thengoto19end num++ 
x=(not 0) and not 0 y=1 ifx!=y thengoto19end num++ 
x=(not 0) and not 1 y=0 ifx!=y thengoto19end num++ 
x=1+(not 1) y=1 ifx!=y thengoto19end num++ 
x=1+(not 0) y=2 ifx!=y thengoto19end num++ 
x=not 1+1 y=0 ifx!=y thengoto19end num++ 
x=not 0+1 y=0 ifx!=y thengoto19end num++ 
x=not 0+0 y=1 ifx!=y thengoto19end num++ 







ifnum!=10then:OUTPUT="Skipped: "+(10-num)+" tests" goto20end
:OUTPUT="ok" goto20
:OUTPUT="Failed test #"+num+" got: "+x+" but wanted: "+y
goto20"#
        );
    }

    #[test]
    fn acid_sqrt() {
        tester(100,
r#"n=1 x=sqrt 24 y=4.899 if x!=y then goto19 end n++ 
x=(sqrt 2) y=1.414 if x!=y then goto19 end n++ 
x=(sqrt 7) y=2.645 if x!=y then goto19 end n++ 
x=(sqrt 32199) y=179.440 if x!=y then goto19 end n++ 
x=(sqrt 1000001) y=1000 if x!=y then goto19 end n++ 
x=(sqrt 1000002) y=1000.001 if x!=y then goto19 end n++ 
x=sqrt 9223372036854775.807 y=-9223372036854775.808 n++ goto19/(x!=y)
x=(sqrt -3) y=-9223372036854775.808 if x!=y then goto19 end n++ 
x=sqrt 9223372036854775 y=-9223372036854775.808 n++ goto19/(x!=y) 
x=sqrt 9223372036854774.999 y=96038388.349 n++ goto19/(x!=y)






if n != 11 then :OUTPUT="Skipped: "+(11-n)+" tests" goto 20 end
:OUTPUT="ok" goto20
:OUTPUT="Failed test #"+n+" got: "+x+" but wanted: "+y
goto20"#
        );
    }

    #[test]
    fn acid_string_length() {
        tester(10_000,
r#"a="字字字字字字字字字字字字字字字字字字字字" //20
b=a b+=b b+=b b+=b b+=b b+=b b+=b b+=b b+=b // b=7680chars
c+=b!="" b-- c+=b!="" b-- c+=b!="" b-- c+=b!="" b-- c+=b!="" b-- goto3
if c==1024 then :OUTPUT="ok" goto4 end
:OUTPUT="Wrong length! got: "+c+" but wanted: 1024" goto5"#
        );
    }

    #[test]
    fn acid_string_length_inv() {
        tester(10_000,
r#"a="字字字字字字字字字字字字字字字字字字字字" //20
b=a b+=b b+=b b+=b b+=b b+=b b+=b b+=b b+=b // b=7680chars
c+=b!="" b-- c+=b!="" b-- c+=b!="" b-- c+=b!="" b-- c+=b!="" b-- goto3
if c==1023 then :OUTPUT="ok" goto4 end
:OUTPUT="Wrong length! got: "+c+" but wanted: 1024" goto5"#
        );
    }

    #[test]
    fn acid_string_logic() {
        tester(100,
r#"num=1 if "" then goto 19 end num++
if "abc" then goto 19 end num++
if "1" then goto 19 end num++
if "0" then goto 19 end num++
if not "" then goto 19 end num++
if not "1" then goto 19 end num++
if not "0" then goto 19 end num++
if 1 and "" then goto 19 end num++
if 1 and "1" then goto 19 end num++
if 1 and "0" then goto 19 end num++
if not (1 or "") then goto 19 end num++
if not (1 or "1") then goto 19 end num++
if not (1 or "0") then goto 19 end num++
if 0 or "" then goto 19 end num++
if 0 or "1" then goto 19 end num++
if 0 or "0" then goto 19 end num++
if num != 17 then :OUTPUT="Skipped: "+(17-num)+" tests" goto 20 end
:OUTPUT="ok" goto20
:OUTPUT="Failed test #"+num
goto20"#
        );
    }

    #[test]
    fn acid_tan() {
        tester(100,
r#"x=tan(0)
u/=x!=0 :OUTPUT="Failed #1 : " + x goto8
x=tan(45)
u/=x!=1 :OUTPUT="Failed #2 : " + x goto8
x=tan(90)
u/=x!=-22877332.428 :OUTPUT="Failed #3 : " + x goto8
:OUTPUT="ok"
goto8"#
        );
    }

    #[test]
    fn many_lines() {
        tester(100, &("\n".repeat(30) + r#":output="ok" goto30"#));
    }

    #[test]
    fn black_friday_zijkhal() {
        let src =
r#":i="8591433801" a="*********"i=a+9p+=a goto++k/57
h=a--+8g=a--+7f=a--+6e=a--+5d=a--+4c=a--+3b=a--+2a="*1"
t=:i+:i q=p-0+t-a-b-c-d-e-f-g-h-i-0s=q+t z=s l=s-z--
q=q+l-a-b-c-d-e-f-g-h-i-0s=q+t z=s m=s-z--q=q+m-a-b-c-d-e-f-g-h-i-0s=q+t+t
z=s n=s-z--q=q+n-a-b-c-d-e-f-g-h-i-0:done=1s=q+t+t z=s :o=l+m+n+(s-z--)goto3"#;
        let program = YololParser::unrestricted().parse(src).unwrap();
        let mut simple_interp = SimpleInterp::new(program.clone());
        let mut vm = IRMachine::from_ast(
            CodegenOptions {
                protect_locals: true,
                protect_globals: true,
            },
            program,
        );
        let ident = Ident::global("done");
        while if let Some(v) = simple_interp.values().get(&ident) {
            !v.as_bool()
        } else {
            true
        } {
            simple_interp.step_line();
        }
        while !vm.get_ident_value(&ident).as_bool() {
            vm.step();
        }
        for (ident, value) in simple_interp.values() {
            assert_eq!(
                *value,
                vm.get_ident_value(ident),
            );
        }
    }

    #[test]
    fn rtl_test() {
        tester(100,
r#"s="hello" s=s-s-- n++ if s!="" then goto 20 end
s=5 s=s-s-- n++ if s!=0 then goto 20 end
s=5 s=s+s++ n++ if s!=12 then goto 20 end
s=5 s=(s--+(2*s++))+s++ n++ if s!=26 then goto 20 end
s=5 s=((2*s++)+s--)+s++ n++ if s!=23 then goto 20 end













:output="ok" goto 19
:output="Failed test "+n goto 20"#
        );
    }
}