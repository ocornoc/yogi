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
    sinters: Vec<SuperInstr>,
    numbers: Vec<UnsafeCell<Number>>,
    strings: Vec<UnsafeCell<YString>>,
    values: Vec<UnsafeCell<Value>>,
    next_sinstr: SuperInstrReg,
    globals: AHashMap<String, AnyReg>,
    string_buffer: UnsafeCell<YString>,
}

impl SIExec {
    pub fn new(vm: VMExec) -> Self {
        let mut cfg = vm.control_flow_graph();
        cfg.clean_up();
        let mut map = AHashMap::with_capacity(cfg.node_count());
        let mut si = SIExec {
            sinters: Vec::with_capacity(20),
            numbers: vm.numbers,
            strings: vm.strings,
            values: vm.values,
            next_sinstr: SuperInstrReg(0),
            globals: vm.globals,
            string_buffer: vm.string_buffer,
        };
        for section in cfg.node_indices() {
            map.insert(section, SuperInstr::new(&mut si, &vm.code[cfg[section].0.clone()]));
        }
        todo!()
    }

    regs!(num, NumberReg, numbers, Number);
    regs!(str, StringReg, strings, YString);
    regs!(val, ValueReg, values, Value);

    unsafe fn get_buffer<'a>(&'a self) -> &'a mut YString {
        &mut *self.string_buffer.get()
    }
}
