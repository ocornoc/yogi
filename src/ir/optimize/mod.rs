use super::*;

mod sections;
mod instr_scheduling;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct OptControlFlow {
    progress: bool,
    repeat: bool,
}

#[allow(dead_code)]
impl OptControlFlow {
    const fn progressed(repeat: bool) -> Self {
        OptControlFlow {
            progress: true,
            repeat,
        }
    }

    const fn no_progress(repeat: bool) -> Self {
        OptControlFlow {
            progress: false,
            repeat,
        }
    }

    const fn repeat(progress: bool) -> Self {
        OptControlFlow {
            progress,
            repeat: true,
        }
    }

    const fn no_repeat(progress: bool) -> Self {
        OptControlFlow {
            progress,
            repeat: false,
        }
    }

    const fn repeat_if_progress(progress: bool) -> Self {
        if progress {
            Self::progressed(true)
        } else {
            Self::no_progress(false)
        }
    }
}

trait Optimization {
    fn initialize(&mut self, vm: &IRMachine) {
        let _ = vm;
    }

    fn optimize(&mut self, vm: &mut IRMachine) -> OptControlFlow;
}

pub(super) fn optimize(vm: &mut IRMachine) {
    let mut optimizations: Vec<Box<dyn Optimization>> = vec![
        Box::new(sections::CombineSections),
        Box::new(instr_scheduling::ReorderInstrs),
    ];
    let mut progress = true;
    remove_unfixed_sections(&mut vm.lines, &mut vm.sections);
    while progress {
        progress = false;
        for optimizer in optimizations.iter_mut() {
            optimizer.initialize(vm);
            let mut repeat = true;
            while repeat {
                let control_flow = optimizer.optimize(vm);
                repeat = control_flow.repeat;
                progress |= control_flow.progress;
            }
        }
    }
}

fn checked_remove_numreg(
    idents: &Idents,
    regs: &mut Numbers,
    sections: &mut Sections,
    reg: NumReg,
) {
    assert!(idents.iter().all(|(_, &r)| r != reg));
    remove_numreg(regs, sections, reg);
}

fn remove_numreg(regs: &mut Numbers, sections: &mut Sections, reg: NumReg) {
    regs.remove(reg.0);
    for section in sections {
        for instr in section.instrs.iter_mut() {
            instr.remove_reg(reg.into());
        }

        if let SectionOrLine::Line(ref mut r) = section.success {
            debug_assert_ne!(*r, reg, "Tried to remove existing reg from section");
            if *r > reg {
                r.0 -= 1;
            }
        }
    }
}

fn checked_remove_strreg(
    idents: &Idents,
    regs: &mut Strings,
    sections: &mut Sections,
    reg: StrReg,
) {
    assert!(idents.iter().all(|(_, &r)| r != reg));
    remove_strreg(regs, sections, reg);
}

fn remove_strreg(regs: &mut Strings, sections: &mut Sections, reg: StrReg) {
    regs.remove(reg.0);
    for section in sections {
        for instr in section.instrs.iter_mut() {
            instr.remove_reg(reg.into());
        }
    }
}

fn checked_remove_valreg(
    idents: &Idents,
    regs: &mut Values,
    sections: &mut Sections,
    reg: ValReg,
) {
    assert!(idents.iter().all(|(_, &r)| r != reg));
    remove_valreg(regs, sections, reg);
}

fn remove_valreg(regs: &mut Values, sections: &mut Sections, reg: ValReg) {
    regs.remove(reg.0);
    for section in sections {
        for instr in section.instrs.iter_mut() {
            instr.remove_reg(reg.into());
        }
    }
}

fn remove_section(lines: &mut Lines, sections: &mut Sections, section: Section) -> SectionCode {
    for line in lines {
        assert_ne!(*line, section, "Tried to remove line-start section");
        if *line > section {
            line.0 -= 1;
        }
    }

    let original = sections.remove(section.0);

    for code in sections {
        for instr in code.instrs.iter_mut() {
            instr.remove_section(section);
        }

        if let SectionOrLine::Section(ref mut s) = code.success {
            if *s >= section {
                s.0 -= 1;
            }
        }
    }

    original
}

fn fixup_removed_section(section: Section, code: &mut SectionCode) -> bool {
    for instr in code.instrs.iter_mut() {
        if instr.checked_remove_section(section) {
            return false;
        }
    }

    if let SectionOrLine::Section(ref mut s) = code.success {
        if *s > section {
            s.0 -= 1;
        } else if *s == section {
            return false;
        }
    }

    true
}

fn remove_unfixed_sections(lines: &mut Lines, sections: &mut Sections) {
    let remove = sections
        .iter()
        .enumerate()
        .filter(|(_, s)| s.success == SUCCESS_NEEDS_FIXING)
        .map(|(i, _)| Section(i))
        .rev()
        .collect::<Vec<_>>();
    for section in remove {
        remove_section(lines, sections, section);
    }
}