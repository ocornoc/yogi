use super::*;

pub(super) fn optimize(vm: &mut IRMachine) {
    
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
            debug_assert_ne!(*s, section, "Tried to remove existing section");
            if *s > section {
                s.0 -= 1;
            }
        }
    }

    original
}