use super::*;

/// Combine sections together.
///
/// # Conditions
///
/// ## Contiguous sections
///
/// All of the following conditions must be true to be able to combine sections together. Assume
/// we're given two sections `S1` and `S2`:
///
/// 1. `S1` and `S2` must be *contiguous*: the success action of `S1` must be to go to *only* `S2`.
///
///     To be clear, this does allow `S1` to have jumps to other sections.
/// 
/// 2. `S2` must *not* be a line start.
/// 
/// 3. `S2` must only be reachable by `S1`.
/// 
///     Restriction 2 means we don't have to check for error-jumps, but we still have to check for
///     multiple sections being able to reach `S2` (either through success actions or
///     conditional-jumps).
///
/// 4. `S2` is not the start section.
///
/// 5. `S1` != `S2`.
#[derive(Debug, Default)]
pub(super) struct CombineSections;

impl CombineSections {
    fn combine_sections(vm: &mut IRMachine, s1: Section, s2: Section) {
        let mut s2_code = remove_section(&mut vm.lines, &mut vm.sections, s2);
        let fixup = fixup_removed_section(s2, &mut s2_code);
        debug_assert!(fixup);
        let s1_code = &mut vm.sections[s1.0];
        s1_code.instrs.append(&mut s2_code.instrs);
        s1_code.success = s2_code.success;
    }
}

impl Optimization for CombineSections {
    fn optimize(&mut self, vm: &mut IRMachine) -> OptControlFlow {
        for s1 in 0..vm.sections.len() {
            if let Some(s2) = contiguous_to(&vm.sections[s1]) {
                if
                    Section(s1) != s2
                    && !vm.sections[s2.0].line_start
                    && vm.current_sect != s2
                    && only_reachable_once(s2, &vm.sections)
                {
                    Self::combine_sections(vm, Section(s1), s2);
                    return OptControlFlow::progressed(true);
                }
            }
        }
        OptControlFlow::no_progress(false)
    }
}

//struct ConstantGoto;

fn contiguous_to(section: &SectionCode) -> Option<Section> {
    if let SectionOrLine::Section(success) = section.success {
        Some(success)
    } else {
        None
    }
}

// does not check for s2 line start nor that s1 in particular actually reaches s2
fn only_reachable_once(s2: Section, sections: &Sections) -> bool {
    let mut reaches = 0_u32;
    for section in sections {
        match section.success {
            SectionOrLine::Section(snext) if snext == s2 => {
                reaches += 1;
            },
            _ => (),
        }

        for instr in section.instrs.iter() {
            match instr.get_section() {
                Some(snext) if snext == s2 => {
                    reaches += 1;
                },
                _ => (),
            }
        }
    }

    reaches <= 1
}