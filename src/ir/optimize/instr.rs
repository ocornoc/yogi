use super::*;

/* 
pub(super) struct RemoveRedundantInstrs;

impl Optimization for RemoveRedundantInstrs {
    fn optimize(&mut self, vm: &mut IRMachine) -> OptControlFlow {
        todo!()
    }
}
*/

#[derive(Default, Debug)]
pub struct ReorderInstrs;

impl Optimization for ReorderInstrs {
    fn optimize(&mut self, vm: &mut IRMachine) -> OptControlFlow {
        let mut progress = false;

        for code in vm.sections.iter_mut() {
            // cocktail shaker sort
            for j in 1..code.instrs.len() {
                let i = j - 1;
                if code.instrs[i].can_swap(code.instrs[j]) {
                    code.instrs.swap(i, j);
                    progress |= true;
                }
            }
            for j in (1..code.instrs.len()).into_iter().rev() {
                let i = j - 1;
                if code.instrs[i].can_swap(code.instrs[j]) {
                    code.instrs.swap(i, j);
                    progress |= true;
                }
            }
        }

        OptControlFlow::repeat_if_progress(progress)
    }
}