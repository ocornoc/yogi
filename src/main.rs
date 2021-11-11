use std::convert::*;
use std::time::Instant;

mod parser;
mod arith;
/* 
mod vm;

fn script() -> parser::raw::Script {
    "\
    :done++ b=97 c=89\n\
    :o++ :done++\n\
    :done++ x-- x=\"abc\" x=atan x\n\
    i=(127-1) _=(i/3%1==0)*i/3>1+(i/5%1==0)*i/5>1+(i/7%1==0)*i/7>1 a=i/11%1==0 x=atan x\n\
    _+=a*i/11>1+(i/13%1==0)*i/13>1+(i/17%1==0)*i/17>1+(i/19%1==0)*i/19>1 x=atan x\n\
    _+=(i/23%1==0)*i/23>1+(i/29%1==0)*i/29>1+(i/31%1==0)*i/31>1a=i/37%1==0 x=atan x\n\
    _+=a*i/37>1+(i/41%1==0)*i/41>1+(i/43%1==0)*i/43>1+(i/47%1==0)*i/47>1 x=atan x\n\
    _+=(i/53%1==0)*i/53>1+(i/59%1==0)*i/59>1+(i/61%1==0)*i/61>1a=i/67%1==0 x=atan x\n\
    _+=a*i/67>1+(i/71%1==0)*i/71>1+(i/73%1==0)*i/73>1+(i/79%1==0)*i/79>1 x=atan x\n\
    _+=(i/83%1==0)*i/83>1+(i/c%1==0)*i/c>1+(i/b%1==0)*i/b>1:o+=_<1:done++ x=atan x\n\
    a=1 if _ then a=2 else a=\"2\" end _/=a\n\
    z=:o :done++goto 4\n\
    ".parse().unwrap()
}

fn set_core_affinity() {
    core_affinity::set_for_current(core_affinity::get_core_ids()
        .unwrap()
        .into_iter()
        .last()
        .unwrap()
    );
}

fn fs_main() {
    const NUM_LINES: usize = 100_000;
    set_core_affinity();
    let script = script();
    let script: parser::cst::Script = script.try_into().unwrap();
    let script: parser::pre_ast::Script = script.try_into().unwrap();
    let script: parser::ast::Script = script.try_into().unwrap();
    let mut vm = vm::VMExec::from(script);
    for _ in 0..NUM_LINES {
        vm.step();
    }
}
*/

fn main() {
    /*
    if firestorm::enabled() {
        firestorm::bench("./flames/", fs_main).unwrap();
    } else {
        const NUM_LINES: usize = 100_000_000;
        set_core_affinity();
        let script = script();
        let script: parser::cst::Script = script.try_into().unwrap();
        let script: parser::pre_ast::Script = script.try_into().unwrap();
        let script: parser::ast::Script = script.try_into().unwrap();
        let mut vm = vm::VMExec::from(script);
        let mut graph = vm.control_flow_graph();
        graph.clean_up();
        //println!("CFG:\n{}", graph);
        //println!("DFG:\n{}", graph.dfg(&vm));
        println!("{}", graph.domg().display(&graph));
        loop {
            let start = Instant::now();
            for _ in 0..NUM_LINES {
                vm.step();
            }
            let done = Instant::now();
            let dur = done - start;
            let time_taken = dur.as_secs_f32();
            let lines_per_sec = NUM_LINES as f32 / time_taken;
            let ns_per_line = dur.as_nanos() as f32 / NUM_LINES as f32;
            println!(
                "finished execution of {} lines in {}s at a rate of {} lines/sec ({} ns/line).",
                NUM_LINES,
                time_taken,
                lines_per_sec,
                ns_per_line,
            );
            println!("globals as of right now");
            for (name, val) in vm.globals() {
                println!("{}: {}", name, val);
            }
        }
    }
    */
}
