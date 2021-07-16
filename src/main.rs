use std::convert::*;
use std::time::Instant;

mod parser;
mod vm;
mod arith;

fn script() -> parser::raw::Script {
    let mut script = parser::raw::Script::default();
    script.0[0].0 =
        ":done++ b=97 c=89"
        .chars()
        .collect();
    script.0[1].0 =
        ":o++ :done++"
        .chars()
        .collect();
    script.0[2].0 =
        ":done++ x-- x=\"abc\" x=atan x"
        .chars()
        .collect();
    script.0[3].0 =
        "i=(127-1) _=(i/3%1==0)*i/3>1+(i/5%1==0)*i/5>1+(i/7%1==0)*i/7>1 a=i/11%1==0 x=atan x"
        .chars()
        .collect();
    script.0[4].0 =
        "_+=a*i/11>1+(i/13%1==0)*i/13>1+(i/17%1==0)*i/17>1+(i/19%1==0)*i/19>1 x=atan x"
        .chars()
        .collect();
    script.0[5].0 =
        "_+=(i/23%1==0)*i/23>1+(i/29%1==0)*i/29>1+(i/31%1==0)*i/31>1a=i/37%1==0 x=atan x"
        .chars()
        .collect();
    script.0[6].0 =
        "_+=a*i/37>1+(i/41%1==0)*i/41>1+(i/43%1==0)*i/43>1+(i/47%1==0)*i/47>1 x=atan x"
        .chars()
        .collect();
    script.0[7].0 =
        "_+=(i/53%1==0)*i/53>1+(i/59%1==0)*i/59>1+(i/61%1==0)*i/61>1a=i/67%1==0 x=atan x"
        .chars()
        .collect();
    script.0[8].0 =
        "_+=a*i/67>1+(i/71%1==0)*i/71>1+(i/73%1==0)*i/73>1+(i/79%1==0)*i/79>1 x=atan x"
        .chars()
        .collect();
    script.0[9].0 =
        "_+=(i/83%1==0)*i/83>1+(i/c%1==0)*i/c>1+(i/b%1==0)*i/b>1:o+=_<1:done++ x=atan x"
        .chars()
        .collect();
    script.0[10].0 =
        "a=1 if _ then a=2 else a=\"2\" end _/=a"
        .chars()
        .collect();
    script.0[11].0 =
        "z=:o :done++goto 4"
        .chars()
        .collect();
    script
}


fn fs_main() {
    const NUM_LINES: usize = 10_000;
    let script = script();
    let script: parser::cst::Script = script.try_into().unwrap();
    let script: parser::pre_ast::Script = script.try_into().unwrap();
    let script: parser::ast::Script = script.try_into().unwrap();
    let mut vm = vm::VMExec::from(script);
    for _ in 0..NUM_LINES {
        vm.step();
    }
}

fn main() {
    if firestorm::enabled() {
        firestorm::bench("./flames/", fs_main).unwrap();
    } else {
        const NUM_LINES: usize = 1_000_000;
        let script = script();
        let script: parser::cst::Script = script.try_into().unwrap();
        let script: parser::pre_ast::Script = script.try_into().unwrap();
        let script: parser::ast::Script = script.try_into().unwrap();
        let mut vm = vm::VMExec::from(script);
        let start = Instant::now();
        for _ in 0..NUM_LINES {
            vm.step();
        }
        let done = Instant::now();
        let time_taken = (done - start).as_secs_f32();
        let lines_per_sec = NUM_LINES as f32 / time_taken;
        println!("finished execution in {}s at a rate of {} lines/sec.", time_taken, lines_per_sec);
        println!("Globals:");
        for (name, val) in vm.globals() {
            println!(":{} = {}", name, val);
        }
    }
}
