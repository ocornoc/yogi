use std::convert::*;
use std::time::Instant;

mod parser;
mod vm;
mod arith;

fn main() {
    let mut script = parser::raw::Script::default();
    script.0[0].0 =
        ":a=2 ++bc bc++ ++:bc :bc++ ++a c++ goto :a+:b"
        .chars()
        .collect();
    let script: parser::cst::Script = script.try_into().unwrap();
    let script: parser::pre_ast::Script = script.try_into().unwrap();
    let script: parser::ast::Script = script.try_into().unwrap();
    let mut vm = vm::VMExec::from(script);
    const NUM_LINES: usize = 100_000_000;
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
