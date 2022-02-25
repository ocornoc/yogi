use std::time::Instant;
use yogi::*;
use parser::YololParser;

fn script() -> parser::Program {
    YololParser::unrestricted().parse(
r#":done++ b=97 c=89
:o++ :done++
:done++ x-- x="abc" x=atan x
i=(127-1) _=(i/3%1==0)*i/3>1+(i/5%1==0)*i/5>1+(i/7%1==0)*i/7>1 a=i/11%1==0 x=atan x
_+=a*i/11>1+(i/13%1==0)*i/13>1+(i/17%1==0)*i/17>1+(i/19%1==0)*i/19>1 x=atan x
_+=(i/23%1==0)*i/23>1+(i/29%1==0)*i/29>1+(i/31%1==0)*i/31>1a=i/37%1==0 x=atan x
_+=a*i/37>1+(i/41%1==0)*i/41>1+(i/43%1==0)*i/43>1+(i/47%1==0)*i/47>1 x=atan x
_+=(i/53%1==0)*i/53>1+(i/59%1==0)*i/59>1+(i/61%1==0)*i/61>1a=i/67%1==0 x=atan x
_+=a*i/67>1+(i/71%1==0)*i/71>1+(i/73%1==0)*i/73>1+(i/79%1==0)*i/79>1 x=atan x
_+=(i/83%1==0)*i/83>1+(i/c%1==0)*i/c>1+(i/b%1==0)*i/b>1:o+=_<1:done++ x=atan x
a=1 if _ then a=2 else a="2" end _/=a
if :o then :output="ok" else :output="failed" end
:done++goto4"#
    ).unwrap()
}

fn set_core_affinity() {
    core_affinity::set_for_current(core_affinity::get_core_ids()
        .unwrap()
        .into_iter()
        .last()
        .unwrap()
    );
}

fn main() {
    const NUM_LINES: usize = 1_000_000;
    set_core_affinity();
    let script = script();
    let mut vm = ir::IRMachine::from_ast(Default::default(), script);
    {
        vm.print_bytecode(std::fs::File::create("unoptimized.yogir").unwrap()).unwrap();
        vm.optimize();
        vm.print_bytecode(std::fs::File::create("optimized.yogir").unwrap()).unwrap();
    }
    loop {
        let start = Instant::now();
        vm.step_repeat(NUM_LINES);
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
        println!("idents as of right now");
        for (ident, val) in vm.idents() {
            println!("{}: {}", ident, val);
        }
    }
}
