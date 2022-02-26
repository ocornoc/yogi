use std::time::Instant;
use yogi::*;
use parser::YololParser;

type VM = yogi::ir::IRMachine;

const SAMPLE_SIZE: usize = 100_000;
const SAMPLES: usize = 100;
const OUTLIERS_REMOVED: usize = 4;
const ELAPSED_LINES: usize = SAMPLE_SIZE * SAMPLES;

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

fn bench_sample(vm: &mut VM) -> f32 {
    let start = Instant::now();
    vm.step_repeat(SAMPLE_SIZE);
    start.elapsed().as_secs_f32()
}

fn main() {
    set_core_affinity();
    let script = script();
    let mut vm = ir::IRMachine::from_ast(Default::default(), script);
    {
        vm.print_bytecode(std::fs::File::create("unoptimized.yogir").unwrap()).unwrap();
        vm.optimize();
        vm.print_bytecode(std::fs::File::create("optimized.yogir").unwrap()).unwrap();
    }
    let mut samples = Vec::with_capacity(SAMPLES + OUTLIERS_REMOVED);
    for _ in 0..SAMPLES + OUTLIERS_REMOVED {
        samples.push(bench_sample(&mut vm));
    }
    // Remove the two largest and two smallest samples from the data set, in case of warmup/etc
    samples.sort_unstable_by(|l, r| l.partial_cmp(r).unwrap());
    for _ in 0..OUTLIERS_REMOVED / 2 {
        samples.pop();
        samples.remove(0);
    }
    // calculating some data
    let elapsed_lines = ELAPSED_LINES;
    let elapsed_s = samples.iter().copied().sum::<f32>();
    let mean_lps = elapsed_lines as f32 / elapsed_s;
    let mean_spl = mean_lps.recip();
    let top: f32 = samples
        .iter()
        .map(|&s| ((SAMPLE_SIZE as f32) / s - mean_lps).powi(2))
        .sum::<f32>();
    let bot: f32 = SAMPLES as f32;
    // standard deviation for lines per second
    let stddev_lps = (top / bot).sqrt();
    println!("Total lines: {elapsed_lines}");
    println!("Total time: {elapsed_s}");
    println!("Mean lines per second: {mean_lps:.1} (\u{03c3} = {stddev_lps:.1} L/s)");
    let mean_nspl = mean_spl * 1_000_000_000.0;
    println!("Mean nanoseconds per line: {mean_nspl:.1}");
}
