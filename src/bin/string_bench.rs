use std::convert::*;
use std::time::Instant;
use yogi::*;
use arith:: Value;
use parser::{YololParser, Ident};

type VM = yogi::ir::IRMachine;

const SAMPLE_SIZE: usize = 10_000;
const SAMPLES: usize = 100;
const OUTLIERS_REMOVED: usize = 4;
const ELAPSED_LINES: usize = SAMPLE_SIZE * SAMPLES;

fn script() -> parser::Program {
    YololParser::unrestricted().parse(
r#"a="_1" b="__1" c="____" d=c+c c+=1 e=d+d d+=1 f=e+e e+=1 g=f+f f+=1 h=g+g
g+=1 i=h+h h+=1 j=i+i i+=1 j+=1
l=1023-:i m=l>511 l%=512 n=l>255 l%=256 o=l>127 l%=128 p=l>63 l%=64 q=l>31
l%=32 r=l>15 l%=16 s=l>7 l%=8 t=l>3 l%=4 u=l>1 l%=2 k=j-m-j+i-n-i+h-o-h+g-p-g+f
k=k-q-f+e-r-e+d-s-d+c-t-c+b-u-b+a-l-a v=k+:s v-=k w=v :o=v-w-- :done++ goto3"#
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
    vm.set_ident(&Ident::global("s"), Value::Str("Hello Cylon".to_string().into()));
    vm.set_ident(&Ident::global("i"), Value::Num(6.into()));
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
        .map(|&s| (s.recip() - mean_spl).powi(2))
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
