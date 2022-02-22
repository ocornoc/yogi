use std::convert::*;
use std::time::Instant;
use yogi::*;
use arith:: Value;
use parser::{YololParser, Ident};

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

fn main() {
    const NUM_LINES: usize = 500_000;
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
