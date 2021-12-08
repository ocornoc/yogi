use std::time::*;
use std::io::{stdin, stdout, Read};
use yogi::{arith::Value, parser::{YololParser, Ident}, ir::{IRMachine, CodegenOptions}};
use clap::clap_app;
use anyhow::Result;
use serde::{Serialize, Deserialize};

#[derive(Debug, Serialize, Deserialize)]
enum DataValue {
    Number(String),
    String(String),
}

#[derive(Debug, Serialize, Deserialize)]
struct VarData {
    pub name: String,
    pub global: bool,
    pub value: DataValue,
}

#[derive(Debug, Serialize)]
struct DataResults {
    pub vars: Vec<VarData>,
    pub elapsed_s: f32,
    pub mean_lps: f32,
    pub stddev_lps: f32,
    pub current_line: usize,
    pub elapsed_lines: usize,
}

fn print_results(vm: &IRMachine, elapsed_s: f32, elapsed_lines: usize, samples: Vec<f32>) {
    let mean_lps = elapsed_lines as f32 / elapsed_s;
    let mean_spl = mean_lps.recip();
    let top: f32 = samples
        .iter()
        .map(|&s| (s.recip() - mean_spl).powi(2))
        .sum::<f32>();
    let bot: f32 = (samples.len() - 1) as f32;
    let stddev_lps = (top / bot).sqrt();
    let results = DataResults {
        vars: vm
            .idents()
            .into_iter()
            .map(|(i, v)| VarData {
                name: i.name.clone(),
                global: i.global,
                value: match v {
                    Value::Num(n) => DataValue::Number(n.to_string()),
                    Value::Str(s) => DataValue::String(s.to_string()),
                },
            })
            .collect(),
        elapsed_s,
        mean_lps,
        stddev_lps,
        current_line: vm.get_current_line().map(|i| i + 1).unwrap_or(usize::MAX),
        elapsed_lines,
    };
    serde_json::to_writer(stdout(), &results).unwrap();
}

fn set_core_affinity() {
    core_affinity::set_for_current(core_affinity::get_core_ids()
        .unwrap()
        .into_iter()
        .last()
        .unwrap()
    );
}

fn read_vars() -> Result<Vec<VarData>> {
    let mut bytes = Vec::with_capacity(100);
    for c in stdin().lock().bytes() {
        match c? {
            b'\0' => break,
            c => bytes.push(c),
        }
    }
    serde_json::from_slice(&bytes).map_err(|e| e.into())
}

fn read_program() -> Result<yogi::parser::Program> {
    let mut s = String::with_capacity(1000);
    stdin().read_to_string(&mut s)?;
    YololParser {
        max_line_length: 70,
        ..YololParser::unrestricted()
    }.parse(&s)
}

fn setup_and_bench(
    stop_flag: Ident,
    mut max_lines: usize,
    max_dur: Option<Duration>,
    start_line: usize,
    _terminate_pc_of: bool,
) -> Result<()> {
    let orig_max_lines = max_lines;
    let vars = read_vars()?;
    let program = read_program()?;
    let mut vm = IRMachine::from_ast(CodegenOptions {
        protect_locals: true,
        protect_globals: true,
    }, program);
    vm.set_next_line(start_line);
    let outer_iters = max_lines / 1000;
    let mut samples = Vec::with_capacity(outer_iters.min(1_000_000));

    for vd in vars {
        let ident = Ident {
            name: vd.name,
            global: vd.global,
        };
        vm.set_ident(&ident, match vd.value {
            DataValue::Number(n) => Value::Num(n.parse()?),
            DataValue::String(s) => Value::Str(s.into()),
        });
    }

    set_core_affinity();
    let start = Instant::now();

    'outer: for _ in 0..=outer_iters {
        if let Some(max_dur) = max_dur {
            if start.elapsed() >= max_dur {
                break;
            }
        }

        let inner_start = Instant::now();

        for _ in 0..max_lines.min(1000) {
            vm.step();

            if vm.get_ident_value(&stop_flag).as_bool() {
                break 'outer;
            }
        }

        samples.push(inner_start.elapsed());
        max_lines = max_lines.saturating_sub(1000);
    }

    let elapsed_s = start.elapsed().as_secs_f32();
    let mut iter = samples.into_iter().skip(3);
    iter.next_back();
    print_results(
        &vm,
        elapsed_s,
        orig_max_lines - max_lines,
        iter.map(|d| d.as_secs_f32()).collect(),
    );
    Ok(())
}

fn main() {
    let matches = clap_app!(yogi_ref_harness =>
        (version: "0.2")
        (author: "Grayson Burton <ocornoc@protonmail.com>")
        (@arg STOP_FLAG: -f --("stop-flag") +takes_value "The variable used to detect stopping")
        (@arg MAX_STEPS: -s --("max-steps") +takes_value "The maximum number of steps (lines) before timeout")
        (@arg MAX_SEC: -t --("max-sec") +takes_value "The maximum amount of seconds before timeout")
        (@arg START_PC: --("start-pc") +takes_value "The starting line")
        (@arg TERMINATE_PC_OF: --("term-pc-of") "Terminate on program counter overflow")
    ).get_matches();
    let stop_flag: Ident = matches.value_of("STOP_FLAG").unwrap_or(":done").parse().unwrap();
    let max_lines = matches
        .value_of("MAX_STEPS")
        .map(str::parse)
        .transpose()
        .unwrap()
        .unwrap_or(u32::MAX);
    let max_dur = matches
        .value_of("MAX_SEC")
        .map(str::parse)
        .transpose()
        .unwrap()
        .map(Duration::from_secs_f32);
    let start_line: u32 = matches
        .value_of("START_PC")
        .map(str::parse)
        .transpose()
        .unwrap()
        .unwrap_or(0);
    let terminate_pc_of = matches.is_present("TERMINATE_PC_OF");
    let e = setup_and_bench(
        stop_flag,
        max_lines as usize,
        max_dur,
        start_line as usize,
        terminate_pc_of,
    );
    if let Err(e) = e {
        #[derive(Debug, Serialize)]
        struct Err {
            error: String,
        }

        serde_json::to_writer(stdout(), &Err {
            error: format!("{}", e),
        }).unwrap();
    }
}