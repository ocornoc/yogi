use std::time::*;
use std::io::{stdin, stdout, Read};
use yogi::{arith::Value, parser::{YololParser, Ident}, ir::{IRMachine, CodegenOptions}};
use clap::Parser;
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

fn read_vars() -> Result<Vec<VarData>> {
    let mut bytes = Vec::with_capacity(100);
    for c in stdin().lock().bytes() {
        match c? {
            0 => break,
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
    let vars = read_vars()?;
    let program = read_program()?;
    let mut vm = IRMachine::from_ast(CodegenOptions {
        protect_locals: true,
        protect_globals: true,
    }, program);
    vm.set_next_line(start_line);
    let outer_iters = max_lines / 1000;
    let mut samples = Vec::with_capacity(outer_iters.min(1_000_000));
    let mut elapsed_lines = 0;

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
            elapsed_lines += 1;

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
        elapsed_lines,
        iter.map(|d| d.as_secs_f32()).collect(),
    );
    Ok(())
}

#[derive(Parser)]
/// Yogi harness for Referee/Yolol.IL
#[clap(name = "yogi_ref_harness", version, author)]
struct Cli {
    /// The variable used to detect stopping
    #[clap(default_value = ":done", short = 'f', long)]
    stop_flag: Ident,
    /// The maximum number of steps (lines) before timeout
    #[clap(default_value_t = u32::MAX, short = 's', long)]
    max_steps: u32,
    /// The maximum amount of seconds before timeout
    #[clap(short = 't', long)]
    max_sec: Option<f32>,
    /// The starting line
    #[clap(default_value_t = 0, long)]
    start_pc: u32,
    /// Terminate on program overflow
    #[clap(long = "term-pc-of")]
    terminate_pc_of: bool,
}

fn main() {
    let Cli {
        stop_flag,
        max_steps,
        max_sec,
        start_pc,
        terminate_pc_of,
    } = Cli::parse();
    let max_sec = max_sec.map(Duration::from_secs_f32);
    let e = setup_and_bench(
        stop_flag,
        max_steps as usize,
        max_sec,
        start_pc as usize,
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