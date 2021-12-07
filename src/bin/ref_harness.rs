use std::convert::*;
use std::time::*;
use std::io::{stdin, stdout, Read};
use yogi::{arith::Value, parser::{YololParser, Ident}, ir::{IRMachine, CodegenOptions}};
use clap::clap_app;
use anyhow::Result;
use serde::Serialize;
use derive_more::From;

#[derive(Debug, Serialize, From)]
enum DataValue {
    Number(i64),
    String(String),
}

#[derive(Debug, Serialize)]
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
}

fn print_results(vm: &IRMachine, elapsed_s: f32, elapsed_l: usize, samples: Vec<f32>) {
    let mean_lps = elapsed_l as f32 / elapsed_s;
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
                    Value::Num(n) => n.0.into(),
                    Value::Str(s) => s.to_string().into(),
                },
            })
            .collect(),
        elapsed_s,
        mean_lps,
        stddev_lps,
        current_line: vm.get_current_line().map(|i| i + 1).unwrap_or(usize::MAX),
    };
    serde_json::to_writer(stdout(), &results).unwrap();
}

fn main() -> Result<()> {
    let matches = clap_app!(yogi_ref_harness =>
        (version: "0.1")
        (author: "Grayson Burton <ocornoc@protonmail.com>")
        (@arg STOP_FLAG: -f --("stop-flag") +takes_value "The variable used to detect stopping")
        (@arg MAX_STEPS: -s --("max-steps") +takes_value "The maximum number of steps (lines) before timeout")
        (@arg MAX_SEC: -t --("max-sec") +takes_value "The maximum amount of seconds before timeout")
    ).get_matches();
    let stop_flag: Ident = matches.value_of("STOP_FLAG").unwrap_or(":done").parse()?;
    let mut max_lines = matches
        .value_of("MAX_STEPS")
        .map(str::parse)
        .transpose()?
        .unwrap_or(usize::MAX);
    let orig_max_lines = max_lines;
    let max_dur = matches
        .value_of("MAX_SEC")
        .map(str::parse)
        .transpose()?
        .map(Duration::from_secs_f32);
    if let Some(max_dur) = max_dur {
        assert!(max_dur.as_secs_f32() > 0.0);
    }

    let mut s = String::with_capacity(1000);
    stdin().read_to_string(&mut s)?;
    let program = YololParser {
        max_line_length: 70,
        ..YololParser::unrestricted()
    }.parse(&s)?;
    let mut vm = IRMachine::from_ast(CodegenOptions {
        protect_locals: true,
        protect_globals: true,
    }, program);
    let outer_iters = max_lines / 1000;
    let mut samples = Vec::with_capacity(outer_iters);
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